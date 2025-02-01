open Bril

type block = Instr.t list

type data = {
    blocks : block list;
    map : (string * block) list;
    name : string;
}

type cfg = {
    graph : (block * block list) list;
    name : string;
}

let sprintf = Printf.sprintf

(* to get around bugs in [Instr.to_string] *)
let instr_to_string instr = 
    match instr with
    | Instr.Label _ -> Instr.to_string instr ^ ":"
    | Instr.Call  _ -> "call @" ^ Instr.to_string instr ^ ";"
    | _ -> Instr.to_string instr ^ ";"

let block_to_string block = 
    let rec helper str = function
        | h :: [] -> str ^ (instr_to_string h)
        | h :: t  -> helper (str ^ sprintf "%s\\l" (instr_to_string h)) t
        | [] -> str
    in
    helper "" block

let cfg_to_string { graph; name } =
    let rec nodes str = function
        | (block, blocks) :: t -> 
            let node = block_to_string block in
            nodes (str ^ sprintf "\"%s\" [shape=box];\n" node) t
        | [] -> str
    in
    let rec edges str = function
        | (block, blocks) :: t -> 
            let f str b = 
                let pred, succ = 
                    block_to_string block, block_to_string b
                in
                str ^ (sprintf "\"%s\" -> \"%s\";\n" pred succ)
            in
            edges (List.fold_left f str blocks) t
        | [] -> str
    in
    sprintf "digraph %s {\n%s%s}" name (nodes "" graph) (edges "" graph) 

let parse () =
  In_channel.(stdin |> input_all)
  |> Yojson.Basic.from_string 
  |> Bril.from_json

let form_blocks (func : Func.t) =
    let ( %% ) h t = 
        if h = [] then t else (List.rev h) :: t 
    in
    let ( $$ ) (x, y) t = 
        match x with | None -> t | Some x -> (x, List.rev y) :: t 
    in

    let rec form_blocks_aux blocks block map label = function
        | [] -> 
            { 
                blocks = List.rev (block %% blocks); 
                map = (label, block) $$ map; 
                name = func.name 
            }
        | h :: t -> begin
            match h with
            | Instr.Label l as lbl -> 
                form_blocks_aux (block %% blocks) [lbl] ((label, block) $$ map) (Some l) t
            | Instr.Jmp _ | Instr.Br _ | Instr.Ret _ -> 
                form_blocks_aux ((h :: block) %% blocks) [] ((label, (h :: block)) $$ map) None t
            | _ -> 
                form_blocks_aux blocks (h :: block) map label t
            end
    in
    form_blocks_aux [] [] [] None (Func.instrs func)

let cfg { blocks; map; name } = 
    let cfg_aux (i, graph) block = 
        match List.rev block with
        | Instr.Jmp l :: _ -> 
            (i + 1, (block, [List.assoc l map]) :: graph)
        | Instr.Br (_, lt, lf) :: _-> 
            (i + 1, (block, List.map (fun l -> List.assoc l map) [lt; lf]) :: graph)
        | Instr.Ret _ :: _ ->
            (i + 1, (block, []) :: graph)
        | _ :: t -> begin
            match List.nth_opt blocks (i + 1) with
            | None   -> (i + 1, (block, [])  :: graph)
            | Some b -> (i + 1, (block, [b]) :: graph)
            end
        | [] -> failwith "ERROR: empty block"
    in
    match List.fold_left cfg_aux (0, []) blocks with (_, graph) -> { graph; name }

let _ = 
    match Array.length Sys.argv with
    | 2 -> parse ()
        |> List.find (fun (f : Func.t) -> f.name = Sys.argv.(1))
        |> form_blocks
        |> cfg
        |> cfg_to_string
        |> print_endline
    | _ -> print_endline (sprintf "Usage: %s <function>" Sys.argv.(0)); exit 1
