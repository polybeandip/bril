open Bril

let safe_args j = try Instr.args j with Failure _ -> []

let reorder func = 
    let fixed i = 
        let open Instr in
        match i with
        | Label _ 
        | Jmp _ 
        | Br _ 
        | Ret _ 
        | Print _ 
        | Speculate 
        | Guard _ 
        | Call _ -> true
        | _ -> false
    in
    let same_dest i j = 
        match Instr.dest i, Instr.dest j with
        | Some x, None | None, Some x -> false
        | None, None -> false
        | Some x, Some y -> x = y
    in
    let data_flow i j =
        match Instr.dest i with
        | None -> false
        | Some (i, _) -> List.exists (fun x -> x = i) (safe_args j)
    in
    let no_swap i j =
        fixed i       || 
        fixed j       || 
        same_dest i j || 
        data_flow i j || 
        data_flow j i
    in

    let rec reorder_aux instrs = function
        | i :: j :: t when no_swap i j -> reorder_aux (i :: instrs) (j :: t)
        | i :: j :: t -> reorder_aux (i :: j :: instrs) t
        | i :: [] -> List.rev (i :: instrs)
        | _ -> List.rev instrs
    in
    reorder_aux [] (Func.instrs func) |> Func.set_instrs func 

let _ = 
    In_channel.(stdin |> input_all)
    |> Yojson.Basic.from_string 
    |> Bril.from_json
    |> List.map reorder
    |> Bril.to_json
    |> Yojson.Basic.to_string
    |> print_endline
