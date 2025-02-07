open Bril

module ArgSet = Set.Make (struct
    type t = Instr.arg
    let compare = Instr.compare_arg
end)

let converge step curr =
    let rec converge_aux step curr = function
        | None -> converge_aux step (step curr) (Some curr)
        | Some prev -> 
            if prev = curr then curr else converge_aux step (step curr) (Some curr)
    in
    converge_aux step curr None

let global instrs = 
    let args = 
        let rec collect_args used = function
            | [] -> used
            | Instr.Label _ :: t -> collect_args used t
            | h :: t -> 
                let args = h |> Instr.args |> ArgSet.of_list in
                collect_args (ArgSet.union used args) t
        in
        collect_args ArgSet.empty (instrs)
    in
    let rec global_aux kept = function
        | [] -> kept |> List.rev
        | h :: t ->
            match Instr.dest h with
            | None -> global_aux (h :: kept) t
            | Some (arg, _) ->
                match ArgSet.find_opt arg args with
                | None -> global_aux kept t
                | Some _ -> global_aux (h :: kept) t
    in
    global_aux [] instrs

let local block = 
    let delete = 
        let rec mark_delete unused lastdef delete i = function
            | [] -> delete
            | Instr.Label _ :: t -> mark_delete unused lastdef delete (i + 1) t
            | h :: t ->
                let args = h |> Instr.args |> ArgSet.of_list in
                let unused = ArgSet.diff unused args in
                match Instr.dest h with
                | None -> mark_delete unused lastdef delete (i + 1) t
                | Some (arg, _) -> 
                    let unused' = ArgSet.add arg unused in
                    let lastdef' = (arg, i) :: lastdef in
                    let delete' = 
                        match List.assoc_opt arg lastdef with
                        | None -> delete
                        | Some j -> j :: delete
                    in
                    match ArgSet.find_opt arg unused with
                    | None -> mark_delete unused' lastdef' delete (i + 1) t
                    | Some _ -> mark_delete unused' lastdef' delete' (i + 1) t
        in
        mark_delete ArgSet.empty [] [] 0 block
    in
    List.filteri (fun i _ -> not (List.exists (fun j -> i = j) delete)) block

let tdce func = 
    Func.instrs func
    |> converge global
    |> Func.set_instrs func
    |> Func.map_blocks (converge local)

let _ = 
    Yojson.Basic.from_channel (In_channel.stdin)
    |> from_json
    |> List.map tdce
    |> to_json
    |> Yojson.Basic.to_channel (Out_channel.stdout)
