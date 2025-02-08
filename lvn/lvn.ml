open Bril
open Util
open Env

let local block = 
    (* mapping var -> last instr that writes to it *)
    let lastdef =
        List.mapi (fun i x -> (Instr.dest x, i)) block
        |> List.filter (fun (x, _) -> x <> None)
        |> List.map (fun (x, i) -> Option.get x |> fst, i)
        |> List.rev
    in

    (* iterate over instrs *)
    let rec local_aux env instrs = function
        | [] -> List.rev instrs

        (* skip labels *)
        | (i, _, Instr.Label l) :: t -> 
            local_aux env (Instr.Label l :: instrs) t

        (* make instrs with no dest canonical and then skip *)
        | (i, None, instr) :: t -> 
            let env, value = instr_to_value env instr in
            local_aux env (reconstruct env.table value instr :: instrs) t
        
        | (i, Some (name, typ), instr) :: t ->
            let env, value = instr_to_value env instr in
            
            (* constant folding *)
            let (op, nums) as value = try_fold env.table value in 
            let instr =
                match op with
                | Const const -> Instr.Const ((name, typ), const)
                | _ -> instr
            in

            match search_table (`Value value) env.table with

            (* unseen value *)
            | None ->
                let num = fresh_int () in
                let env' = { env with cloud = (name, num) :: env.cloud } in
                (* replace overwritten dests with fresh names *)
                let dest = 
                    if List.assoc name lastdef = i then 
                        (name, typ) 
                    else 
                        (fresh_name name, typ) 
                in
                let env' = { env' with table = (value, num, dest) :: env'.table } in
                let instr = 
                    instr 
                    |> Instr.set_dest (Some dest) 
                    |> reconstruct env.table value
                in
                local_aux env' (instr :: instrs) t

            (* previously seen value *)
            | Some (value, num, canon) ->
                let env = { env with cloud = (name, num) :: env.cloud } in
                let instr = copy (name, typ) canon value in
                local_aux env (instr :: instrs) t
    in
    
    let env = { table = []; cloud = [] } in
    let instrs = List.mapi (fun i x -> i, Instr.dest x, x) block in
    local_aux env [] instrs

let lvn = Func.map_blocks local

let _ = 
    Yojson.Basic.from_channel (In_channel.stdin)
    |> from_json
    |> List.map lvn
    |> to_json
    |> Yojson.Basic.to_channel (Out_channel.stdout)
