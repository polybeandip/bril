open Bril

let fresh_int = 
    let counter = ref 0 in
    fun () -> 
        counter := !counter + 1; 
        !counter

let fresh_name = 
    let counter = ref 0 in
    fun name -> 
        counter := !counter + 1; 
        Printf.sprintf "öö_%d_%s" !counter name

type value = Instr.t * int list

type data = {
    table : (value * int * Instr.arg) list;
    cloud : (Instr.arg * int) list;
}

let instr_to_value instr data : value * data = 
    let rec instr_to_value_aux ints cloud = function
        | [] -> cloud, List.rev ints
        | h :: t -> 
            match List.assoc_opt h cloud with
            | None -> 
                let fresh = fresh_int () in 
                instr_to_value_aux (fresh :: ints) ((h, fresh) :: cloud) t
            | Some i -> instr_to_value_aux (i :: ints) cloud t
    in
    let (cloud, ints) = instr_to_value_aux [] data.cloud (Instr.args instr) in
    (instr, ints), { data with cloud }

let equal_value (i, ints) (i', ints') num =
    let open Instr in
    match ints = ints', i, i' with
    | _, Label _, Label _
    | _, Jmp _, Jmp _
    | _, Br _, Br _
    | _, Ret _,  Ret _
    | _, Print _, Print _
    | _, Nop, Nop
    | _, Speculate, Speculate
    | _, Commit, Commit
    | _, Guard _, Guard _
    | _, Free _, Free _
    | _, Store _, Store _ -> 
        failwith "ERROR: [equal_value] called on instr with no dest"
    | true, Load _, Load _
    | true, PtrAdd _, PtrAdd _ -> true
    | true, Phi (_, ls), Phi (_, ls') -> List.map fst ls = List.map fst ls'
    | true, Const (_, c), Const (_, c') -> c = c'
    | true, Binary (_, op, _, _), Binary (_, op', _, _) -> op = op'
    | true, Unary (_, op, _), Unary (_, op', _) -> op = op'
    (* side effects *)
    | _, Alloc _, Alloc _
    | true, Call _, Call _ -> false 
    (* copy propogation *)
    | _, _, Unary (_, Op.Unary.Id, _) -> ints' = [num]
    (* commutativity *)
    | _, Binary (_, Op.Binary.Add, _, _), Binary (_, Op.Binary.Add, _, _)
    | _, Binary (_, Op.Binary.Mul, _, _), Binary (_, Op.Binary.Mul, _, _)
    | _, Binary (_, Op.Binary.Eq, _, _), Binary (_, Op.Binary.Eq, _, _)
    | _, Binary (_, Op.Binary.And, _, _), Binary (_, Op.Binary.And, _, _)
    | _, Binary (_, Op.Binary.Or, _, _), Binary (_, Op.Binary.Or, _, _)
    | _, Binary (_, Op.Binary.FAdd, _, _), Binary (_, Op.Binary.FAdd, _, _)
    | _, Binary (_, Op.Binary.FMul, _, _), Binary (_, Op.Binary.FMul, _, _)
    | _, Binary (_, Op.Binary.FEq, _, _), Binary (_, Op.Binary.FEq, _, _) ->
        (List.sort Int.compare ints) = (List.sort Int.compare ints')
    (* default *)
    | _ -> false

let search_table x table =
    let f (v, i, _) = 
        match x with 
        | `Value v' -> equal_value v v' i
        | `Num i' -> i = i'
    in
    List.find_opt f table

let try_fold value data =
    let is_const i = 
        match search_table (`Num i) data.table with
        | Some ((Instr.Const (_ ,c), _), _, _) -> Some c
        | _ -> None
    in
    match value with
    | Instr.Binary (dest, op, _, _), [i; i'] -> begin
        match is_const i, is_const i' with
        | Some c, Some c' -> Instr.Const(dest, Op.Binary.fold op c c'), []
        | _ -> value
        end
    | Instr.Unary (dest, op, _), [i] -> begin
        match is_const i with
        | Some c -> Instr.Const(dest, Op.Unary.fold op c), []
        | _ -> value
        end
    | _ -> value

let copy dest src (instr, _) = 
    match instr with
    | Instr.Const _ -> Instr.set_dest (Some dest) instr (* constant propogation *)
    | _ -> Instr.Unary (dest, Op.Unary.Id, src)

let reconstruct data instr =
    let f a = 
        match search_table (`Num (List.assoc a data.cloud)) data.table with
        | Some (_, _, a) -> a
        | None -> a
    in
    Instr.args instr |> List.map f |> fun args -> Instr.set_args args instr

let local block = 
    let lastdef =
        List.mapi (fun i x -> (Instr.dest x, i)) block
        |> List.filter (fun (x, _) -> x <> None)
        |> List.map (fun (x, i) -> Option.get x |> fst, i)
        |> List.rev
    in
    let rec local_aux data instrs = function
        | [] -> List.rev instrs
        | (i, _, Instr.Label l) :: t -> local_aux data (Instr.Label l :: instrs) t
        | (i, None, instr) :: t -> 
            let _, data = instr_to_value instr data in
            local_aux data (reconstruct data instr :: instrs) t
        | (i, Some (name, typ), instr) :: t ->
            let value, data = instr_to_value instr data in
            let (instr, nums) as value = try_fold value data in
            match search_table (`Value value) data.table with
            | None ->
                let num = fresh_int () in
                let data' = { data with cloud = (name, num) :: data.cloud } in
                let name = if List.assoc name lastdef = i then name else fresh_name name in
                let data' = { data' with table = (value, num, name) :: data'.table } in
                let instr =
                    instr 
                    |> Instr.set_dest (Some (name, typ))
                    |> reconstruct data
                in
                local_aux data' (instr :: instrs) t
            | Some (value, num, canon) ->
                let data' = { data with cloud = (name, num) :: data.cloud } in
                let instr = copy (name, typ) canon value in
                local_aux data' (instr :: instrs) t
    in
    local_aux { table = []; cloud = [] } [] (List.mapi (fun i x -> i, Instr.dest x, x) block)

let lvn = Func.map_blocks local

let _ = 
    Yojson.Basic.from_channel (In_channel.stdin)
    |> from_json
    |> List.map lvn
    |> to_json
    |> Yojson.Basic.to_channel (Out_channel.stdout)
