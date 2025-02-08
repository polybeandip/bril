open Bril
open Util

type op =
    | Const of Const.t
    | Binary of Op.Binary.t
    | Unary of Op.Unary.t
    | Jmp of Instr.label
    | Br of Instr.label * Instr.label
    | Call 
    | Ret 
    | Print 
    | Nop
    | Phi of Instr.label list
    | Speculate
    | Commit
    | Guard of Instr.label
    | Alloc 
    | Free 
    | Store 
    | Load 
    | PtrAdd 

let op_of_instr (instr : Instr.t) : op = 
    match instr with
    | Const (_, c) -> Const c
    | Binary (_, b, _, _) -> Binary b
    | Unary (_, u, _) -> Unary u
    | Jmp l -> Jmp l
    | Br (_, t, f) -> Br (t, f)
    | Call _ -> Call
    | Ret _ -> Ret
    | Print _ -> Print
    | Nop -> Nop
    | Phi (_, ls) -> Phi (List.map fst ls)
    | Speculate -> Speculate
    | Commit -> Commit
    | Guard (_, l) -> Guard l
    | Alloc _ -> Alloc
    | Free _ -> Free
    | Store _ -> Store
    | Load _ -> Load
    | PtrAdd _ -> PtrAdd
    | Label _ -> failwith "ERROR: [instr_to_op] called on label"
    
type value = op * int list

type row   = value * int * Dest.t
type table = row list
type cloud = (Instr.arg * int) list
type t     = { table : table; cloud : cloud; }

let instr_to_value env instr = 
    let rec instr_to_value_aux ints cloud = function
        | [] -> cloud, List.rev ints
        | h :: t -> 
            match List.assoc_opt h cloud with
            | None -> 
                let fresh = fresh_int () in 
                instr_to_value_aux (fresh :: ints) ((h, fresh) :: cloud) t
            | Some i -> instr_to_value_aux (i :: ints) cloud t
    in

    Instr.args instr
    |> instr_to_value_aux [] env.cloud 
    |> fun (cloud, ints) -> { env with cloud }, (op_of_instr instr, ints)

let find ((op, nums) : value) ((op', nums') : value) (num : int) =
    match nums = nums', op, op' with
    | true, Load , Load
    | true, PtrAdd , PtrAdd -> true
    | true, Phi ls, Phi ls' -> ls = ls'
    | true, Const c, Const c' -> c = c'
    | true, Binary op, Binary op' -> op = op'
    | true, Unary op, Unary op' -> op = op'

    (* [find] crashes on instrs without destinations *)
    | _, Jmp _, _     | _, _, Jmp _
    | _, Br _, _      | _, _, Br _
    | _, Ret, _       | _, _, Ret
    | _, Print, _     | _, _, Print
    | _, Nop, _       | _, _, Nop
    | _, Speculate, _ | _, _, Speculate
    | _, Commit, _    | _, _, Commit
    | _, Guard _, _   | _, _, Guard _
    | _, Free, _      | _, _, Free
    | _, Store, _     | _, _, Store -> 
        failwith "ERROR: [find] called in instr without dest"

    (* [find] ignores instrs with potential side effects *)
    | _, Alloc, Alloc
    | _, Call, Call -> false 

    (* copy propogation *)
    | _, Unary Op.Unary.Id, _ -> nums = [num]

    (* commutativity *)
    | _, Binary Op.Binary.Add, Binary Op.Binary.Add
    | _, Binary Op.Binary.Mul, Binary Op.Binary.Mul
    | _, Binary Op.Binary.Eq,  Binary Op.Binary.Eq
    | _, Binary Op.Binary.And, Binary Op.Binary.And
    | _, Binary Op.Binary.Or,  Binary Op.Binary.Or
    | _, Binary Op.Binary.FAdd,Binary Op.Binary.FAdd
    | _, Binary Op.Binary.FMul,Binary Op.Binary.FMul
    | _, Binary Op.Binary.FEq, Binary Op.Binary.FEq ->
        (List.sort Int.compare nums) = (List.sort Int.compare nums')

    | _ -> false

let search_table key table = 
    let f (v, i, _) = 
        match key with 
        | `Value kv -> find kv v i
        | `Num ki -> ki = i
    in
    List.find_opt f table

(* constant folding *)
let try_fold table value =
    let is_const i = 
        match search_table (`Num i) table with
        | Some ((Const c, _), _, _) -> Some c
        | _ -> None
    in
    let try_binary_fold op i i' = 
        match is_const i, is_const i' with
        | Some c, Some c' -> Const (Op.Binary.fold op c c'), []
        | _ -> value
    in
    let try_unary_fold op i = 
        match is_const i with
        | Some c -> Const (Op.Unary.fold op c), []
        | _ -> value
    in
    match value with
    | Binary op, [i; i'] -> try_binary_fold op i i'
    | Unary op, [i] -> try_unary_fold op i
    | _ -> value

let copy dest src = function
    (* constant propogation *)
    | Const const, _ -> Instr.Const (dest, const)
    (* normal copy *)
    | _ -> Instr.Unary (dest, Op.Unary.Id, fst src)

let reconstruct table (_, nums) instr =
    let canonical i a = 
        match search_table (`Num i) table with
        | Some (_, _, a) -> fst a
        | None -> a
    in
    List.map2 canonical nums (Instr.args instr) |> fun a -> Instr.set_args a instr
