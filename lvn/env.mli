open Bril

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

val op_of_instr : Instr.t -> op

type value = op * int list

type row   = value * int * Dest.t
type table = row list
type cloud = (Instr.arg * int) list
type t     = { table : table; cloud : cloud; }

val instr_to_value : t -> Instr.t -> t * value
val search_table : [< `Num of int | `Value of value ] -> table -> row option
val try_fold : table -> value -> value
val copy : Dest.t -> Dest.t -> value -> Instr.t
val reconstruct : table -> value -> Instr.t -> Instr.t
