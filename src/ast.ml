open Llvm
    
(* 
type args = Args of string list
*)

type expr =
    | Number of float 
    | Variable of string 
    | Binary_c of string * expr * expr 
    | Binary_l of string * expr * expr 
    | Call of string * expr array
    (* | If of expr * expr * expr *)
[@@deriving show]


type proto = Prototype of string * string array
[@@deriving show]


type func = Function of proto * expr
[@@deriving show]

type toplevel = 
    Exp of func | Def of func | Ext of proto
[@@deriving show]
