open Llvm
(*type token = Def | Extern | Ident of string | Number of flaot | Kwd of char*)

let hoge = create_context ()

type args = Args of string list

type expr =
    Number of float | Variable of string | Binary of string * expr * expr | 
    Call of string * expr | If of expr * expr * expr

type proto = Prototype of string * args

type func = Function of proto * expr

type toplevel = 
    Exp of expr | Def of func | Ext of proto

let rec print_arg = function
    | Args [] -> ()
    | Args (x::xs) -> print_string x; print_arg (Args xs)

let rec print_expr = function
    Number f -> print_string (string_of_float f)
  | Variable s -> print_string s
  | Binary (op, a, b) -> (print_string ("(" ^ op ^ ", "); print_expr a;
                        print_string ", "; print_expr b; print_string ") ")
  | Call (s, e) -> (print_string ("(" ^ s ^ ", "); print_expr e; print_string ")")
  | If (e1, e2, e3) -> (print_string "(if, "; print_expr e1; print_string ", ";
                        print_expr e2; print_string ", "; print_expr e3;
                        print_string ") ")
let print_proto = function
    Prototype (p, e) -> (print_string ("(Prototype, "^p^", "); print_arg e;
                        print_string ")")


let print_func = function
    Function (p, e) -> (print_string ("(Function , "); print_proto p;
                        print_string ", "; print_expr e; print_string ")")

let rec print_toplevel = function
  | Exp e -> print_expr e
  | Def p -> print_func p
  | Ext e -> print_proto e

