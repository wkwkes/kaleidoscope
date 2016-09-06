
(* The type of tokens. *)

type token = 
  | TIMES
  | SEMI
  | RPARENT
  | PLUS
  | NUMBER of (float)
  | MINUS
  | LT
  | LPARENT
  | IDENT of (string)
  | GT
  | EXTERN
  | DIV
  | DEF
  | COMM

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val top: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.toplevel)
