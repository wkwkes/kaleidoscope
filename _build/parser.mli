type token =
  | DEF
  | EXTERN
  | LPARENT
  | RPARENT
  | PLUS
  | MINUS
  | TIMES
  | DIV
  | LT
  | GT
  | IF
  | THEN
  | ELSE
  | EOF
  | NUMBER of (float)
  | IDENT of (string)

val top :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Syntax.toplevel
