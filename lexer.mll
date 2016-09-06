
let space = ' ' | '\n' | '\r' | '\t'
let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let ident = alpha (alpha | digit)*
let num   = digit+ | digit+ '.' digit+

rule main = parse
| space+        { main lexbuf }
| "#"           { comment_lex lexbuf; main lexbuf }
| ";"           { Parser.SEMI }
| ","           { Parser.COMM }
| "def"         { Parser.DEF }
| "extern"      { Parser.EXTERN }
(*
| "if"          { Parser.IF }
| "then"        { Parser.THEN }
| "else"        { Parser.ELSE }
*)
| "+"           { Parser.PLUS }
| "-"           { Parser.MINUS }
| "*"           { Parser.TIMES }
| "/"           { Parser.DIV }
| "("           { Parser.LPARENT }
| ")"           { Parser.RPARENT }
| "<"           { Parser.LT }
| ">"           { Parser.GT }
| ident as id   { Parser.IDENT id }
| num as n      { Parser.NUMBER (float_of_string n) }
(* | eof           { Parser.EOF } *) 
| _             { failwith ("Unknown Token: " ^ Lexing.lexeme lexbuf) }

and comment_lex = parse
| "\t" | eof    { () }
| _             { comment_lex lexbuf }
