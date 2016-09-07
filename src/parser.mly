%{
    open Ast
%}

%token SEMI COMM DEF EXTERN LPARENT RPARENT PLUS MINUS TIMES DIV LT GT (* IF THEN ELSE EOF *)
%token <float> NUMBER
%token <string> IDENT

%start top
%type <Ast.toplevel> top

%%

top:
    | DEF prot expr SEMI          { Def (Function ($2, $3)) }
    | EXTERN prot SEMI            { Ext $2 }
    | expr SEMI                   { Exp (Function (Prototype ("00", [||]), $1)) } 
    (* | EOF                         {} *)
    ;

prot:
    | IDENT LPARENT proto_args RPARENT { Prototype ($1, Array.of_list $3)}
    | IDENT LPARENT RPARENT            { Prototype ($1, [||]) }
    ;

proto_args:
    | IDENT proto_args   { $1::$2 }
    | proto_arg         { [$1] }
    ;

proto_arg:
   | IDENT { $1 }

expr:
  | arith_expr LT arith_expr    { Binary_l ("<", $1, $3) }
  | arith_expr GT arith_expr    { Binary_l (">", $1, $3) }
  (* | IF expr THEN expr ELSE expr { If($2, $4, $6) } *)
  | arith_expr                  { $1 } 
;

arith_expr:
  | arith_expr PLUS factor_expr   { Binary_c ("+", $1, $3) }
  | arith_expr MINUS factor_expr  { Binary_c ("-", $1, $3) }
  | factor_expr                   { $1 }
;

factor_expr:
  | factor_expr TIMES atomic_expr { Binary_c ("*", $1, $3) }
  | factor_expr DIV atomic_expr   { Binary_c ("/", $1, $3) }
  | atomic_expr                   { $1 }  
  | IDENT LPARENT expr_args RPARENT   { Call($1, Array.of_list $3) }
  | IDENT LPARENT RPARENT       { Call($1, [||]) }
  ;

atomic_expr:
  | NUMBER               { Number $1 }
  | IDENT                { Variable $1 }
  | LPARENT expr RPARENT { $2 }
  ;

expr_args:
    | expr COMM expr_args   { $1::$3 }
    | expr_arg         { [$1] }
    ;

expr_arg:
   | expr { $1 }