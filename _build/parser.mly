%{
    open Syntax
%}

%token DEF EXTERN LPARENT RPARENT PLUS MINUS TIMES DIV LT GT IF THEN ELSE EOF
%token <float> NUMBER
%token <string> IDENT

%start top
%type <Syntax.toplevel> top

%%

top:
    | DEF prot expr EOF          { Def (Function ($2, $3)) }
    | EXTERN prot EOF            { Ext $2 }
    | expr EOF                   { Exp $1 } 
    ;

prot:
    | IDENT LPARENT args RPARENT { Prototype ($1, Args $3)}
    ;

args:
    | IDENT args   { $1::$2 }
    | arg         { [$1] }
    ;

arg:
   | IDENT { $1 }


expr:
  | arith_expr LT arith_expr    { Binary ("<", $1, $3) }
  | arith_expr GT arith_expr    { Bianry (">", $3) }
  | IF expr THEN expr ELSE expr { If($2, $4, $6) }
  | IDENT LPARENT arg RPARENT   { Call($1, $3) }
  | arith_expr                  { $1 } 
;

arith_expr:
  | arith_expr PLUS factor_expr   { Binary ("+", $1, $3) }
  | arith_expr MINUS factor_expr  { Binary ("-", $1, $3) }
  | factor_expr                   { $1 }
;

factor_expr:
  | factor_expr TIMES atomic_expr { Binary ("*", $1, $3) }
  | factor_expr DIV atomic_expr   { Binary ("/", $1, $3) }
  | atomic_expr                   { $1 }  
  ;

atomic_expr:
      | NUMBER               { Number $1 }
      | IDENT                { Variable $1 }
      | LPARENT expr RPARENT { $2 }
      ;
