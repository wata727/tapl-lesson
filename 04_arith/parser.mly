%{
  open Core
%}

%token IF
%token THEN
%token ELSE
%token TRUE
%token FALSE
%token SUCC
%token PRED
%token ISZERO
%token ZERO
%token EOF

%start toplevel
%type <Core.term> toplevel

%%

toplevel: Term  { $1 }

Term: IF Term THEN Term ELSE Term { TmIf($2, $4, $6) }
    | SUCC Term                   { TmSucc($2) }
    | PRED Term                   { TmPred($2) }
    | ISZERO Term                 { TmIsZero($2) }
    | TRUE                        { TmTrue }
    | FALSE                       { TmFalse }
    | ZERO                        { TmZero }
