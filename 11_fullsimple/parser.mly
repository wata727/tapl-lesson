%{
open Core
open Support
%}

%token <Support.info> LAMBDA
%token <Support.info> LET
%token <Support.info> IN
%token <Support.info> AS
%token <Support.info> IF
%token <Support.info> THEN
%token <Support.info> ELSE
%token <Support.info> TRUE
%token <Support.info> FALSE
%token <Support.info> BOOL
%token <Support.info> USTRING
%token <Support.info> UFLOAT
%token <Support.info> TIMESFLOAT
%token <Support.info> UUNIT
%token <Support.info> UNIT
%token <Support.info> SUCC
%token <Support.info> PRED
%token <Support.info> ISZERO
%token <Support.info> NAT
%token <string Support.withinfo> UCID
%token <string Support.withinfo> LCID
%token <int Support.withinfo> INTV
%token <float Support.withinfo> FLOATV
%token <string Support.withinfo> STRINGV
%token <Support.info> ARROW
%token <Support.info> COLON
%token <Support.info> DOT
%token <Support.info> EQ
%token <Support.info> EOF
%token <Support.info> LPAREN
%token <Support.info> RPAREN
%token <Support.info> SEMI
%token <Support.info> SLASH
%token <Support.info> USCORE

%start toplevel
%type < Core.context -> (Core.command list * Core.context) > toplevel

%%

toplevel: EOF                   { fun ctx -> [],ctx }
        | Command SEMI toplevel { fun ctx -> let cmd,ctx = $1 ctx in
                                             let cmds,ctx = $3 ctx in
                                             cmd::cmds,ctx }

Command: Term          { fun ctx -> (let t = $1 ctx in Eval(tmInfo t,t)),ctx }
       | LCID Binder   { fun ctx -> ((Bind($1.i,$1.v,$2 ctx)), addname ctx $1.v) }
       | UCID TyBinder { fun ctx -> ((Bind($1.i,$1.v,$2 ctx)), addname ctx $1.v) }

Binder: COLON Type { fun ctx -> VarBind ($2 ctx) }

Type: ArrowType { $1 }

AType: LPAREN Type RPAREN { $2 }
     | UCID               { fun ctx -> if isnamebound ctx $1.v then TyVar(name2index $1.i ctx $1.v, ctxlength ctx)
                                       else TyId($1.v) }
     | BOOL               { fun ctx -> TyBool }
     | USTRING            { fun ctx -> TyString }
     | UFLOAT             { fun ctx -> TyFloat }
     | UUNIT              { fun ctx -> TyUnit }
     | NAT                { fun ctx -> TyNat }

ArrowType: AType ARROW ArrowType { fun ctx -> TyArr($1 ctx, $3 ctx) }
         | AType                 { $1 }

TyBinder:         { fun ctx -> TyVarBind }
        | EQ Type { fun ctx -> TyAbbBind($2 ctx) }

Term: AppTerm                           { $1 }
    | LAMBDA LCID COLON Type DOT Term   { fun ctx -> let ctx1 = addname ctx $2.v in TmAbs($1,$2.v,$4 ctx, $6 ctx1) }
    | LAMBDA USCORE COLON Type DOT Term { fun ctx -> let ctx1 = addname ctx "_" in TmAbs($1,"_",$4 ctx, $6 ctx1) }
    | IF Term THEN Term ELSE Term       { fun ctx -> TmIf($1, $2 ctx, $4 ctx, $6 ctx) }
    | LET LCID EQ Term IN Term          { fun ctx -> TmLet($1, $2.v, $4 ctx, $6 (addname ctx $2.v)) }
    | LET USCORE EQ Term IN Term        { fun ctx -> TmLet($1, "_", $4 ctx, $6 (addname ctx "_")) }

AppTerm: AscribeTerm                        { $1 }
       | AppTerm AscribeTerm                { fun ctx -> let e1 = $1 ctx in
                                                         let e2 = $2 ctx in
                                                         TmApp(tmInfo e1,e1,e2) }
       | TIMESFLOAT AscribeTerm AscribeTerm { fun ctx -> TmTimesfloat($1,$2 ctx,$3 ctx) }
       | SUCC AscribeTerm                   { fun ctx -> TmSucc($1,$2 ctx) }
       | PRED AscribeTerm                   { fun ctx -> TmPred($1,$2 ctx) }
       | ISZERO AscribeTerm                 { fun ctx -> TmIsZero($1,$2 ctx) }

AscribeTerm: ATerm AS Type { fun ctx -> TmAscribe($2, $1 ctx, $3 ctx) }
           | ATerm         { $1 }

TermSeq: Term              { $1 }
       | Term SEMI TermSeq { fun ctx -> TmApp($2, TmAbs($2, "_", TyUnit, $3 (addname ctx "_")), $1 ctx) }

ATerm: LPAREN TermSeq RPAREN { $2 }
     | LCID                  { fun ctx -> TmVar($1.i, name2index $1.i ctx $1.v, ctxlength ctx) }
     | TRUE                  { fun ctx -> TmTrue($1) }
     | FALSE                 { fun ctx -> TmFalse($1) }
     | STRINGV               { fun ctx -> TmString($1.i,$1.v) }
     | UNIT                  { fun ctx -> TmUnit($1) }
     | FLOATV                { fun ctx -> TmFloat($1.i,$1.v) }
     | INTV                  { fun ctx -> let rec f n = match n with
                                              0 -> TmZero($1.i)
                                            | n -> TmSucc($1.i,f (n-1))
                                          in f $1.v }
