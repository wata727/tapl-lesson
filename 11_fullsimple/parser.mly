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
%token <Support.info> CASE
%token <Support.info> OF
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
%token <Support.info> DDARROW
%token <Support.info> COLON
%token <Support.info> COMMA
%token <Support.info> DOT
%token <Support.info> EQ
%token <Support.info> EOF
%token <Support.info> LCURLY
%token <Support.info> RCURLY
%token <Support.info> LPAREN
%token <Support.info> RPAREN
%token <Support.info> LT
%token <Support.info> GT
%token <Support.info> SEMI
%token <Support.info> SLASH
%token <Support.info> USCORE
%token <Support.info> VBAR

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
      | EQ Term    { fun ctx -> TmAbbBind($2 ctx, None) }

Type: ArrowType { $1 }

AType: LPAREN Type RPAREN       { $2 }
     | UCID                     { fun ctx -> if isnamebound ctx $1.v then TyVar(name2index $1.i ctx $1.v, ctxlength ctx)
                                             else TyId($1.v) }
     | BOOL                     { fun ctx -> TyBool }
     | USTRING                  { fun ctx -> TyString }
     | UFLOAT                   { fun ctx -> TyFloat }
     | UUNIT                    { fun ctx -> TyUnit }
     | LCURLY FieldTypes RCURLY { fun ctx -> TyRecord($2 ctx 1) }
     | LT FieldTypes GT         { fun ctx -> TyVariant($2 ctx 1) }
     | NAT                      { fun ctx -> TyNat }

ArrowType: AType ARROW ArrowType { fun ctx -> TyArr($1 ctx, $3 ctx) }
         | AType                 { $1 }

TyBinder:         { fun ctx -> TyVarBind }
        | EQ Type { fun ctx -> TyAbbBind($2 ctx) }

FieldTypes:              { fun ctx i -> [] }
          | NEFieldTypes { $1 }

NEFieldTypes: FieldType                    { fun ctx i -> [$1 ctx i] }
            | FieldType COMMA NEFieldTypes { fun ctx i -> ($1 ctx i) :: ($3 ctx (i+1)) }

FieldType: Type            { fun ctx i -> (string_of_int i, $1 ctx) }
         | LCID COLON Type { fun ctx i -> ($1.v, $3 ctx) }

Term: AppTerm                           { $1 }
    | LAMBDA LCID COLON Type DOT Term   { fun ctx -> let ctx1 = addname ctx $2.v in TmAbs($1,$2.v,$4 ctx, $6 ctx1) }
    | LAMBDA USCORE COLON Type DOT Term { fun ctx -> let ctx1 = addname ctx "_" in TmAbs($1,"_",$4 ctx, $6 ctx1) }
    | IF Term THEN Term ELSE Term       { fun ctx -> TmIf($1, $2 ctx, $4 ctx, $6 ctx) }
    | LET LCID EQ Term IN Term          { fun ctx -> TmLet($1, $2.v, $4 ctx, $6 (addname ctx $2.v)) }
    | LET USCORE EQ Term IN Term        { fun ctx -> TmLet($1, "_", $4 ctx, $6 (addname ctx "_")) }
    | CASE Term OF Cases                { fun ctx -> TmCase($1, $2 ctx, $4 ctx) }

AppTerm: PathTerm                     { $1 }
       | AppTerm PathTerm             { fun ctx -> let e1 = $1 ctx in
                                                   let e2 = $2 ctx in
                                                   TmApp(tmInfo e1,e1,e2) }
       | TIMESFLOAT PathTerm PathTerm { fun ctx -> TmTimesfloat($1,$2 ctx,$3 ctx) }
       | SUCC PathTerm                { fun ctx -> TmSucc($1,$2 ctx) }
       | PRED PathTerm                { fun ctx -> TmPred($1,$2 ctx) }
       | ISZERO PathTerm              { fun ctx -> TmIsZero($1,$2 ctx) }

AscribeTerm: ATerm AS Type { fun ctx -> TmAscribe($2, $1 ctx, $3 ctx) }
           | ATerm         { $1 }

PathTerm: PathTerm DOT INTV { fun ctx -> TmProj($2, $1 ctx, string_of_int $3.v) }
        | PathTerm DOT LCID { fun ctx -> TmProj($2, $1 ctx, $3.v) }
        | AscribeTerm       { $1 }

TermSeq: Term              { $1 }
       | Term SEMI TermSeq { fun ctx -> TmApp($2, TmAbs($2, "_", TyUnit, $3 (addname ctx "_")), $1 ctx) }

ATerm: LPAREN TermSeq RPAREN      { $2 }
     | LCID                       { fun ctx -> TmVar($1.i, name2index $1.i ctx $1.v, ctxlength ctx) }
     | TRUE                       { fun ctx -> TmTrue($1) }
     | FALSE                      { fun ctx -> TmFalse($1) }
     | STRINGV                    { fun ctx -> TmString($1.i,$1.v) }
     | UNIT                       { fun ctx -> TmUnit($1) }
     | LCURLY Fields RCURLY       { fun ctx -> TmRecord($1, $2 ctx 1) }
     | FLOATV                     { fun ctx -> TmFloat($1.i,$1.v) }
     | INTV                       { fun ctx -> let rec f n = match n with
                                                   0 -> TmZero($1.i)
                                                 | n -> TmSucc($1.i,f (n-1))
                                               in f $1.v }
     | LT LCID EQ Term GT AS Type { fun ctx -> TmTag($1, $2.v, $4 ctx, $7 ctx) }

Cases: Case            { fun ctx -> [$1 ctx] }
     | Case VBAR Cases { fun ctx -> ($1 ctx) :: ($3 ctx) }

Case: LT LCID EQ LCID GT DDARROW AppTerm { fun ctx -> let ctx1 = addname ctx $4.v
                                                      in ($2.v, ($4.v, $7 ctx1)) }

Fields:          { fun ctx i -> [] }
      | NEFields { $1 }

NEFields: Field                { fun ctx i -> [$1 ctx i] }
        | Field COMMA NEFields { fun ctx i -> ($1 ctx i) :: ($3 ctx (i+1)) }

Field: Term         { fun ctx i -> (string_of_int i, $1 ctx) }
     | LCID EQ Term { fun ctx i -> ($1.v, $3 ctx) }
