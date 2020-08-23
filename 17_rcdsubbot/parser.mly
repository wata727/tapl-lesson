%{
open Core
open Support
%}

%token <Support.info> LAMBDA
%token <Support.info> IF
%token <Support.info> THEN
%token <Support.info> ELSE
%token <Support.info> TRUE
%token <Support.info> FALSE
%token <Support.info> BOOL
%token <Support.info> TTOP
%token <Support.info> TBOT
%token <string Support.withinfo> LCID
%token <int Support.withinfo> INTV
%token <Support.info> ARROW
%token <Support.info> COLON
%token <Support.info> COMMA
%token <Support.info> DOT
%token <Support.info> EQ
%token <Support.info> EOF
%token <Support.info> LCURLY
%token <Support.info> RCURLY
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

Command: Term        { fun ctx -> (let t = $1 ctx in Eval(tmInfo t,t)),ctx }
       | LCID Binder { fun ctx -> ((Bind($1.i,$1.v,$2 ctx)), addname ctx $1.v) }

Binder: COLON Type { fun ctx -> VarBind ($2 ctx) }

PathTerm: PathTerm DOT LCID { fun ctx -> TmProj($2, $1 ctx, $3.v) }
        | PathTerm DOT INTV { fun ctx -> TmProj($2, $1 ctx, string_of_int $3.v) }
        | ATerm             { $1 }

Type: ArrowType { $1 }

AType: LPAREN Type RPAREN       { $2 }
     | BOOL                     { fun ctx -> TyBool }
     | LCURLY FieldTypes RCURLY { fun ctx -> TyRecord($2 ctx 1) }
     | TTOP                     { fun ctx -> TyTop }
     | TBOT                     { fun ctx -> TyBot }

FieldTypes:              { fun ctx i -> [] }
          | NEFieldTypes { $1 }

NEFieldTypes: FieldType                    { fun ctx i -> [$1 ctx i] }
            | FieldType COMMA NEFieldTypes { fun ctx i -> ($1 ctx i) :: ($3 ctx (i+1)) }

FieldType: LCID COLON Type { fun ctx i -> ($1.v, $3 ctx) }
         | Type            { fun ctx i -> (string_of_int i, $1 ctx) }

ArrowType: AType ARROW ArrowType { fun ctx -> TyArr($1 ctx, $3 ctx) }
         | AType                 { $1 }

Term: AppTerm                           { $1 }
    | LAMBDA LCID COLON Type DOT Term   { fun ctx -> let ctx1 = addname ctx $2.v in TmAbs($1,$2.v,$4 ctx, $6 ctx1) }
    | LAMBDA USCORE COLON Type DOT Term { fun ctx -> let ctx1 = addname ctx "_" in TmAbs($1,"_",$4 ctx, $6 ctx1) }
    | IF Term THEN Term ELSE Term       { fun ctx -> TmIf($1, $2 ctx, $4 ctx, $6 ctx) }

AppTerm: PathTerm         { $1 }
       | AppTerm PathTerm { fun ctx -> let e1 = $1 ctx in
                                       let e2 = $2 ctx in
                                       TmApp(tmInfo e1,e1,e2) }

ATerm: LPAREN Term RPAREN   { $2 }
     | LCID                 { fun ctx -> TmVar($1.i, name2index $1.i ctx $1.v, ctxlength ctx) }
     | TRUE                 { fun ctx -> TmTrue($1) }
     | FALSE                { fun ctx -> TmFalse($1) }
     | LCURLY Fields RCURLY { fun ctx -> TmRecord($1, $2 ctx 1) }

Fields:          { fun ctx i -> [] }
      | NEFields { $1 }

NEFields: Field                { fun ctx i -> [$1 ctx i] }
        | Field COMMA NEFields { fun ctx i -> ($1 ctx i) :: ($3 ctx (i+1)) }

Field: LCID EQ Term { fun ctx i -> ($1.v, $3 ctx) }
     | Term         { fun ctx i -> (string_of_int i, $1 ctx) }
