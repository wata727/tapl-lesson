%{
open Core
open Support
%}

%token <Support.info> LAMBDA
%token <string Support.withinfo> LCID
%token <Support.info> DOT
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

Command: Term        { fun ctx -> (let t = $1 ctx in Eval(tmInfo t,t)),ctx }
       | LCID Binder { fun ctx -> ((Bind($1.i,$1.v,$2 ctx)), addname ctx $1.v) }

Binder: SLASH { fun ctx -> NameBind }

Term: AppTerm                { $1 }
    | LAMBDA LCID DOT Term   { fun ctx -> let ctx1 = addname ctx $2.v in TmAbs($1,$2.v,$4 ctx1) }
    | LAMBDA USCORE DOT Term { fun ctx -> let ctx1 = addname ctx "_" in TmAbs($1,"_",$4 ctx1) }

AppTerm: ATerm         { $1 }
       | AppTerm ATerm { fun ctx -> let e1 = $1 ctx in
                                    let e2 = $2 ctx in
                                    TmApp(tmInfo e1,e1,e2) }

ATerm: LPAREN Term RPAREN { $2 }
     | LCID               { fun ctx -> TmVar($1.i, name2index $1.i ctx $1.v, ctxlength ctx) }
