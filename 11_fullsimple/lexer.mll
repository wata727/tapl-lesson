{
open Core
open Support

let lineno   = ref 1
and start    = ref 0
and filename = ref ""

let info lexbuf =
    createInfo (!filename) (!lineno) (Lexing.lexeme_start lexbuf - !start)
let text = Lexing.lexeme
}

rule main = parse
    [' ' '\n' '\r' '\t']+            { main lexbuf }
  | "lambda"                         { Parser.LAMBDA(info lexbuf) }
  | "let"                            { Parser.LET(info lexbuf) }
  | "in"                             { Parser.IN(info lexbuf) }
  | "fix"                            { Parser.FIX(info lexbuf) }
  | "letrec"                         { Parser.LETREC(info lexbuf) }
  | "as"                             { Parser.AS(info lexbuf) }
  | "if"                             { Parser.IF(info lexbuf) }
  | "then"                           { Parser.THEN(info lexbuf) }
  | "else"                           { Parser.ELSE(info lexbuf) }
  | "true"                           { Parser.TRUE(info lexbuf) }
  | "false"                          { Parser.FALSE(info lexbuf) }
  | "Bool"                           { Parser.BOOL(info lexbuf) }
  | "case"                           { Parser.CASE(info lexbuf) }
  | "of"                             { Parser.OF(info lexbuf) }
  | "String"                         { Parser.USTRING(info lexbuf) }
  | "Float"                          { Parser.UFLOAT(info lexbuf) }
  | "Unit"                           { Parser.UUNIT(info lexbuf) }
  | "Nat"                            { Parser.NAT(info lexbuf) }
  | "timesfloat"                     { Parser.TIMESFLOAT(info lexbuf) }
  | "succ"                           { Parser.SUCC(info lexbuf) }
  | "pred"                           { Parser.PRED(info lexbuf) }
  | "iszero"                         { Parser.ISZERO(info lexbuf) }
  | "unit"                           { Parser.UNIT(info lexbuf) }
  | "."                              { Parser.DOT(info lexbuf) }
  | "{"                              { Parser.LCURLY(info lexbuf) }
  | "}"                              { Parser.RCURLY(info lexbuf) }
  | "("                              { Parser.LPAREN(info lexbuf) }
  | ")"                              { Parser.RPAREN(info lexbuf) }
  | "<"                              { Parser.LT(info lexbuf) }
  | ">"                              { Parser.GT(info lexbuf) }
  | ";"                              { Parser.SEMI(info lexbuf) }
  | "/"                              { Parser.SLASH(info lexbuf) }
  | "_"                              { Parser.USCORE(info lexbuf) }
  | ":"                              { Parser.COLON(info lexbuf) }
  | ","                              { Parser.COMMA(info lexbuf) }
  | "->"                             { Parser.ARROW(info lexbuf) }
  | "==>"                            { Parser.DDARROW(info lexbuf) }
  | "="                              { Parser.EQ(info lexbuf) }
  | "|"                              { Parser.VBAR(info lexbuf) }
  | ['0'-'9']+                       { Parser.INTV{i=(info lexbuf);v=(int_of_string (text lexbuf))} }
  | ['0'-'9']+ '.' ['0'-'9']+        { Parser.FLOATV{i=(info lexbuf);v=(float_of_string (text lexbuf))} }
  | '"' ['A'-'Z' 'a'-'z' '_']+ '"'   { Parser.STRINGV{i=(info lexbuf);v=(List.nth (String.split_on_char '"' (text lexbuf)) 1)} }
  | ['A'-'Z'] ['A'-'Z' 'a'-'z' '_']* { Parser.UCID {i=(info lexbuf);v=(text lexbuf)} }
  | ['A'-'Z' 'a'-'z' '_']*           { Parser.LCID {i=(info lexbuf);v=(text lexbuf)} }
  | eof                              { Parser.EOF(info lexbuf) }
  | _                                { Support.error (info lexbuf) "Illegal character" }
