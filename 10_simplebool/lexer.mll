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
    [' ' '\n' '\r' '\t']+  { main lexbuf }
  | "lambda"               { Parser.LAMBDA(info lexbuf) }
  | "if"                   { Parser.IF(info lexbuf) }
  | "then"                 { Parser.THEN(info lexbuf) }
  | "else"                 { Parser.ELSE(info lexbuf) }
  | "true"                 { Parser.TRUE(info lexbuf) }
  | "false"                { Parser.FALSE(info lexbuf) }
  | "Bool"                 { Parser.BOOL(info lexbuf) }
  | "."                    { Parser.DOT(info lexbuf) }
  | "("                    { Parser.LPAREN(info lexbuf) }
  | ")"                    { Parser.RPAREN(info lexbuf) }
  | ";"                    { Parser.SEMI(info lexbuf) }
  | "/"                    { Parser.SLASH(info lexbuf) }
  | "_"                    { Parser.USCORE(info lexbuf) }
  | ":"                    { Parser.COLON(info lexbuf) }
  | "->"                   { Parser.ARROW(info lexbuf) }
  | ['A'-'Z' 'a'-'z' '_']* { Parser.LCID {i=(info lexbuf);v=(text lexbuf)} }
  | eof                    { Parser.EOF(info lexbuf) }
  | _                      { Support.error (info lexbuf) "Illegal character" }
