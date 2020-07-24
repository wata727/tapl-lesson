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
  | "."                    { Parser.DOT(info lexbuf) }
  | "("                    { Parser.LPAREN(info lexbuf) }
  | ")"                    { Parser.RPAREN(info lexbuf) }
  | ";"                    { Parser.SEMI(info lexbuf) }
  | "/"                    { Parser.SLASH(info lexbuf) }
  | "_"                    { Parser.USCORE(info lexbuf) }
  | ['A'-'Z' 'a'-'z' '_']* { Parser.LCID {i=(info lexbuf);v=(text lexbuf)} }
  | eof                    { Parser.EOF(info lexbuf) }
  | _                      { Support.error (info lexbuf) "Illegal character" }
