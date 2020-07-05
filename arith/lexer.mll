rule main = parse
    [' ' '\n' '\r' '\t']+ { main lexbuf }
  | "if"                  { Parser.IF }
  | "then"                { Parser.THEN }
  | "else"                { Parser.ELSE }
  | "true"                { Parser.TRUE }
  | "false"               { Parser.FALSE }
  | "succ"                { Parser.SUCC }
  | "pred"                { Parser.PRED }
  | "iszero"              { Parser.ISZERO }
  | '0'                   { Parser.ZERO }
  | eof                   { Parser.EOF }
  | _                     { failwith "Unknown token found" }
