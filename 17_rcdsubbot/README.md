# rcdsubbot

See chapter 17: An ML Implementation of Subtyping

## Build

```console
% make
ocamlc -c support.ml
ocamlc -c core.ml
ocamlyacc -v parser.mly
ocamlc -c parser.mli
ocamlc -c parser.ml
ocamllex lexer.mll
51 states, 2247 transitions, table size 9294 bytes
ocamlc -c lexer.ml
ocamlc -c main.ml
Linking f
ocamlc -o f support.cmo core.cmo parser.cmo lexer.cmo main.cmo
rm lexer.ml
```

## Usage

```console
% ./f "lambda x:Top. x;"
(lambda x:Top. x): Top -> Top
% ./f "(lambda x:Top. x) (lambda x:Top. x);"
(lambda x:Top. x): Top
% ./f "(lambda x:Top->Top. x) (lambda x:Top. x);"
(lambda x:Top. x): Top -> Top
% ./f "(lambda r:{x:Top->Top}. r.x r.x){x=lambda z:Top.z, y=lambda z:Top.z};"
(lambda z:Top. z): Top
% ./f "lambda x:Bot. x;"
(lambda x:Bot. x): Bot -> Bot
% ./f "lambda x:Bot. x x;"
(lambda x:Bot. (x x)): Bot -> Bot
```
