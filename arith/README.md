# arith

See chapter 4: An ML Implementation of Arithmetic Expressions.

## Build

```console
% make
ocamlc -c core.ml
ocamlyacc -v parser.mly
ocamlc -c parser.mli
ocamlc -c parser.ml
ocamllex lexer.mll
36 states, 285 transitions, table size 1356 bytes
ocamlc -c lexer.ml
ocamlc -c main.ml
Linking f
ocamlc -o f core.cmo parser.cmo lexer.cmo main.cmo
rm lexer.ml
```

## Usage

```console
% ./f true
true
% ./f false
false
% ./f 'iszero pred succ 0'
true
% ./f 'iszero succ succ 0'
false
% ./f 'if true then 0 else false'
0
```
