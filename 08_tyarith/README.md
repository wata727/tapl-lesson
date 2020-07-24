# tyarith

See chapter 8: Typed Arithmetic Expressions

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
true : Bool
% ./f false
false : Bool
% ./f 'iszero pred succ 0'
true : Bool
% ./f 'iszero succ succ 0'
false : Bool
% ./f 'if true then 0 else false'
0 : Fatal error: exception Core.TypeError("arms of conditional have different types")
% ./f 'succ 0'
succ 0 : Nat
% ./f 'succ false'
succ false : Fatal error: exception Core.TypeError("argument of succ is not a number")
```
