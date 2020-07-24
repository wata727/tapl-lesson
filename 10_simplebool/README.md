# simplebool

See chapter 10: An ML Implementation of Simple Types.

## Build

```console
% make
ocamlc -c support.ml
ocamlc -c core.ml
ocamlyacc -v parser.mly
ocamlc -c parser.mli
ocamlc -c parser.ml
ocamllex lexer.mll
42 states, 2015 transitions, table size 8312 bytes
ocamlc -c lexer.ml
ocamlc -c main.ml
Linking f
ocamlc -o f support.cmo core.cmo parser.cmo lexer.cmo main.cmo
rm lexer.ml
```

## Usage

```console
% ./f 'lambda x:Bool. x;'
(lambda x:Bool. x): Bool -> Bool
% ./f '(lambda x:Bool. if x then false else true);'
(lambda x:Bool. if x then false else true): Bool -> Bool
% ./f '(lambda x:Bool->Bool. if x false then true else false);'
(lambda x:Bool -> Bool. if (x false) then true else false): (Bool -> Bool) -> Bool
% ./f '(lambda x:Bool->Bool. if x false then true else false) (lambda x:Bool. if x then false else true);'
true: Bool
% ./f '(lambda x:Bool->Bool. if x then false else true);'
:1.22: guard of conditional not a boolean
% ./f '(lambda x:Bool. if x then (lambda x:Bool. x) else true);'
:1.16: arms of conditional have different types
```
