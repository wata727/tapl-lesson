# untyped

See chapter 7: An ML Implementation of the Lambda-Calculus.

## Build

```console
% make
ocamlc -c support.ml
ocamlc -c core.ml
ocamlyacc -v parser.mly
ocamlc -c parser.mli
ocamlc -c parser.ml
ocamllex lexer.mll
17 states, 739 transitions, table size 3058 bytes
ocamlc -c lexer.ml
ocamlc -c main.ml
Linking f
ocamlc -o f support.cmo core.cmo parser.cmo lexer.cmo main.cmo
rm lexer.ml
```

## Usage

```console
% ./f 'x/; x;'
x
x
% ./f 'x/; lambda x. x;'
x
(lambda x'. x')
% ./f 'x/; (lambda x. x) (lambda x. x x);'
x
(lambda x'. (x' x'))
% ./f 'x/; y/; (lambda x. x) ((lambda x. x) (lambda y. (lambda x. x) y));'
x
y
(lambda y'. ((lambda x'. x') y'))
```
