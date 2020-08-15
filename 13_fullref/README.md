# fullsimple

See chapter 13: References.

## Build

```console
% make
ocamlc -c support.ml
ocamlc -c core.ml
ocamlyacc -v parser.mly
ocamlc -c parser.mli
ocamlc -c parser.ml
ocamllex lexer.mll
77 states, 3813 transitions, table size 15714 bytes
ocamlc -c lexer.ml
ocamlc -c main.ml
Linking f
ocamlc -o f support.cmo core.cmo parser.cmo lexer.cmo main.cmo
rm lexer.ml
```

## Usage

```console
% ./f 'r = ref 5; !r; r := 7; !r;'
r : Ref Nat
5: Nat
unit: Unit
7: Nat
% ./f 'r = ref 7; (r:=succ(!r); !r);'
r : Ref Nat
8: Nat
% ./f 'r = ref 7; (lambda _:Unit. !r)(r := succ(!r));'
r : Ref Nat
8: Nat
% ./f 'r = ref 7; (r:=succ(!r); r:=succ(!r); r:=succ(!r); r:=succ(!r); !r);'
r : Ref Nat
11: Nat
% ./f 'r = ref 7; s = r; s := 82; !r;'
r : Ref Nat
s : Ref Nat
unit: Unit
82: Nat
% ./f 'c = ref 0; incc = lambda x:Unit. (c := succ (!c); !c); decc = lambda x:Unit. (c := pred (!c); !c); incc unit; decc unit;'
c : Ref Nat
incc : Unit -> Nat
decc : Unit -> Nat
1: Nat
0: Nat
```
