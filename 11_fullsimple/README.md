# fullsimple

See chapter 11: Simple Extensions.

## Build

```console
% make
ocamlc -c support.ml
ocamlc -c core.ml
ocamlyacc -v parser.mly
ocamlc -c parser.mli
ocamlc -c parser.ml
ocamllex lexer.mll
118 states, 5720 transitions, table size 23588 bytes
ocamlc -c lexer.ml
ocamlc -c main.ml
Linking f
ocamlc -o f support.cmo core.cmo parser.cmo lexer.cmo main.cmo
rm lexer.ml
```

## Usage

```console
# 11.1 Base Types
% ./f "\"hello\";"
"hello": String
% ./f "timesfloat 2.0 3.14159;"
6.28318: Float
% ./f "iszero(pred(1));"
true: Bool
% ./f "lambda x:Nat. succ x;"
(lambda x:Nat. (succ x)): Nat -> Nat
% ./f "(lambda x:Nat. succ (succ x)) (succ 0);"
3: Nat
% ./f "lambda x:A. x;"
(lambda x:A. x): A -> A
% ./f "T = Nat -> Nat; lambda f:T. lambda x:Nat. f (f x);"
T :: *
(lambda f:T. (lambda x:Nat. (f (f x)))): T -> Nat -> Nat

# 11.2 The Unit Type
% ./f "unit;"
unit: Unit

# 11.3 Derived Forms: Sequencing and Wildcards
% ./f "((lambda _:Bool. unit)true; 2);"
2: Nat

# 11.4 Ascription
% ./f "1 as Nat;"
1: Nat
% ./f "1 as String;"
:1.2: body of as-term does not have the expected type
% ./f "T = Nat -> Nat; (lambda x:Nat. x) as T;"
T :: *
(lambda x:Nat. x): T

# 11.5 Let Bindings
% ./f "let x=true in x;"
true: Bool

# 11.7 Tuples
% ./f "{true, false};"
{true,false}: {Bool,Bool}
% ./f "{true, (lambda x:Nat. x)1};"
{true,1}: {Bool,Nat}
% ./f "{true, (lambda x:Nat. x)1}.1;"
true: Bool
% ./f "(lambda x:{Bool,Bool}. x.1){true, false};"
true: Bool

# 11.8 Records
% ./f "{x=true, y=false};"
{x=true,y=false}: {x:Bool,y:Bool}
% ./f "{x=true, y=false}.x;"
true: Bool
% ./f "(lambda x:{a:Nat, b:Nat}. x.b);"
(lambda x:{a:Nat,b:Nat}. x.b): {a:Nat,b:Nat} -> Nat
% ./f "(lambda x:{a:Nat, b:Nat}. x.b){a=1,b=2};"
2: Nat
% ./f "(lambda x:{a:Nat, b:Nat}. x.b){b=2,a=1};"
:1.1: parameter type mismatch

# 11.10 Variants
% ./f "lambda x:<a:Bool,b:Bool>. x;"
(lambda x:<a:Bool,b:Bool>. x): <a:Bool,b:Bool> -> <a:Bool,b:Bool>
% ./f "Addr = <physical:{firstlast:String,addr:String},virtual:{name:String,email:String}>; (lambda a:Addr. case a of <physical=x> ==> x.firstlast | <virtual=y> ==> y.name);"
Addr :: *
(lambda a:Addr. case a of <physical=x> ==> x.firstlast | <virtual=y> ==> y.name): Addr -> String
% ./f "Addr = <physical:{firstlast:String,addr:String},virtual:{name:String,email:String}>; (lambda a:Addr. case a of <physical=x> ==> x.firstlast | <virtual=y> ==> y.addr);"
Addr :: *
:1.159: label addr not found
% ./f "Addr = <physical:{firstlast:String,addr:String},virtual:{name:String,email:String}>; addr = <physical={firstlast=\"foo\",addr=\"bar\"}> as Addr; getName = (lambda a:Addr. case a of <physical=x> ==> x.firstlast | <virtual=y> ==> y.name); getName addr;"
Addr :: *
addr : Addr
getName : Addr -> String
"foo": String
% ./f "Addr = <physical:{firstlast:String,addr:String},virtual:{name:String,email:String}>; addr = <virtual={name=\"foo\",email=\"bar\"}> as Addr; getName = (lambda a:Addr. case a of <physical=x> ==> x.firstlast | <virtual=y> ==> y.name); getName addr;"
Addr :: *
addr : Addr
getName : Addr -> String
"foo": String
% ./f "OptionalNat = <none:Unit, some:Nat>; Table = Nat -> OptionalNat; (lambda n:OptionalNat. case n of <none=x> ==> 999 | <some=y> ==> y);"
OptionalNat :: *
Table :: *
(lambda n:OptionalNat. case n of <none=x> ==> 999 | <some=y> ==> y): OptionalNat -> Nat
% ./f "OptionalNat = <none:Unit, some:Nat>; Table = Nat -> OptionalNat; (lambda n:OptionalNat. case n of <none=x> ==> 999 | <some=y> ==> y)<some=10> as OptionalNat;"
OptionalNat :: *
Table :: *
10: Nat
% ./f "OptionalNat = <none:Unit, some:Nat>; Table = Nat -> OptionalNat; (lambda n:OptionalNat. case n of <none=x> ==> 999 | <some=y> ==> y)<none=unit> as OptionalNat;"
OptionalNat :: *
Table :: *
999: Nat
% ./f "Weekday = <monday:Unit,tuesday:Unit,wednessday:Unit,thursday:Unit,friday:Unit>; nextBusinessDay = (lambda w:Weekday. case w of <monday=x> ==> <tuesday=unit> as Weekday | <tuesday=x> ==> <wednessday=unit> as Weekday | <wednessday=x> ==> <thursday=unit> as Weekday | <thursday=x> ==> <friday=unit> as Weekday | <friday=x> ==> <monday=unit> as Weekday); nextBusinessDay <monday=unit> as Weekday;"
Weekday :: *
nextBusinessDay : Weekday -> Weekday
<tuesday=unit> as Weekday: Weekday
% ./f "DollarAmount = <dollars:Float>; EuroAmount = <euros:Float>; dollarsToEuros = (lambda d:DollarAmount. case d of <dollars=x> ==> <euros=timesfloat x 1.1325> as EuroAmount); eurosToDollars = (lambda e:EuroAmount. case e of <euros=x> ==> <dollars=timesfloat x 0.883> as DollarAmount); mybankbalance = <dollars=39.50> as DollarAmount; eurosToDollars(dollarsToEuros(mybankbalance));"
DollarAmount :: *
EuroAmount :: *
dollarsToEuros : DollarAmount -> EuroAmount
eurosToDollars : EuroAmount -> DollarAmount
mybankbalance : DollarAmount
<dollars=39.49990125> as DollarAmount: DollarAmount
% ./f "DollarAmount = <dollars:Float>; EuroAmount = <euros:Float>; dollarsToEuros = (lambda d:DollarAmount. case d of <dollars=x> ==> <euros=timesfloat x 1.1325> as EuroAmount); eurosToDollars = (lambda e:EuroAmount. case e of <euros=x> ==> <dollars=timesfloat x 0.883> as DollarAmount); mybankbalance = <dollars=39.50> as DollarAmount; dollarsToEuros(dollarsToEuros(mybankbalance));"
DollarAmount :: *
EuroAmount :: *
dollarsToEuros : DollarAmount -> EuroAmount
eurosToDollars : EuroAmount -> DollarAmount
mybankbalance : DollarAmount
:1.330: parameter type mismatch

# 11.11 General Recursion
% ./f "ff = lambda ie:Nat -> Bool. (lambda x:Nat. if iszero x then true else if iszero(pred x) then false else ie (pred(pred x))); iseven = fix ff; iseven 7;"
ff : (Nat -> Bool) -> Nat -> Bool
iseven : Nat -> Bool
false: Bool
% ./f "ff = lambda ieio:{iseven:Nat -> Bool,isodd:Nat -> Bool}. {iseven=(lambda x:Nat. if iszero x then true else ieio.isodd (pred x)),isodd=(lambda x:Nat. if iszero x then false else ieio.iseven (pred x))}; r = fix ff; iseven = r.iseven; iseven 7;"
ff : {iseven:Nat -> Bool,isodd:Nat -> Bool} -> {iseven:Nat -> Bool,isodd:Nat -> Bool}
r : {iseven:Nat -> Bool,isodd:Nat -> Bool}
iseven : Nat -> Bool
false: Bool
% ./f "diverge = lambda _:Unit. fix (lambda x:T. x); diverge unit;"
diverge : Unit -> T
Fatal error: exception Stack_overflow
% ./f "letrec iseven:Nat -> Bool = lambda x:Nat. if iszero x then true else if iszero (pred x) then false else iseven (pred(pred x)) in iseven 7;"
false: Bool
```
