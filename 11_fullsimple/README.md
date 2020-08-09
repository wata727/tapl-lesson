# fullsimple

See chapter 11: Simple Extensions.

## Build

```console
% make
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
T = Nat -> Nat
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
% ./f "1 as String";
:1.2: body of as-term does not have the expected type
% ./f "T = Nat -> Nat; (lambda x:Nat. x) as T;"
T = Nat -> Nat
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
Addr = <physical:{firstlast:String,addr:String},virtual:{name:String,email:String}>
(lambda a:Addr. case a of <physical=x> ==> x.firstlast | <virtual=y> ==> y.name): Addr -> String
% ./f "Addr = <physical:{firstlast:String,addr:String},virtual:{name:String,email:String}>; (lambda a:Addr. case a of <physical=x> ==> x.firstlast | <virtual=y> ==> y.addr);"
Addr = <physical:{firstlast:String,addr:String},virtual:{name:String,email:String}>
:1.159: label addr not found
% ./f "Addr = <physical:{firstlast:String,addr:String},virtual:{name:String,email:String}>; let addr = <physical={firstlast=\"foo\",addr=\"bar\"}> as Addr in (lambda a:Addr. case a of <physical=x> ==> x.firstlast | <virtual=y> ==> y.name)addr;"
Addr = <physical:{firstlast:String,addr:String},virtual:{name:String,email:String}>
"foo": String
% ./f "Addr = <physical:{firstlast:String,addr:String},virtual:{name:String,email:String}>; let addr = <virtual={name=\"foo\",email=\"bar\"}> as Addr in (lambda a:Addr. case a of <physical=x> ==> x.firstlast | <virtual=y> ==> y.name)addr;"
Addr = <physical:{firstlast:String,addr:String},virtual:{name:String,email:String}>
"foo": String
% ./f "OptionalNat = <none:Unit, some:Nat>; Table = Nat -> OptionalNat; (lambda n:OptionalNat. case n of <none=x> ==> 999 | <some=y> ==> y);"
OptionalNat = <none:Unit,some:Nat>
Table = Nat -> OptionalNat
(lambda n:OptionalNat. case n of <none=x> ==> 999 | <some=y> ==> y): OptionalNat -> Nat
% ./f "OptionalNat = <none:Unit, some:Nat>; Table = Nat -> OptionalNat; (lambda n:OptionalNat. case n of <none=x> ==> 999 | <some=y> ==> y)<some=10> as OptionalNat;"
OptionalNat = <none:Unit,some:Nat>
Table = Nat -> OptionalNat
10: Nat
% ./f "OptionalNat = <none:Unit, some:Nat>; Table = Nat -> OptionalNat; (lambda n:OptionalNat. case n of <none=x> ==> 999 | <some=y> ==> y)<none=unit> as OptionalNat;"
OptionalNat = <none:Unit,some:Nat>
Table = Nat -> OptionalNat
999: Nat
% ./f "Weekday = <monday:Unit,tuesday:Unit,wednessday:Unit,thursday:Unit,friday:Unit>; (lambda w:Weekday. case w of <monday=x> ==> <tuesday=unit> as Weekday | <tuesday=x> ==> <wednessday=unit> as Weekday | <wednessday=x> ==> <thursday=unit> as Weekday | <thursday=x> ==> <friday=unit> as Weekday | <friday=x> ==> <monday=unit> as Weekday)<monday=unit> as Weekday;"
Weekday = <monday:Unit,tuesday:Unit,wednessday:Unit,thursday:Unit,friday:Unit>
<tuesday=unit> as Weekday: Weekday
% ./f "DollarAmount = <dollars:Float>; EuroAmount = <euros:Float>; (lambda d:DollarAmount. case d of <dollars=x> ==> <euros=timesfloat x 1.1325> as EuroAmount)<dollars=39.5> as DollarAmount;"
DollarAmount = <dollars:Float>
EuroAmount = <euros:Float>
<euros=44.73375> as EuroAmount: EuroAmount
% ./f "DollarAmount = <dollars:Float>; EuroAmount = <euros:Float>; (lambda d:DollarAmount. case d of <dollars=x> ==> <euros=timesfloat x 1.1325> as EuroAmount)<euros=39.5> as EuroAmount;"
DollarAmount = <dollars:Float>
EuroAmount = <euros:Float>
:1.61: parameter type mismatch

# 11.11 General Recursion

```
