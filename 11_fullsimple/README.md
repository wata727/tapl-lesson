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

# 11.10 Variants

# 11.11 General Recursion

```
