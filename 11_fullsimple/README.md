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

# 11.4 Ascription

# 11.5 Let Bindings

# 11.7 Tuples

# 11.8 Records

# 11.10 Variants

# 11.11 General Recursion

```
