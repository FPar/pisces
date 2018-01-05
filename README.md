# pisces <>< [![Build Status](https://travis-ci.org/FPar/pisces.svg?branch=master)](https://travis-ci.org/FPar/pisces)

## What is pisces?

I wanted to learn how to write a compiler in Haskell using parser combinators.
So pisces is a very basic programming language with the syntax inspired a little
bit by the Rust programming languge. The compiler provided in this repository
targets LLVM and emits LLVM assembly language.

A simple pisces program looks like this (extern functions calls are not yet
implemented, so no "Hello World" 😞):

```rust
fn add(a: i64, b: i64) -> i64
{
  var c: i64 = a + b;
  return c;
}

fn main () -> i64
{
  return add(4, 5);
}
```

## Usage

In order to build it you need LLVM 5.0. From here it's just `stack build`.

Compile and run a program:

```
stack exec pscsc main.pscs
llvm-as a.ll
lli a.bc
```

To build a native executable:

```
...
llc -filetype=obj a.bc
clang a.o
./a.out
```

## Documentation

* [Pisces language reference](/doc/language.md)
* [API](https://fpar.github.io/pisces/)
