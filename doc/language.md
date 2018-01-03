# Pisces language reference

Pisces is a very simple language without a real type system. It also doesn't
support many control structures, which limits its use and also doesn't support
calling external functions. Therefore its use is somewhat limited. However, the
Rust inspired syntax should be straight forward and easy to learn.

## Variables and expressions

Variables are declared with the keyword `var` followed by a name, type and
definition.

```rust
var x: i64 = 12;
```

Currently, only the `i64` type is supported. It is possible to do most of the
normal math expressions (`+`, `-`, `*`, `/`, `%`) and use parentheses to change
associativity.

```rust
var x: i64 = a * (b + 4);
```

## Functions

Functions are defined with a name, a few parameters, an optional return type and
a function body. The `return` keyword is used to return a value from the
function.

```rust
fn add (a: i64, b: i64) -> i64
{
	return a + b;
}
```

As there are no global variables, functions will usually return a value. The
compiler will look for a function called `main` as entry point.

## if statments

If statments are only possible at the end of a function block (a result of
LLVM's SSA form that requires merging variables with a phi node, which is not
yet possible).

```rust
fn max (a: i64, b: i64) -> i64
{
	if (a > b)
	{
		return a;
	}
	else
	{
		return b;
	}
}

## Loops

Currently, there is no real language construct for creating loops. Instead,
recursion can be used.

```rust
fn squaresumStep (a: i64, sum: i64) -> i64
{
	if (a > 0)
	{
		return squaresumStep (a / 10, sum + a % 10);
	}
	else
	{
		return sum;
	}
}

fn squaresum (a: i64) -> i64
{
	return squaresumStep (a, 0);
}
```
