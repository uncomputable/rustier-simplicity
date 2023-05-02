# Simple Playground

"If you know Rust, you already know Simplicity."

Simplicity is not hard and this repo tries to show that by implementing it using the Rust type system.
No pointers are used; everything lives on the stack.
Everything is as close to the [tech report](https://github.com/ElementsProject/simplicity/blob/pdf/Simplicity-TR.pdf) as possible.
Feel free to read the code and the report side by side.

## Values

Learn about types and values in [src/value.rs](https://github.com/uncomputable/simple-playground/blob/master/src/value.rs).
There is an example in [examples/values.rs](https://github.com/uncomputable/simple-playground/blob/master/examples/values.rs).
Values serve as data.
There are unit, sum and product types.
Bits and byte strings of arbitrary length can be encoded using these primitives.

```
cargo run --example values
```

## Combinators

Learn about combinators and execution in [src/combinator.rs](https://github.com/uncomputable/simple-playground/blob/master/src/combinator.rs).
There is an example in [examples/combinators.rs](https://github.com/uncomputable/simple-playground/blob/master/examples/combinators.rs).
Combinators serve as functions that work on data (aka values).
There are nine combinators: unit, iden, take, drop, injl, injr, pair, comp, case.
Any total function that works on bits or bytes can be computed by some combinator.

```
cargo run --example combinators
```
