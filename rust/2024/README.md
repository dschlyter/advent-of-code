## Setup

Install Rust https://www.rust-lang.org/tools/install

Install rust-analyser for vscode

TODO CodeLLDB plugin?

## Run

    cargo run --bin day1

or run a specific test file

    cargo run --bin day1 _test

## Notes

Cool syntax/features I should use:

- if/while/etc blocks return values
    - `break 42` makes the block evaluate to 42
- destructuring with `..` to ignore tail or struct elements
- if let
- let else
- while let

Lang notes

- `for x in col` will default to `col.into_iter()` which consumes the collection.
    - alternatives `col.iter()` and `col.iter_mut()`
- closures: Fn, FnMut, FnOnce
- generics: <T: Eq + Hash>