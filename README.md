# Lost

A programming language written in Rust. Guaranteed to be Blazingly Fast™ :P

# Why the name?

This was inspired by the Lox programming language from [Crafting Interpreters](https://craftinginterpreters.com) by Robert Nystrom.

For obvious reasons, I cannot name it Lust, hence Lox + Rust = Lost :)

# Usage

To run the Lost REPL, simply execute `cargo run`.

To invoke the compiler on a `.lost` file, pass the file as an argument to the above command, i.e. `cargo run file.lost`.

# Getting started

The syntax is fairly straightforward, and borrows the best syntaxes of existing languages. Reading the below snippet should suffice to start writing code in Lost.

```rust
fn foo(n) {
    if (n == 0) {
        print "Zero";
        while (n <= 1) {
            print n;
            n = n + 1;
        }
        return true;
    } else {
        print "Not zero";
        for (let i = 0; i < n; i = i + 1) {
            print i;
        }
        return false;
    }
}

let n = 5; // All numbers are handled as floating point values
let isZero = foo(n);
print isZero; // false
print foo(0); // true

let a; // Uninitialised variables are null by default
print a; // null
```
