# Lost

A general-purpose interpreted programming language written in Rust. Guaranteed to be Blazingly Fast™ :P

# Why the name?

This is loosely based on the Lox programming language from [Crafting Interpreters](https://craftinginterpreters.com) by Robert Nystrom.

For obvious reasons, I cannot name it Lust, hence Lox + Rust = Lost :)

# Usage

To run the Lost REPL, simply execute `cargo run --bin lost`.

To invoke the interpreter on a `.lost` file, pass the file as an argument, i.e. `cargo run --bin lost file.lost`.

The experimental bytecode VM can be used with `--bin lost_vm`. **It is not complete yet, use at your own risk!**

# Getting started

The syntax is fairly straightforward, and is mostly borrowed from existing languages. Reading the below snippet should suffice to start writing code in Lost.

```rust
fn foo(n) {
    if (n == 0) {
        print("zero");
        while (n <= 1) {
            print(n);
            n = n + 1;
        }
        return true;
    } else {
        print("not zero");
        // Post-increment (i++) tends to be ambiguous, both in
        // expected behaviour and actual outcome. We're better
        // off without using it - the only type of increment
        // available is pre-increment (++i).
        for (let i = 0; i < n; ++i) {
            print(i);
        }
        return false;
    }
}

let n = 5; // All numbers are floating point values
print(foo(n)); // false
print(foo(0)); // true, prints "Zero"

let a; // Uninitialised variables are null by default
print(a); // null

// Classes are slightly different compared to other languages.
// There are no fields in the class declaration, only methods.
// Self-referencing works with the `this` keyword.
class Vehicle {
  // The constructor is just a function
  // with the same name as the class
  fn Vehicle() {
    this.wheels = 0;
  }

  fn countWheels() {
    print(this.wheels);
  }
}

// Classes can inherit from a parent class with the `<-` operator
class Car <- Vehicle {
  fn Car() {
    this.wheels = 4;
  }
}

let car = Car();
car.countWheels(); // 4
```

# License

This project is licensed under the [MIT License](/LICENSE). Do whatever you want with it.
