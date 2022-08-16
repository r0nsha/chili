# 🌶 The Chili Programming Language

Chili is a general-purpose, compiled programming language, focused on productivity, expressiveness and joy of programming™.

At this stage, Chili brings a robust type system, bringing the syntax and fun of a modern high-level programming language, with the performance characteristics of a low-level language.

## Hello, World

```rust
fn main() = println("Hello, World!")
```

For more information about up-to-date syntax and language features, check out the [demo file](https://github.com/r0nsha/chili/blob/main/examples/demo/demo.chl).

## Installation

> **🌶 The language is still going through its early iterations.**  
> To build the compiler from source, you will need to:
>
> - [Set up](https://github.com/llvm/llvm-project/releases/tag/llvmorg-12.0.1) LLVM 12 on your machine.You should build from source under windows.
> - [Install](https://www.rust-lang.org/tools/install) the rust compiler

As there's no freestanding binary yet, we still need to build the compiler from source. To build the compiler, run:

```
cargo build
```

**Note:** Although the compiler itself supports both Windows and Linux (Mac OS support is also planned), the standard library is very young, and doesn't support Windows yet. This will be implemented in the near future. Have a look at the [Tasks](#Tasks) section.

## Running the example:

1. Open your favorite terminal
2. Change directory to Chili's repository's root
3. Run: `cargo run -- ./examples/hello_world.chl --run`

## Tooling

- VSCode plugin is available [here](https://marketplace.visualstudio.com/items?itemName=chili-lang.chili) (currently includes syntax highlighting)

## Tasks
### Compiler
- [x] Functions
- [x] Variables
- [x] Static Typing
  - [x] Global type inference
  - [x] Scalar types
  - [x] Pointers & Arrays & Slices
  - [x] Structs & Tuples
- [x] Modules & Imports
- [x] Binding patterns: Struct/Module unpack, Tuple unpack and Wildcard
- [x] Compile time execution with FFI support
  - [x] FFI support
  - [x] Build configuration based on compile-time execution
- [x] Dynamically sized types
- [x] Static variables
- [x] Attributes
- [x] Default function arguments
- [x] Panic function
- [x] Varargs
- [ ] Runtime type introspection
- [ ] Printing and formatting
- [ ] Parametric polymorphism - supporting both types and constant values
- [ ] Associated functions / Methods
- [ ] Memory management model - undecided, but here are some options, from most likely to least likely:
  - [ ] Regions
  - [ ] Ownership + ARC
  - [ ] Ownership
  - [ ] Garbage collection
- [ ] Enums & Pattern matching
- [ ] Traits / Typeclasses
- [ ] Closures
- [ ] Named function arguments
- [ ] Code testing
- [ ] Error handling
- [ ] Multithreading

### Standard library
- [ ] OS Abstractions for Windows
  - [ ] Filesystem API
  - [ ] Networking and HTTP
  - [ ] Date/Time
- [ ] OS Abstractions for Linux
  - [ ] Filesystem API
  - [ ] Networking and HTTP
  - [ ] Date/Time
- [ ] OS Abstractions for MacOS
  - [ ] Filesystem API
  - [ ] Networking and HTTP
  - [ ] Date/Time
- [ ] Formatting/Printing
- [ ] Basic data structure and algorithm


## Contributing

As the language is in its very early stages, every contribution will help in shaping Chili into what it will become. The best way to contribute right now, is opening issues/bugs and suggesting features/changes. This project is open source, and always will be.

[Our Discord Server](https://discord.gg/Tu4s49Pdre)
