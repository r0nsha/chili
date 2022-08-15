# 🌶 The Chili Programming Language

Chili is a general-purpose, compiled programming language, focused on productivity, expressiveness and joy of programming™.

Chili brings a robust type system, brings the joy of programming in a modern language, with the performance characteristics of lower-level languages.

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

**Note:** Although the compiler itself supports both Windows and Linux(Mac OS support is on the way), the standard library is very young, and doesn't support Windows yet. This will be implemented in the near future. Have a look at the [Tasks](#Tasks) section.

## Running the example:

1. Clone the repository, and go through the (Installation)[#Installation] section.
2. Open your favorite terminal
3. Change directory to the repository's root
4. Run: `cargo run -- ./examples/hello_world.chl --run`

## Running the compiler from cargo:

The compiler targets the interpreter by default, which means that your `main` function won't run, unless specified with a top level `static` block. Look at the [demo file](https://github.com/r0nsha/chili/blob/main/examples/demo/demo.chl) for more information about compile-time evaluation.

To run the compiler (in interpreter mode):

```
cargo run -- %source_file_path%
example: cargo run -- ./build.chl
```

As a convinience, you can compile and run a file directly by adding `--run` or `-r`:

```
cargo run -- %source_file_path% --run
example: cargo run -- ./src/main.chl --run
```

## Tooling

- VSCode plugin is available [here](https://marketplace.visualstudio.com/items?itemName=chili-lang.chili) (currently includes syntax highlighting)

## Tasks

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
- [x] Standard library support for Linux
- [ ] Runtime type introspection
- [ ] Printing and formatting
- [ ] Parametric polymorphism - supporting both types and constant values
- [ ] Standard library support for Windows
- [ ] Associated functions / Methods
- [ ] Memory management model - undecided, but here are some options, from most likely to least likely:
  - [ ] Ownership + ARC
  - [ ] Regions
  - [ ] Ownership
  - [ ] Garbage collection
- [ ] Enums & Pattern matching
- [ ] Traits / Typeclasses
- [ ] Closures
- [ ] Named function arguments

## Contributing

As the language is in its very early stages, every contribution will help in shaping Chili into what it will become. The best way to contribute right now, is opening issues/bugs and suggesting features/changes. This project is open source, and always will be.

[Our Discord Server](https://discord.gg/Tu4s49Pdre)
