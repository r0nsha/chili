# 🌶 The Chili Programming Language

Chili is a general-purpose, compiled programming language, focused on productivity, expressiveness and joy of programming™.

At this stage, Chili brings the syntax and fun of a modern, high-level programming language, a robust type system and the performance characteristics of a low-level language.

## Hello, World

```rust
fn main() = println("Hello, World!")
```

For more information about up-to-date syntax and language features, check out the [demo file](https://github.com/r0nsha/chili/blob/main/examples/demo/demo.chl).

## Getting Started

### For Windows

There are a couple of prerequisites here. First, make sure you have [Rust's toolchain](https://www.rust-lang.org/tools/install) installed. Second, make sure you have [Visual Studio Build Tools](https://visualstudio.microsoft.com/downloads/#build-tools-for-visual-studio-2022) installed.
You can also initialize Visual Studio's environment by running `vcvarsall.bat x64`, which should be under `...\VC\Auxiliary\Build`.

You have to compile Chili from source and set up a working directory, to do that, navigate to Chili's directory and run:

```
./build.bat release
```

This will create a directory called `dist/release` under Chili's directory. To build Chili for debug, run `./build.bat debug`.

### For \*Nix systems

First, make sure you have [Rust's toolchain](https://www.rust-lang.org/tools/install) installed.

For Linux, make sure you have `llvm-12` and `clang` installed through your package manager.

MacOS is currently not supported, but will be in the future.

Navigate to Chili's directory and run:

```
sh build.sh release
```

This will create a directory called `dist/release` under Chili's directory. To build Chili for debug, run `sh build.sh`.

### Trying it out

> For ease of use, I recommend adding `dist/release` to your `PATH` environment variable.

In your terminal, run `chili .\examples\playground\build.chl`

```
chili examples/playground/build.chl
```

You should see "Hello, World!" in printed to your terminal.

To run a compile and run a file directly, without using Chili's build API, use the `--run` flag.
Test this out by running the hello world example:

```
chili examples/hello_world.chl --run
```

For some examples of what you can do with Chili, check out the [examples folder](https://github.com/r0nsha/chili/blob/main/examples).

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
- [ ] Named arguments
- [ ] Runtime type introspection
- [ ] Printing and formatting
- [ ] Parametric polymorphism - supporting both types and constant values
- [ ] Associated functions / Methods
- [ ] Memory management model (The design is still in progress)
  - [ ] Ownership (Move & Copy semantics)
  - [ ] Safe references ("Borrowing")
  - [ ] Regions
- [ ] Enums & Pattern matching
  - [ ] Option & Result types
  - [ ] Try ? operator
- [ ] Traits / Typeclasses
- [ ] Closures
- [ ] Built-in code testing

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
- [ ] Basic data structures
  - [ ] Box
  - [ ] List/Vec/Seq (Haven't settled on a name yet)
  - [ ] String
  - [ ] HashMap
  - [ ] HashSet

## Contributing

As the language is in its very early stages, every contribution will help in shaping Chili into what it will become. The best way to contribute right now, is opening issues/bugs and suggesting features/changes. This project is open source, and always will be.

[Our Discord Server](https://discord.gg/Tu4s49Pdre)
