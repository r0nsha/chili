# 🌶 The Chili Programming Language

Chili is a general-purpose, compiled programming language, focused on productivity, expressiveness and joy of programming™.

At this stage, Chili brings the syntax and fun of a modern, high-level programming language, a robust type system and the performance characteristics of a low-level language.

## Hello, World

```rust
fn main() = println("Hello, World!")
```

For more information about up-to-date syntax and language features, check out the [demo file](https://github.com/r0nsha/chili/blob/main/examples/demo/demo.chl).

## Installation

### Windows

1. [Install](https://www.rust-lang.org/tools/install) the rust toolchain

2. [Install](https://visualstudio.microsoft.com/downloads/#build-tools-for-visual-studio-2022) Visual Studio Build Tools

   ```
   sudo apt-get update
   sudo apt-get -y install llvm-12 clang
   ```

3. Clone the repository

4. Run the build script

   ```
   ./build.bat
   ```

5. You should see a `dist` folder with the compiled binaries. Add `dist` to your PATH environment variable.

6. Restart your shell and try out the compiler:
   ```
   chili examples/playground/build.chl
   ```

### Ubuntu

1. [Install](https://www.rust-lang.org/tools/install) the rust toolchain

2. Install LLVM and Clang binaries

   ```
   sudo apt-get update
   sudo apt-get -y install llvm-12 clang
   ```

3. Clone the repository

4. Run the build script

   ```
   sh build.sh release
   ```

5. You should see a `dist` folder with the compiled binaries. Add `dist` to your PATH environment variable.

6. Restart your shell and try out the compiler:
   ```
   chili examples/playground/build.chl
   ```

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
- [ ] Memory management model (The design is still in progress)
  - [ ] Ownership (Move & Copy semantics)
  - [ ] Safe references ("Borrowing")
  - [ ] Regions
- [ ] Enums & Pattern matching
  - [ ] Option & Result types
  - [ ] Try ? operator
- [ ] Traits / Typeclasses
- [ ] Closures
- [ ] Named function arguments
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
