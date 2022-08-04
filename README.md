# 🌶 Chili

General-purpose programming language focused on productivity, expressiveness and joy of programming™.

> **🌶 The language is still going through its early iterations.**  
> Nothing is final, and everything is subject to potential changes.

## What? Another programming language?

There's still a pretty large gap between high-level and low-level languages.

We either choose complexity and gain performance (i.e manual memory management), or we choose simplicity and lose performance (i.e reference counting or garbage collection).

There are languages such as `Rust` which succeed in making this gap smaller, but the friction is sometimes too high (i.e fighting the borrow checker).

**🌶 Chili** tries to tackle these issues by:

1. Keeping the syntax **orthogonal** and **simple**.
2. Providing a **robust type system**.
3. Being **fully open source** and **community-based**, with the goal of **serving the users**.

> **📝 Note:** There are some thought experiments about including
> ownership, move semantics, or any other strong safety guarantees.
> As discussed in this section, this brings a big learning-curve and lots of complexities with it,
> which is why it was not implemented yet.

## Hello World

```rust
fn main = println("Hello World");
```

For more information about syntax and features, check out the [demo file](https://github.com/r0nsha/chili/blob/main/docs/demo.chl).

## Installation

> **🌶 The language is still going through its early iterations.**  
> To build the compiler from source, you will need to:
>
> - [Set up](https://github.com/llvm/llvm-project/releases/tag/llvmorg-12.0.1) LLVM 12 on your machine (I'm working on including LLVM as part of the project).
> - [Install](https://www.rust-lang.org/learn/get-started) the rust compiler

As there's no freestanding binary yet, we still need to build the compiler from source:

```
cargo build
```

### Running the compiler from cargo:

The compiler is ran in interpreter mode by default, which means that your code won't run, unless specified with a top level `static` block. Look at the [demo file](https://github.com/r0nsha/chili/blob/main/docs/demo.chl) for more information about compile-time evaluation.

To run the compiler (in interpreter mode):

```
cargo run -- %source_file_path%
example: cargo run -- ./build.chl
```

As a convinience, you can compile and run a file directly, by specifying the `--run` flag:

```
cargo run -- %source_file_path% --run
example: cargo run -- ./src/main.chl --run
```

## Tooling

- VSCode plugin is available [here](https://github.com/r0nsha/chili/tree/main/editors/vscode) (currently includes syntax highlighting)

## Contributing

As the language is in its very early stages, every contribution will help in shaping Chili into what it will become. The best way to contribute right now, is opening issues/bugs and suggesting features/changes.

[Our Discord Server](https://discord.gg/Tu4s49Pdre)

## Main Tasks

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
- [ ] Default function arguments
- [ ] Named function arguments
- [ ] Panic function
- [ ] Varargs
- [ ] Printing and formatting
- [ ] Parametric polymorphism - supporting both types and constant values
- [ ] Tagged unions
  - [ ] Pattern matching
  - [ ] First-class `Option` & `Result` types
- [ ] Memory management model - undecided, but here are some options, from most likely to least likely:
  - [ ] Ownership + ARC
  - [ ] Regions
  - [ ] Ownership
  - [ ] Garbage collection
- [ ] Associated functions / Methods
- [ ] Traits / Typeclasses
- [ ] Closures
