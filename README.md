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
let main = fn {
	println("Hello World");
}
```

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

To run the compiler directly from cargo:

```
cargo run -- %source_file_path% --run
example: cargo run -- ./examples/main.chl --run
```

## Tooling

- VSCode plugin is available [here](https://github.com/r0nsha/chili/tree/main/editors/vscode) (currently includes syntax highlighting)

## Contributing

As the language is in its very early stages, every contribution will help in shaping Chili into what it will become. The best way to contribute right now, is opening issues/bugs and suggesting features/changes.

[Our Discord Channel](https://discord.gg/Tu4s49Pdre)

## Syntax

### Types

```rust
// integers
i8 i16 i32 i64
int // machine-word sized integer

// unsigned integers
u8 u16 u32 u64
uint // machine-word sized unsigned integer

// floats
f16 f32 f64

*t // immutable pointer to t
*mut t // mutable pointer to t

[n]t // array of type t with size n
[]t // slice of type t
str // slice of u8

(t1, t2, ..) // tuple

struct { x: t1, y: t2, .. } // struct
```

### Variables

```rust
let x = 5;
let x: int = 5; // you can annotate the variable with a type
```

### Arrays & Slices

```rust
let array: [3]int = [1, 2, 3]; // [1, 2, 3]

// a slice is a struct contains a pointer to an array, and its length
let slice: []int = array[1..]; // [2, 3]

// references to arrays can be coerced to slices
let slice: []int = &array;

array.len // get the length of the array
slice.len // get the length of the slice
```

### Tuples

```rust
let v = (1, 2); // initializes a tuple with (1, 2)

let first = v.0; // access the 0th component of the tuple

let (x, y) = v; // unpack `v` into its components
```

### Structs

```rust
type Vector2 = { // named struct declaration
	x: f32,
	y: f32
};

let v = Vector2 { x = 1, y = 2 }; // initializes a Vector2 with { x = 1, y = 2 }

let x = v.x; // access the `x` field

let { x, y } = v; // unpack `v` into its components
```

### Functions

```rust
// function declaration with explicit return
let add = fn(a: int, b: int) -> int {
	return a + b;
}

// function declaration with implicit return
let add = fn(a: int, b: int) -> int {
	// the return and the semicolon can be omitted here
	// to indicate that we want to return the value.
	// this is true for all blocks.
	a + b
}
```

### Importing from other modules

```rust
// each file is a seperated module (or namespace in other languages)

//
// file: foo.chl
//

// all functions/variables in a file are private by default
let foo = 5;

// add the `pub` keyword to make it public to other modules
pub let im_public = 5;

//
// file: bar.chl
//

// you can use other modules (files) using the `import` keyword
// foo is on the same directory as us, so we only have to type `foo`.
let foo = import("foo")

// modules can be unpacked the same way as structs
let { im_public } = import("foo")

// the compiler can automatically unpack all public
// symbols from `foo` for us, using `?`
let ? = import("foo")

// Using the imported symbol
let main = fn() {
    let x = im_public; // we can now use im_public in `bar.chl`
}
```

## Licenses

- https://opensource.org/licenses/MIT
- https://www.apache.org/licenses/LICENSE-2.0
