# 2025-12-21

Currently, the interpreter can bootstrap the scheme compiler v0.3.1 and pass bootstrapping test on Linux Fedora, x86 CPU.

- Partial support for call/cc and values
    - unwoke values is not supported
- Macro expansion are baked into the interpreter.
- Bootstrapping scheme compiler v0.3.1 time is 10 minutes.
- Environment are sliced into frames.
- Globals are supported via symbol table.
- Closure, macro and primitives are the same datatype.

# 2024-12-10

Postlouge: I forgot which commit hash of scheme compiler that can recreate this.

Currently, the interpreter can bootstrap the scheme compiler and pass all test on Linux Fedora, x86 CPU.

On my fedora laptop, below is the elapsed time of bootstrapping the compiler and running test

|fedora power setting|power save on|normal|performant|
|-|-|-|-|
|test-interp-compiler-boot.sh|8 min|3 min 40s|3 min 20s|

For comparison, the scheme compiler can bootstrap itself within few seconds.

If I could start over again, then I will do these instead

- Environment should be sliced into frame by frame rather than a linear linked list.
  This enables easier implementation of inner definition bounding while still able to set global variable.
  This is required by the standard document R5RS.
- Design to make closure, macro and primitive application having the same data format.