# Scheme Interpreter

This repo contains scheme interpreter written in C.
The goal is to develop an interpreter that can bootstrap my scheme compiler at [https://github.com/taimoon/scheme-compiler](https://github.com/taimoon/scheme-compiler).
In other words, the program is meant to be thrown away once bootstrap the compiler and work in the new language.
Therefore, this repo lacks of how to use interpreter itself.
Furthermore, the program is hacky and the technique here is not well documented here.

For background and development notes, see [CHANGELOG](CHANGELOG.md).

# How to use

The usage involve complicated bootstrapping step. These are workarounds to streamline and speed up macro expansion during compiler bootstrap.

```bash
gcc -O2 -fno-omit-frame-pointer -g -Wall -rdynamic interp.c main.c -o interp.out
RIDER=KICK SCM_BOOT="kernel.scm" ./interp.out -E kernel-rider.scm kernel.scm
SCM_BOOT="kernel-rider.scm" ./interp.out test.scm
```

The `-E <output-file> <input-file> ...` option expands the input Scheme files and concatenates the result into `<output-file>`.
Generating kernel-rider.scm takes roughly a minute.
Here, test.scm is your intended input.

Since `kernel-rider.scm` contains CLI handling and always loads `test.scm`, that you might not want this.
In that case, you can skip the rider kick, but you must ensure that the interpreter runs the expanded `kernel.scm` first before continuing with your program and for example, you can achieve this by starting your program with `(include "kernel-exp.scm")`.: 

```bash
gcc -O2 -fno-omit-frame-pointer -g -Wall -rdynamic interp.c main.c -o interp.out
RIDER=KICK SCM_BOOT="kernel.scm" ./interp.out -E kernel-rider.scm kernel.scm
SCM_BOOT=kernel-rider.scm ./interp.out -E kernel-exp.scm kernel.scm
SCM_BOOT=kernel-rider.scm ./interp.out -E a.scm test.scm
SCM_BOOT=a.scm ./interp.out
```

# Features

- Minimal Scheme subset, sufficient to bootstrap the compiler
- LISP-style macros (unhygienic, `define-macro`)
- eval (runtime evaluation of code expressions)
- call/cc (partial support)
- multiple-values (partial support)
- First-class functions
- Interpreter semantics

# Interlude

This repo is a solution to SICP's exercise 5.51.

SICP's exercise 5.51 asked the readers to implement scheme in C but by translating the explicit-control evaluator in the same book into C.
Besides, the authors also tell readers that you will need storage allocation routine and other run-time supports.
Though, one bad thing about this exercise is that authors did not warn readers how much efforts needed in doing these.
And, I can't really grasp the whole chapter 5 back then. (XwX)


# Implementation Note

Beside know how to program in C and scheme,
the program is written in continuation passing style,
so have to get comfortable convert program into CPS.
Readers may refer the book EOPL to learn about CPS.

Continuation passing style is one of method enabling developer to implement functional programming language in imperative programming language.
However, my main motivation is CPS as a workaround to implement garbage collector for the interpreter
in a non-GC language like C.
This is because consequence of CPS is the program run on fixed set of registers which are technically the root set.
This allows tracing GC objects starting from these registers rather than walking the stack.
GC is required because bootstrapping the scheme compiler simply need a lot of memory.

CPS program is also tail-call, hence tail recursive call would not cause stack overflow.

I write proof-of-concept/reference implementation for CPS interpreter in the choice of language
that support GC and first-class function like scheme and python.
Only then continue programming in C from the reference implementation.

Personally, I convert the program CPS by thinking:

- What to do next step
- Store the thing you need after returning from the next step

For example,

```scheme
; (eval (+ left right)) => (pr+ (eval left) (eval right))
(eval (+ ( * 3 3) (* 4 4)) end-cont)
(eval (* 3 3) (pr+ (* 4 4) end-cont))
(eval 9 (pr+ (* 4 4) end-cont))
(apply-cont 9 (pr+ (* 4 4) end-cont))
(eval (* 4 4) (pr+* 9 end-cont))
(eval 16 (pr+* 9 end-cont))
(apply-cont 16 (pr+* 9 end-cont))
(eval 25 end-cont)
(apply-cont 25 end-cont)
25
```

Though the statement appears very abstract.
The example is actually adapted from chapter 8 of the book at the website [https://users.cs.utah.edu/~mflatt/past-courses/cs7520/public_html/s06/notes.pdf](https://users.cs.utah.edu/~mflatt/past-courses/cs7520/public_html/s06/notes.pdf).

C does not guarantee tail call and C accumulates context for every function call.
The workaround is to do trampoline by repeatedly calling the global variable `NEXT`.
C does not support first class "label" which I mean that we cannot really goto everywhere arbitrary like we do in assembly.
Indeed, `NEXT` is a workaround to jump to everywhere arbitrary except with a severe limitation: it cannot directly accept parameters.
C provides no direct support polymorphic procedures.
Possible implementation is to use variadic procedures but I never learn how to and I decide that inventing new trick is more fun.
Instead of learning new trick, I dictate that all trampoline procedures will get arguments from the global variables `VAL` and `CONT`.
To sum, `NEXT` is the goto I am using.

# Reference
- [Essentials of Programming Languages - Third Edition](https://eopl3.com/)
