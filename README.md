# Scheme Interpreter

This repo contains scheme interpreter written in C.
The goal is to develop an interpreter that can bootstrap my scheme compiler at [https://github.com/taimoon/scheme-compiler](https://github.com/taimoon/scheme-compiler).
In other word, the program is meant to be thrown away once bootstrap the compiler and work in the new language.
Therefore, this repo lacks of how to use interpreter itself.
Furthermore, the program is hacky and the technique here is not well documented here.

Currently, the interpreter can bootstrap the scheme compiler and pass all test on Linux Fedora, x86 CPU.

On my fedora laptop, below is the elapsed time of bootstrapping the compiler and running test

|fedora power setting|power save on|normal|performant|
|-|-|-|-|
|bootstrap|1 min|25s|22s|
|test-interp-compiler-boot.sh|15 min|7 min|6 min|

For comparison, the scheme compiler can bootstrap itself within few seconds.

If I could start over again, then I will do these instead

- Environment should be sliced into frame by frame rather than a linear linked list.
  This enables easier implementation of inner definition bounding while still able to set global variable.
  This is required by the standard document R5RS.
- Design to make closure, macro and primitive application having the same data format.

# Prerequisite 

Have the same prerequisite as in [https://github.com/taimoon/scheme-compiler](https://github.com/taimoon/scheme-compiler)

Then, run this command to start bootstrap and testing.

```sh
sh test-interp-compiler-boot.sh
```

# How to Use

```sh
gcc -Wall main.c interp.c -o interp.out
./interp.out test/test-lit.scm    # without library
./interp.out interp-boot.scm      # to have preprocess libraries
./interp.out interp.scm test/test-match.scm
scheme --script run-test.scm      # run test using chez-scheme
```

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
This allows tracing GC objects to trace from these registers rather than stack walking.
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
The workaround is to do trampoline by repeatively calling the global variable `NEXT`.
C does not support first class "label" which I mean that we cannot really goto everywhere arbitrary like we do in assembly.
Indeed, `NEXT` is a workaround to jump to everywhere arbitrary except with a severe limitation: it does not accept any parameters.
C provides no direct support polymorphic procedures.
Possible implementation is to use variadic procedures but I never learn how to and I decide that inventing new trick is more fun.
Instead of learning new trick, I dictate that all trampoline procedures will get arguments from the global variables `VAL` and `CONT`.
To sum, `NEXT` is the goto I am using.

# Reference
- [Essentials of Programming Languages - Third Edition](https://eopl3.com/)
