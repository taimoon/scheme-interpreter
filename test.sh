#!/bin/bash
set -xe
[ -d scheme-compiler ] || git clone https://github.com/taimoon/scheme-compiler
cppcheck --check-level=exhaustive *.c *.h
gcc -O2 -fno-omit-frame-pointer -g -Wall -rdynamic interp.c main.c -o interp.out
diff <(SCM_BOOT="test/test-macro.scm" ./interp.out) "test/test-macro.txt"
diff <(SCM_BOOT="test/test-callcc.scm" ./interp.out) "test/test-callcc.txt"
diff <(SCM_BOOT="test/test-gc.scm" ./interp.out) "test/test-gc.txt"
diff <(SCM_BOOT="test/test-callcc-tail.scm" ./interp.out) "test/test-callcc-tail.txt"
diff <(SCM_BOOT="test/test-vector.scm" ./interp.out) "test/test-vector.txt"
time RIDER=KICK SCM_BOOT="kernel.scm" ./interp.out -E kernel-rider.scm kernel.scm
time SCM_BOOT="kernel-rider.scm" ./interp.out -E kernel-exp.scm kernel.scm

cp kernel-rider.scm scheme-compiler     # to preprocess compiler
cp kernel-exp.scm scheme-compiler       # required by compat.scm
cp interp.out scheme-compiler

pushd scheme-compiler
make clean
cp ../interp.out .
SCM_BOOT="kernel-rider.scm" ./interp.out -E compiler-impl.scm \
    lib/scheme-libs.scm \
    lib/set.scm \
    lib/utils.scm \
    front.scm \
    compiler-amd64.scm \
    compiler-rider-amd64.scm
SCM_BOOT="kernel-rider.scm" ./interp.out -E compiler.scm ../compat.scm
source env.sh
make make_runtime SCM_RUNTIME=runtime.so TARGET_ARCH=amd64
export SCM_RUNTIME=runtime.so
export SCM_BOOT=compiler.scm
FOREIGN_IO=TRUE PRIM_CALLCC=TRUE ./interp.out -o ./a.out test/test-let.scm
time ./a.out
time make bootstrap_3 TARGET_ARCH=amd64 BOOTSTRAP_TEST=0 SCM_CC="./interp.out" SCM_NCC="./compile-amd64.out"
popd