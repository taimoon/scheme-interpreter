#!/bin/bash
source env.sh
set -xe
[ -d pico ] || bash pico_setup.sh

make clean
make all

# make interpreter for pico
mkdir -p build
pushd build
cmake ..
make -j
popd

# test
diff <(BATCH_MODE= LOUD_MODE= ./interp.out test/test-0.scm) test/test-0.txt
time BATCH_MODE= make test
HEAP_SIZE=$(echo "print(16 * (1 << 20))" | python3)
time RIDER=KICK SCM_BOOT=kernel.scm HEAP_SIZE=${HEAP_SIZE} BATCH_MODE= ./interp.out -E kernel-rider.scm kernel.scm
time SCM_BOOT=kernel-rider.scm HEAP_SIZE=${HEAP_SIZE} BATCH_MODE= ./interp.out -E kernel-exp.scm kernel.scm
time SCM_BOOT=kernel-rider.scm HEAP_SIZE=${HEAP_SIZE} BATCH_MODE= ./interp.out -F kernel-free.scm kernel.scm
if [[ "${BOOTSTRAP_TEST}" == "1" ]]; then
    [ -d scheme-compiler ] || git clone https://github.com/taimoon/scheme-compiler
    cp kernel-rider.scm scheme-compiler     # to preprocess compiler
    cp kernel-exp.scm scheme-compiler       # required by compat.scm
    cp scheme-compiler-compat.scm scheme-compiler
    pushd scheme-compiler
    git checkout v0.3.1
    make clean
    cp ../interp.out .
    source env.sh
    export BATCH_MODE=
    export HEAP_SIZE=$(echo "print(256 * (1 << 20))" | python3)
    time SCM_BOOT="kernel-rider.scm" ./interp.out -E compiler-impl.scm \
        lib/scheme-libs.scm \
        lib/set.scm \
        lib/utils.scm \
        front.scm \
        compiler-amd64.scm \
        compiler-rider-amd64.scm
    time SCM_BOOT="kernel-rider.scm" ./interp.out -E compiler.scm scheme-compiler-compat.scm
    make make_runtime SCM_RUNTIME=runtime.so TARGET_ARCH=amd64
    export SCM_RUNTIME=runtime.so
    export SCM_BOOT=compiler.scm
    time FOREIGN_IO=TRUE PRIM_CALLCC=TRUE ./interp.out -o ./a.out test/test-let.scm
    time ./a.out
    time (make bootstrap_3 TARGET_ARCH=amd64 BOOTSTRAP_TEST=0 SCM_CC="./interp.out" SCM_NCC="./compile-amd64.out" NRPOC=4)
    popd
fi