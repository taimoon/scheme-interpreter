#!/bin/bash
set -xe
pushd scheme-compiler
git clone https://github.com/taimoon/scheme-compiler.git
git checkout int-v0
popd
gcc -Wall main.c interp.c -o interp.out
./interp.out interp-boot.scm
compiler_path="scheme-compiler"

cp interp.out "${compiler_path}/interp.out"
mkdir -p "${compiler_path}/preprocess"
cp -rf preprocess/ "${compiler_path}/"
cp interp-compiler.scm "${compiler_path}/interp-compiler.scm"
rm -rf preprocess

cd "${PWD}/${compiler_path}"
source "${PWD}/activate.sh"
gcc -fno-omit-frame-pointer -m32 runtime.c -c -o runtime.o
# scheme --script run-test.scm "./interp-compiler.out ~a"
scheme --script make-compiler.scm "./interp.out interp-compiler.scm ~a" interp-compiler.out
scheme --script run-test.scm "./interp-compiler.out ~a"
scheme --script run-test.scm "./interp-compiler.out ~a" "./interp-compiler-1.out" "./interp-compiler-1.out ~a"
rm -rf scheme-compiler