#!/bin/bash
# Adapted from https://raw.githubusercontent.com/raspberrypi/pico-setup/master/pico_setup.sh

# Exit on error
set -e

# Where will the output go?
OUTDIR="$(pwd)/pico"

# Install dependencies
GIT_DEPS="git"
SDK_DEPS="cmake gcc-arm-none-eabi gcc g++ ninja-build"
UART_DEPS="minicom"

# Build full list of dependencies
DEPS="$GIT_DEPS $SDK_DEPS"

echo "Installing Dependencies"
sudo dnf update
sudo dnf install --skip-unavailable -y $DEPS

## Patch require for newer toolchain (Fedora 44)
PICO_SDK_PATCH=$(realpath patches/pico-sdk-gcc16-unused-fix.patch)

echo "Creating $OUTDIR"
# Create pico directory to put everything in
mkdir -p $OUTDIR
cd $OUTDIR

# Clone sw repos
GITHUB_PREFIX="https://github.com/raspberrypi/"
GITHUB_SUFFIX=".git"
SDK_BRANCH="master"

for REPO in sdk examples extras playground
do
    DEST="$OUTDIR/pico-$REPO"

    if [ -d $DEST ]; then
        echo "$DEST already exists so skipping clone"
    else
        REPO_URL="${GITHUB_PREFIX}pico-${REPO}${GITHUB_SUFFIX}"
        echo "Cloning $REPO_URL"
        git clone -b $SDK_BRANCH $REPO_URL
    fi
    echo "Initializing $DEST"
    cd $DEST
    git submodule update --init
    if [[ "$REPO" == "sdk" ]]; then
        git apply ${PICO_SDK_PATCH} | true
    fi
    cd $OUTDIR
    VARNAME="PICO_${REPO^^}_PATH"
    export ${VARNAME}=$DEST
done

cd $OUTDIR
# Debugprobe and picotool
for REPO in picotool debugprobe
do
    DEST="$OUTDIR/$REPO"
    REPO_URL="${GITHUB_PREFIX}${REPO}${GITHUB_SUFFIX}"
    if [ -d $DEST ]; then
        echo "$DEST already exists so skipping"
    elif [[ "$REPO" == "picotool" ]]; then
      git clone -b $SDK_BRANCH $REPO_URL
    else
      git clone $REPO_URL
    fi

    # Build both
    cd $DEST
    git submodule update --init
    cmake -S . -B build -GNinja
    cmake --build build

    if [[ "$REPO" == "picotool" ]]; then
        echo "Installing picotool"
        cmake --install build --prefix=install
        export PATH=$(realpath ./install/bin/):$PATH
    fi

    cd $OUTDIR
done

# Build blink and hello world for default boards
cd pico-examples
for board in pico pico_w pico2 pico2_w
do
    build_dir=build_$board
    cmake -S . -B $build_dir -GNinja -DPICO_BOARD=$board -DCMAKE_BUILD_TYPE=Debug
    examples="blink hello_serial hello_usb"
    echo "Building $examples for $board"
    cmake --build $build_dir --target $examples
done
