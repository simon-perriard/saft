#!/usr/bin/env bash

set -e

echo "*** Initializing WASM build environment"

rustup update nightly
rustup update stable

TOOLCHAIN="nightly"

rustup "+$TOOLCHAIN" component add rustc-dev llvm-tools-preview
# override tool chain
rustup override set $TOOLCHAIN

cd meta
rustup "+$TOOLCHAIN" component add rustc-dev llvm-tools-preview
# override tool chain
rustup override set $TOOLCHAIN
cd ..

cd saft
rustup "+$TOOLCHAIN" component add rustc-dev llvm-tools-preview
# override tool chain
rustup override set $TOOLCHAIN
cd ..

rustup default $TOOLCHAIN