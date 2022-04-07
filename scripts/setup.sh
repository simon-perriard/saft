#!/usr/bin/env bash

TOOLCHAIN="nightly"

rustup update stable
rustup update nightly

rustup "+$TOOLCHAIN" component add rustc-dev llvm-tools-preview
# override tool chain
rustup override set $TOOLCHAIN

cd saft_analysis
rustup "+$TOOLCHAIN" component add rustc-dev llvm-tools-preview
# override tool chain
rustup override set $TOOLCHAIN
cd ..

rustup default $TOOLCHAIN