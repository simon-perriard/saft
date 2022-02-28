#!/usr/bin/env bash

TOOLCHAIN="nightly-2022-02-21"

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