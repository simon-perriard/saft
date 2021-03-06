#!/usr/bin/env bash

TOOLCHAIN="nightly-2022-06-10"

rustup update stable
rustup update nightly

rustup "+$TOOLCHAIN" component add rustc-dev llvm-tools-preview
# override tool chain
rustup override set $TOOLCHAIN

rustup default $TOOLCHAIN