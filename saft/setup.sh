#!/usr/bin/env bash

TOOLCHAIN="nightly-2021-12-14"

rustup "+$TOOLCHAIN" component add rustc-dev llvm-tools-preview
# override tool chain
rustup override set $TOOLCHAIN
