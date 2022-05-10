# SAFT - Static Analyzer for Frame palleTs

[![CircleCI](https://circleci.com/gh/simon-perriard/saft/tree/main.svg?style=svg&circle-token=27290d39fe6dbd7d89a3e614f2727114efa59fff)](https://circleci.com/gh/simon-perriard/saft/tree/main)

Static analysis tool that extracts the worst case execution time of a dispatchable call on a [Substrate](https://docs.substrate.io/v3/getting-started/overview/)'s [FRAME](https://docs.substrate.io/v3/runtime/frame/) pallet.

This is my part of my master thesis at the [DSLAB](https://dslab.epfl.ch/) ([EPFL](https://www.epfl.ch/en/)) and [ChainSecurity](https://chainsecurity.com/) and is WIP.

## Installation

Set the toolchain:
```console
./scripts/setup.sh
```

Install the tool for cargo:
```console
cargo install --path .
```

Run it on a pallet:
```console
cd /path/to/frame/pallet/
touch src/*.rs && cargo saft --release
```
We make sure that the pallet is recompiled, otherwise we may not have access to the minimal needed [MIR](https://rustc-dev-guide.rust-lang.org/mir/index.html).