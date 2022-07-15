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

## Running the tool

Run it on a pallet:
```console
cd /path/to/frame/pallet/
touch src/*.rs && cargo saft --release
```
We make sure that the pallet is recompiled, otherwise we may not have access to the minimal needed [MIR](https://rustc-dev-guide.rust-lang.org/mir/index.html).

### Some considerations

The tool does not support recursion, loops and iterators (for now).

The tool does not have access to all the MIR it wants, since we run it on a pallet, some concrete types that are injected by the Runtime are still generic and [monomorphization](https://rustc-dev-guide.rust-lang.org/backend/monomorph.html) cannot happen yet. This leads to the need of manual specifications, and thus imprecisions. The tool, in this proof-of-concept state, only supports the following pallets: [balances](https://github.com/paritytech/substrate/tree/master/frame/balances), [identity](https://github.com/paritytech/substrate/tree/master/frame/identity), [utility](https://github.com/paritytech/substrate/tree/master/frame/utility), [vesting](https://github.com/paritytech/substrate/tree/master/frame/vesting) and [multisig](https://github.com/paritytech/substrate/tree/master/frame/multisig). It possibly supports other pallets but the effort has been put on the previous list in particular.

You can add specifications to fill the needs for your pallet in the [specifications_v2.rs](https://github.com/simon-perriard/saft/blob/symbex/src/analysis/specifications_v2.rs) file.

This is a research project and it is not intended to be used as a product to fully trust.
