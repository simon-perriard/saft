#! /bin/bash

git clone git@github.com:paritytech/substrate.git

cd substrate/frame/

no_extract=("aura authority-discovery authorship benchmarking" "support" "examples" "election-provider-support" "executive" "staking" "try-runtime")
# no_extract contains folders that are not pallets

extract=("balances" "identity" "multisig" "utility" "vesting")

for pallet in *
do
    #if [[ ! " ${no_extract[*]} " =~ "${pallet}" ]]
    if [[ " ${extract[*]} " =~ "${pallet}" ]]
    then
        cd $pallet
        echo "Extracting juice from $pallet"
        touch src/lib.rs
        cargo saft --release
        if [ $? -ne 0 ]
        then
            exit 1
        fi
        cd ..
    fi
done