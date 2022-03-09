#! /bin/bash

git clone git@github.com:paritytech/substrate.git

cd substrate/frame/

no_extract=("benchmarking" "examples" "election-provider-support" "executive" "staking" "try-runtime" "whitelist")
# ne_extract contains folders that are not pallets
# whitelist is a pallet but does not even build on Substrate repo

for pallet in *
do
    if [[ ! " ${no_extract[*]} " =~ "${pallet}" ]]
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