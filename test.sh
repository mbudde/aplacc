#!/bin/bash

verbose=
write_output=
files=()

while [ $# != 0 ]; do
    case "$1" in
        -v)
            verbose=1 ;;
        -w)
            write_output=1 ;;
        --)
            shift
            files+=("$@")
            break ;;
        *)
            files+=("$1") ;;
    esac
    shift
done

cabal build


if [ $verbose ]; then
    output=/dev/stderr
else
    output=/dev/null
fi

for f in "${files[@]}"; do
    if [ $write_output ]; then
        output="${f%.tail}.hs"
    fi
    if [ $verbose ]; then
        echo -e "\033[33;1m>>> Tail input\033[0m"
        cat "$f"
        echo -e "\033[33;1m>>> Accelerate output\033[0m"
    fi
    ./dist/build/aplacc/aplacc "$f" \
        | (tee "$output"; echo -e "\033[33;1m>>> Result\033[0m" >/dev/stderr) \
        | runghc -isrc
done
