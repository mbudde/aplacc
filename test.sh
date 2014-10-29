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

if ! cabal build; then
    echo -e "\033[31;1m<<< Build failed\033[0m"
    exit 1
fi


output=
if [ $verbose ]; then
    output=/dev/stderr
fi

for f in "${files[@]}"; do
    if [ $write_output ]; then
        output="${f%.tail}.hs"
    fi
    if [ $verbose ]; then
        echo -e "\033[33m>>> [$f] Tail input\033[0m"
        cat "$f"
        echo -e "\033[33m>>> [$f] Running aplacc\033[0m"
    fi
    hsoutput=$(./dist/build/aplacc/aplacc "$f")
    if [ $? != 0 ]; then
        echo -e "\033[31;1m<<< [$f] aplacc failed\033[0m"
        continue
    fi
    if [ -n "$output" ]; then
        echo "$hsoutput" > $output
    fi
    echo -e "\033[33m>>> [$f] Running ghc \033[0m"
    echo "$hsoutput" | runghc -isrc
    if [ $? = 0 ]; then
        echo -e "\033[32m<<< [$f] Success\033[0m"
    else
        echo -e "\033[31;1m<<< [$f] ghc failed\033[0m"
    fi
done
