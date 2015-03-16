#!/bin/bash

shopt -s nullglob

toplevel=$(dirname $0)
verbose=
write_output=
files=()
move=
bail=

while [ $# != 0 ]; do
    case "$1" in
        -v)
            verbose=1 ;;
        -w)
            write_output=1 ;;
        -a|--all)
            files=($toplevel/tests/working/*.tail $toplevel/tests/failing/*.tail) ;;
        -m|--move)
            move=1 ;;
        --bail)
            bail=1 ;;
        --)
            shift
            for name in "$@"; do
                if [ -d "$name" ]; then
                    files+=(${name%%/*}/*.tail)
                else
                    files+=($name)
                fi
            done
            break ;;
        *)
            if [ -d "$1" ]; then
                files+=(${1%%/*}/*.tail)
            else
                files+=($1)
            fi
            ;;
    esac
    shift
done

move_to() {
    [ $move ] || return;
    dest="$2/$(basename "$1")"
    if [ ! -f "$dest" ]; then
        git mv "$1" "$dest"
    fi
}

test_output() {
    [ -r "${1}.out" ] || return
    expect=$(cat "${1}.out" | tail -1 | sed 's/^\[\](\(.*\))$/\1/; s/HUGE_VAL/Infinity/')
    got=$(echo "$2" | tail -1 | sed  's/^.*\[\(.*\)\]/\1/')
    if [ "$got" = "Infinity" -a "$expect" = "$got" ]; then
        result=0
    else
        result=$(echo "scale=5; (${got/[eE]/*10^} - ${expect/[eE]/*10^})/1" | bc)
    fi
    if [ "$result" = "0" ]; then
        echo -e "\033[32m<<< [$f] Result correct\033[0m"
    else
        echo "Expected: $expect"
        echo "Got:      $got"
        echo -e "\033[31m<<< [$f] Result incorrect\033[0m"
    fi
}

pushd $toplevel
if ! cabal build; then
    echo -e "\033[31;1m<<< Build failed\033[0m"
    exit 1
fi
popd

output=
if [ $verbose ]; then
    output=/dev/stderr
fi

declare -i num_tests=0 num_failed=0

for f in "${files[@]}"; do
    if [ ! -f "$f" ]; then
        echo -e "\033[31;1m>>> [$f] File not found\033[0m"
        echo
        continue
    fi
    if [ $write_output ]; then
        output="${f%.tail}.hs"
    fi
    echo -e "\033[33m>>> [$f] Testing... \033[0m"
    num_tests+=1
    if [ $verbose ]; then
        echo -e "\033[33m>>> [$f] Tail input\033[0m"
        cat "$f"
        echo -e "\033[33m>>> [$f] Running aplacc\033[0m"
    fi
    hsoutput=$($toplevel/dist/build/aplacc/aplacc --tail "$f")
    if [ $? != 0 ]; then
        echo -e "\033[31;1m<<< [$f] aplacc failed\033[0m"
        move_to "$f" "$toplevel/tests/failing"
        num_failed+=1
        echo
        if [ $bail ]; then
            break
        else
            continue
        fi
    fi
    if [ -n "$output" ]; then
        echo "$hsoutput" > $output
    fi
    echo -e "\033[33m>>> [$f] Running ghc \033[0m"
    result=$(echo "$hsoutput" | runghc "-i$toplevel/src")
    if [ $? = 0 ]; then
        echo "$result"
        echo -e "\033[32m<<< [$f] Success\033[0m"
        test_output "${f%.tail}" "$result"
        echo
        move_to "$f" "$toplevel/tests/working"
    else
        echo "$result"
        echo -e "\033[31;1m<<< [$f] ghc failed\033[0m"
        echo
        move_to "$f" "$toplevel/tests/failing"
        num_failed+=1
        if [ $bail ]; then
            break
        fi
    fi
done
if [ $num_failed = 0 ]; then
   echo -e "\033[32m$num_tests tests run, all succeeded\033[0m"
else
   echo -e "\033[31m$num_tests tests run, $num_failed failed\033[0m"
   exit 1
fi
