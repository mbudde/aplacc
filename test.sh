#!/bin/bash

toplevel=$(dirname $0)
verbose=
write_output=
files=()
move=

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
        --)
            shift
            files+=("$@")
            break ;;
        *)
            files+=("$1") ;;
    esac
    shift
done

move_to() {
    [ $move ] || return;
    dest="$2/$(basename "$1")"
    if [ ! -f "$dest" ]; then
        mv "$1" "$dest"
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

for f in "${files[@]}"; do
    if [ ! -r "$f" ]; then
        echo -e "\033[31;1m>>> [$f] File not found\033[0m"
        continue
    fi
    if [ $write_output ]; then
        output="${f%.tail}.hs"
    fi
    if [ $verbose ]; then
        echo -e "\033[33m>>> [$f] Tail input\033[0m"
        cat "$f"
        echo -e "\033[33m>>> [$f] Running aplacc\033[0m"
    fi
    hsoutput=$($toplevel/dist/build/aplacc/aplacc "$f")
    if [ $? != 0 ]; then
        echo -e "\033[31;1m<<< [$f] aplacc failed\033[0m"
        move_to "$f" "$toplevel/tests/failing"
        continue
    fi
    if [ -n "$output" ]; then
        echo "$hsoutput" > $output
    fi
    echo -e "\033[33m>>> [$f] Running ghc \033[0m"
    echo "$hsoutput" | runghc "-i$toplevel/src"
    if [ $? = 0 ]; then
        echo -e "\033[32m<<< [$f] Success\033[0m"
        move_to "$f" "$toplevel/tests/working"
    else
        echo -e "\033[31;1m<<< [$f] ghc failed\033[0m"
        move_to "$f" "$toplevel/tests/failing"
    fi
done
