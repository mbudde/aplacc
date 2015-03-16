#!/bin/sh

if [ ! -x "$APLT" ]; then
    echo "APLT env var not set"
    exit 1
fi
input_dir=$(dirname "$APLT")/tests

for f in ${@:-$input_dir/*.apl}; do
    if [ ! -r "$f" ]; then
        f="$input_dir/$f.apl"
    fi
    testname=$(basename $f)
    output=${testname%.apl}.tail
    echo "[32mprocessing ${f}...[0m"
    $APLT -s_tail -p_tail -p_types -o "$output" "$input_dir/../lib/prelude.apl" $f \
       | sed '/^Evaluating$/,/^Result is/ {/^Evaluating$/ d; s/^Result is //; b}; d' \
       >${testname%.apl}.out
done
