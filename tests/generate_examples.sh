#!/bin/sh

if [ ! -x "$APLT" ]; then
    echo "APLT env var not set"
    exit 1
fi
output_dir=$(dirname $0)
input_dir=$(dirname "$APLT")/tests

for f in ${@:-$input_dir/*.apl}; do
    output=$(basename $f)
    output=$output_dir/${output%.apl}.tail
    echo "[32mprocessing ${f}...[0m"
    $APLT -c -s_tail -p_tail -p_types -o "$output" "$input_dir/../lib/prelude.apl" $f
done
