#!/bin/sh

output_dir=$(dirname $0)

for f in ~/.smackage/lib/apltail/v1/tests/*.apl; do
    output=$(basename $f)
    output=$output_dir/${output%.apl}.tail
    aplt -p_tail -p_types -noopt -o "$output" $f
done
