#!/usr/bin/env bash

# Loop over PPM images ...
for ppm in *.ppm; do
    # Skip those that do not exist ...
    [[ ! -f $ppm ]] && continue

    # Deduce PNG image ...
    png="${ppm%.ppm}.png"

    # Check if the PNG needs making ...
    if [[ $ppm -nt $png ]]; then
        echo "Making \"$png\" ..."

        convert $ppm $png
        exiftool -overwrite_original -all= $png
        optipng $png
    fi

    # Check if the PPM needs removing ...
    if [[ -f $png ]]; then
        echo "Removing \"$ppm\" ..."

        # Remove PPM ...
        rm $ppm
    fi
done
