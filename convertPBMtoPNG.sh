#!/usr/bin/env bash

# Check that non-standard programs are installed. "standard" programs are
# anything that is specified in the POSIX.1-2008 standard (and the IEEE Std
# 1003.1 standard) or that is a BASH builtin command. Therefore, "non-standard"
# programs are anything that does not appear on the following two lists:
#   * https://pubs.opengroup.org/onlinepubs/9699919799/idx/utilities.html
#   * https://www.gnu.org/software/bash/manual/html_node/Bash-Builtins.html
if ! type convert &> /dev/null; then
    echo "ERROR: \"convert\" is not installed." >&2
    exit 1
fi
if ! type exiftool &> /dev/null; then
    echo "ERROR: \"exiftool\" is not installed." >&2
    exit 1
fi
if ! type optipng &> /dev/null; then
    echo "ERROR: \"optipng\" is not installed." >&2
    exit 1
fi

# Loop over PBM images ...
for pbm in *.pbm; do
    # Skip those that do not exist ...
    [[ ! -f $pbm ]] && continue

    # Deduce PNG image ...
    png="${pbm%.pbm}.png"

    # Check if the PNG needs making ...
    if [[ $pbm -nt $png ]]; then
        echo "Making \"$png\" ..."

        # Make PNG ...
        convert "$pbm" "$png"
        optipng "$png"
        exiftool -overwrite_original -all= "$png"
    fi

    # Check if the PBM needs removing ...
    if [[ -f $png ]]; then
        echo "Removing \"$pbm\" ..."

        # Remove PBM ...
        rm "$pbm"
    fi
done
