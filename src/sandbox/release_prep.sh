#!/usr/bin/env bash

# Input and output files
input="${1:-luadap.lua}"
output="${2:-luadap_release.lua}"

echo "Preparing release build: $input → $output"

awk '

BEGIN {
    in_block_comment = 0      # for normal --[[ ... ]] comments
    in_long_comment = 0       # for --[=[ ... ]=] comments
    long_comment_end = ""     # regex for closing long comment
}

# Remove print_nicely(...) calls (whole line)
# Assumes one call per line, may have spaces before
/^[[:space:]]*print_nicely[[:space:]]*\(.*\)[[:space:]]*$/ {next}

{
    line = $0

    # Detect Lua long bracket comments with equals (keep them)
    if (match(line, /^[[:space:]]*--\[(=+)\[/, arr)) {
        print line
        next
    }

    # Remove normal block comments --[[ ... ]] (no equals)
    if (in_block_comment) {
        if (line ~ /^[[:space:]]*\]\]/) {
            in_block_comment = 0
        }
        next
    }
    if (line ~ /^[[:space:]]*--\[\[/) {
        in_block_comment = 1
        next
    }

    # Remove single-line comments
    gsub(/^[[:space:]]*--.*$/, "", line)
    gsub(/--.*$/, "", line)  # also remove trailing comments

    # Print only non-empty lines
    if (line ~ /[^[:space:]]/) print line
}
' "$input" > "$output"

echo "Done."
