#!/bin/bash

# Check if the correct number of arguments is provided
if [ "$#" -lt 3 ]; then
    echo "Usage: $0 <output_file> <filename_pattern> <directory1> [directory2 ... directoryN]"
    echo "Example of filename_patterns:"
    echo "  \"pwg-*-NLO.top\""
    echo "  \"\""
    exit 1
fi

# Assign the first argument to OUTPUT_FILE
OUTPUT_FILE=$1

# Assign the second argument to FILENAME_PATTERN
FILENAME_PATTERN=$2

# Assign the remaining arguments to the DIRS array
DIRS=${@:3}

# Check if all provided arguments except the first two are directories
for DIR in ${DIRS}; do
    if [ ! -d "$DIR" ]; then
        echo "Error: $DIR is not a directory."
        exit 1
    fi
done

# Empty the output file if it exists
> "$OUTPUT_FILE"

# Find all files matching the pattern in each directory, convert to full paths, and append to OUTPUT_FILE
for DIR in ${DIRS}; do
    find "$DIR" -type f -name "$FILENAME_PATTERN" -exec realpath {} \; >> "$OUTPUT_FILE"
done

# Provide feedback to the user
echo "File paths matching '$FILENAME_PATTERN' saved to $OUTPUT_FILE"