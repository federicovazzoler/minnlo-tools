#!/bin/bash

# Check if the correct number of arguments is provided
if [ "$#" -lt 2 ]; then
    echo "Usage: $0 <output_file> <directory1> [directory2 ... directoryN]"
    exit 1
fi

# Assign the first argument to OUTPUT_FILE
OUTPUT_FILE=$1

# Assign the remaining arguments to the DIRS array
DIRS=${@:2}

# Check if all provided arguments except the first one are directories
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
    find "$DIR" -type f -name "pwg-*-NLO.top" -exec realpath {} \; >> "$OUTPUT_FILE"
done

# Provide feedback to the user
echo "File paths saved to $OUTPUT_FILE"