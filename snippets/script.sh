#!/usr/bin/env bash

# Check if a directory is provided
if [ -z "$1" ]
then
  echo "Usage: $0 <directory>"
  exit 1
fi

# Directory to process
DIR="$1"

# Find all files and replace four spaces with a tab
find "$DIR" -type f -exec sed -i 's/    /\t/g' {} +

echo "Conversion completed."
