#!/bin/bash

bin=$1
scripts_dir=$2

help()
{
    echo "Command should be ./integration-tests.sh <binary filepath> <scripts filepath>"
}

if [ -z "$bin" ]; then
    echo "Binary filepath not found."
    help
    exit 1
fi

if [ -z "$scripts_dir" ]; then
    echo "Scripts filepath not found"
    help
    exit 1
fi

mkdir -p scripts

for file in "$scripts_dir"/*; do
    echo "Running $file..."
    "$bin" "$file" > ./scripts/"$file".txt
done
