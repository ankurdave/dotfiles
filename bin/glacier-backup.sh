#!/bin/bash

# glacier-backup.sh - back up directories to Amazon Glacier

if [ -z $1 -o -z $2 ]; then
    echo "Usage: glacier-backup.sh SRC_DIR DST_VAULT"
    exit -1
fi

echo "Backing up top-level directories in $1 to vault $2..."

find $1 -mindepth 1 -maxdepth 1 -type d | while read dir; do
    dir_basename=$(basename "$dir")
    echo "Backing up $dir..."
    tar -c "$dir" | pv | glacier archive upload --name "$dir_basename.tar" "$2" -
done
