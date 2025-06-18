#!/usr/bin/env bash

for var in "$@"; do
    ./$var
    if [ $? -ne 0 ]; then
        exit 255
    fi
done

