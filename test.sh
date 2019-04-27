#!/bin/bash

./build.sh

PATH="tests/*"
for file in $PATH; do
    ./Main $file
done

