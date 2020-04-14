#!/bin/bash

if [[ ! -f test.file ]]; then
    echo "no test.file"
    touch test.file
else
    echo "yes test.file"
fi
