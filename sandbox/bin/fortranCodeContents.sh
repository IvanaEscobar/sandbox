#!/bin/bash
for f in *.f90; do
    echo "File: $f"
    grep "^[[:blank:]]*USE " $f
    grep "^[[:blank:]]*TYPE " $f
    grep "^[[:blank:]]*SUBROUTINE" $f
done

