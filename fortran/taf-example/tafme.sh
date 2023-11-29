#!/bin/bash
staf -toplevel f -input x -output y -forward -arglist f.f90

# remove the second program...
## Use awk to process the file
# Exit as soon as the first instance of "program" is encountered
awk '
    /program/ { exit }
    { print }
' "f_tl.f90" > "f_tl.f90"

# compile the code
gfortran -o tafme drive_f_tl.f90 f_tl.f90

echo 'Input: a, x'
./tafme
