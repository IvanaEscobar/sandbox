#!/bin/bash
staf -toplevel f -input x -output y -reverse -arglist f.f90

# remove the second program...
# Exit as soon as the first instance of "program" is encountered
awk '/program/ { exit } { print }' "f_ad.f90" > "f_ad.tmp"
mv f_ad.tmp f_ad.f90

# compile the code
gfortran -o tafme_ad drive_f_ad.f90 f_ad.f90

echo 'Input: a, i'
./tafme_ad
