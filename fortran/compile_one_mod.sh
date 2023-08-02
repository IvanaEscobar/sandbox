#!/bin/bash

brew --prefix gfortran
brew info gfortran

gfortran -x f95 -c one_mod.fr9
