clear all, close all

addpath /home/orodrig/FORdoc/at/Matlab/Plot
addpath /home/orodrig/FORdoc/at/Matlab/ReadWrite

disp('Bellhop - Dickins Seamount:') 
disp('Ray trace run')
disp('Geometric hat beams in Cartesian coordinates') 

system('bellhop.exe DickinsFlatBray');

plotray('DickinsFlatBray')

disp('done.')
