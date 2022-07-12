clear all, close all

addpath /home/orodrig/FORdoc/at/Matlab/ReadWrite
addpath /home/orodrig/FORdoc/at/Matlab/Plot

disp('Bellhop - Munk profile:') 
disp('Eigenray trace run')
disp('Geometric hat beams in Cartesian coordinates')

system('bellhop.exe MunkB_eigenray');

plotray('MunkB_eigenray')

disp('done.')
