clear all, close all

addpath /home/orodrig/FORdoc/at/Matlab/ReadWrite
addpath /home/orodrig/FORdoc/at/Matlab/Plot

disp('Bellhop - Munk profile:') 
disp('Ray trace run') 
disp('Geometric hat beams in Cartesian coordinates')

system('bellhop.exe MunkB_ray');

plotray('MunkB_ray')

disp('done.')
