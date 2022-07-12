clear all, close all

addpath /home/orodrig/FORdoc/at/Matlab/Plot

disp('Bellhop - calibration:')
disp('ray calculation') 

system('bellhop.exe calibraygrad');

plotray('calibraygrad')

disp('done.')
