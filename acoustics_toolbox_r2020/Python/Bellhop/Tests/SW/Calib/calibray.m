clear all, close all

addpath /home/orodrig/FORdoc/at/Matlab/Plot

disp('Bellhop - calibration:')
disp('ray calculation') 

system('bellhop.exe calibray');

plotray('calibray')

disp('done.')
