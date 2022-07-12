clear all, close all

addpath /home/orodrig/FORdoc/at/Matlab/Plot
addpath /home/orodrig/FORdoc/at/Matlab/ReadWrite

disp('Bellhop - Dickins Seamount:') 
disp('Ray trace run') 
disp('Geometric hat beams in Cartesian coordinates') 

fid = fopen('DickinsBray.bty','r');
s = fscanf(fid,'%s\n',[1 1]);
n = fscanf(fid,'%d\n',[1 1]);
data = fscanf(fid,'%f %f\n',[2 n]);
fclose(fid);

rbtykm = data(1,:); rbty = rbtykm*1000; 
zbty   = data(2,:);

system('bellhop.exe DickinsBray');

plotray('DickinsBray')
hold on
plot(rbtykm,zbty,'k','LineWidth',2)
hold off

disp('done.')
