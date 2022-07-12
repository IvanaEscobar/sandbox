clear all, close all

addpath /home/orodrig/FORdoc/at/Matlab/Plot
addpath /home/orodrig/FORdoc/at/Matlab/ReadWrite

disp('Bellhop - Topgulf, Range indep.:')
disp('Ray trace run')
disp('Geometric hat beams in Cartesian coordinates') 

fid = fopen('Gulf_ray_ri.bty','r');
s = fscanf(fid,'%s\n',[1 1]);
n = fscanf(fid,'%d\n',[1 1]);
data = fscanf(fid,'%f %f\n',[2 n]);
fclose(fid);

rbtykm = data(1,:); rbty = rbtykm*1000; 
zbty   = data(2,:);

system('bellhop.exe Gulf_ray_ri');

plotray('Gulf_ray_ri')
hold on
plot(rbty,zbty,'k','LineWidth',2)
hold off

disp('done.')
