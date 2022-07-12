clear all, close all

addpath /home/orodrig/FORdoc/at/Matlab/Plot

disp('Bellhop - block case:')
disp('ray calculation')

fid = fopen('blockB_ray.bty','r');
s = fscanf(fid,'%s\n',[1 1]);
n = fscanf(fid,'%d\n',[1 1]);
data = fscanf(fid,'%f %f\n',[2 n]);
fclose(fid);

rbtykm = data(1,:); rbty = rbtykm*1000; 
zbty   = data(2,:);

system('bellhop.exe blockB_ray');

plotray('blockB_ray')
hold on
plot(rbty,zbty,'k','LineWidth',2)
hold off

disp('done.')
