clear all, close all

addpath /home/orodrig/FORdoc/at/Matlab/Plot
addpath /home/orodrig/FORdoc/at/Matlab/ReadWrite

disp('Bellhop - Ellipsoidal boundaries:') 
disp('Ray trace run') 
disp('Geometric gaussian beams')

fid = fopen('Ellipse.ati','r');
s = fscanf(fid,'%s\n',[1 1]);
n = fscanf(fid,'%d\n',[1 1]);
data = fscanf(fid,'%f %f\n',[2 n]);
fclose(fid);

ratikm = data(1,:); rati = ratikm*1000; 
zati   = data(2,:);

fid = fopen('Ellipse.bty','r');
s = fscanf(fid,'%s\n',[1 1]);
n = fscanf(fid,'%d\n',[1 1]);
data = fscanf(fid,'%f %f\n',[2 n]);
fclose(fid);

rbtykm = data(1,:); rbty = rbtykm*1000; 
zbty   = data(2,:);

system('bellhop.exe Ellipse');

plotray('Ellipse.ray')
hold on
plot(rati,zati,'b','LineWidth',2)
plot(rbty,zbty,'k','LineWidth',2)
hold off

disp('done.')
