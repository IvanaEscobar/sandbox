clear all, close all

addpath /home/orodrig/FORdoc/at/Matlab/Plot
addpath /home/orodrig/FORdoc/at/Matlab/ReadWrite

disp('Bellhop - Topgulf, Range dep., Geometric beams:')
disp('Coherent TL calculation')
disp('Geometric hat beams in Cartesian coordinates') 

fid = fopen('GulfB_rd_geo.bty','r');
s = fscanf(fid,'%s\n',[1 1]);
n = fscanf(fid,'%d\n',[1 1]);
data = fscanf(fid,'%f %f\n',[2 n]);
fclose(fid);

rbtykm = data(1,:); rbty = rbtykm*1000; 
zbty   = data(2,:);

system('bellhop.exe GulfB_rd_geo');

[ PlotTitle, PlotType, freq, atten, Pos, p ] = read_shd( 'GulfB_rd_geo.shd' );

zs = Pos.s.depth;

rarray = Pos.r.range; rarraykm = rarray/1000;
zarray = Pos.r.depth;

rmax = max( rarray ); rmaxkm = rmax/1000;
zmax = max( zarray );

pressure = squeeze( p );

tl = -20*log10( abs( pressure ) ); 

tej = flipud( jet( 256 ) );

figure(1), hold on
plot(0,zs,'ko',0,zs,'m*','MarkerSize',16) 
pcolor(rarraykm,zarray,tl), shading interp, colormap( tej ), %caxis([40 90]), 
colorbar
plot(rbtykm,zbty,'k','LineWidth',2)
axis([0 rmaxkm 0 zmax])
hold off
view(0,-90)
xlabel('Range (km)')
ylabel('Depth (m)')
title('Bellhop - Topgulf, Range dep., Geometric beams')

disp('done.')
