clear all, close all

addpath /home/orodrig/FORdoc/at/Matlab/ReadWrite

disp('Bellhop - Arctic profile') 

system('bellhop.exe arcticB_gb');

[ PlotTitle, PlotType, freq, atten, Pos, p ] = read_shd( 'arcticB_gb.shd' );

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
pcolor(rarraykm,zarray,tl), shading interp, colormap( tej ), caxis([40 90]), colorbar
axis([0 rmaxkm 0 zmax])
hold off
view(0,-90)
xlabel('Range (km)')
ylabel('Depth (m)')
title('Bellhop - Arctic profile, 200 Hz')

disp('done.')
