clear all, close all

addpath /home/orodrig/FORdoc/at/Matlab/ReadWrite

disp('Bellhop - calibration:')
disp('TL calculation') 

system('bellhop.exe calibB');

[ PlotTitle, PlotType, freq, atten, Pos, p ] = read_shd( 'calibB.shd' );

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
pcolor(rarraykm,zarray,tl), shading interp, colormap( tej ), caxis([40 90]), 
h = colorbar;
set( h, 'YDir', 'reverse' );
axis([0 rmaxkm 0 zmax])
hold off
view(0,-90)
xlabel('Range (km)')
ylabel('Depth (m)')
title('Bellhop - calibration , 12 kHz')

disp('done.')
