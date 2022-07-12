clear all, close all

addpath /home/orodrig/FORdoc/at/Matlab/ReadWrite
addpath /home/orodrig/FORdoc/at/Matlab/Plot

disp('Bellhop - Lower halfspace:') 
disp('TL calculation') 

system('bellhop.exe lower_half');

[ PlotTitle, PlotType, freq, atten, Pos, p ] = read_shd( 'lower_half.shd' );

zs = Pos.s.depth;

rarray = Pos.r.range; rarraykm = rarray/1000;
zarray = Pos.r.depth;

rmax = max( rarray ); rmaxkm = rmax/1000;
zmax = max( zarray );

pressure = squeeze( p );

tl = -20*log10( abs( pressure ) ); 

figure(1), hold on
plot(rarraykm,tl)
view(0,-90)
xlabel('Range (km)')
ylabel('TL (dB)')
title('Bellhop - Lower halfspace')
grid on, box on

disp('done.')
