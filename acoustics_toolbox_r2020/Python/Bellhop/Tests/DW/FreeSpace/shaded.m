clear all, close all

addpath /home/orodrig/FORdoc/at/Matlab/ReadWrite

disp('Bellhop - Point source in free space:')
disp('using source beam pattern file')

fid = fopen('shaded.sbp','r');
n = fscanf(fid,'%d\n',[1 1]);
data = fscanf(fid,'%f %f\n',[2 n]);
fclose(fid);

angles  = data(1,:); 
powerdB = data(2,:);

figure(1)
polar(angles*pi/180,powerdB)
grid on, box on

system('bellhop.exe shaded');

[ PlotTitle, PlotType, freq, atten, Pos, p ] = read_shd( 'shaded.shd' );

zs = Pos.s.depth;

rarray = Pos.r.range; rarraykm = rarray/1000;
zarray = Pos.r.depth;

rmax = max( rarray ); rmaxkm = rmax/1000;
zmax = max( zarray );

pressure = squeeze( p );

tl = -20*log10( abs( pressure ) ); 

tej = flipud( jet( 256 ) );

figure(2), hold on
plot(0,zs,'ko',0,zs,'m*','MarkerSize',16) 
pcolor(rarraykm,zarray,tl), shading interp, colormap( tej ), caxis([40 90])
h = colorbar;
set( h, 'YDir', 'reverse' );
axis([0 rmaxkm 0 zmax])
hold off
view(0,-90)
xlabel('Range (km)')
ylabel('Depth (m)')
title('Bellhop - Point source in free space, 100 Hz')

disp('done.')
