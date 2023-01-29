function plotssp2D( FileRoot, titleStr, savefig )
% plotssp.m
% Plots the sound speed profile
% Accesses Fileroot.env and FileRoot.env

global units jkpsflag

sspfil = [ FileRoot '.ssp' ];
envfil = [ FileRoot '.env' ]; % append extension


% set labels in m or km
xlab     = 'Range (m)';
% if ( max( rt ) >= 10000 )
%    rt      = rt / 1000.0;
%    xlab    = 'Range (km)';
% end

[ pltitle, ~, SSP, ~, fid ] = read_env( envfil, 'BELLHOP' ); % read in the environmental file

%hh = plot( real( SSP.c ), SSP.z );

%%

% Read the SSPFIL
[ cmat, rProf, NProf, NSSP ] = readssp2d( sspfil );

% set labels in m or km
if ( strcmp( units, 'km' ) )
    xlab  = 'Range (km)';
else
    xlab  = 'Range (m)';
    rProf = 1000 * rProf;
end

%%
%cmocean colormap requires repo
cmap = cmocean('speed');

figure;
%imagesc( rProf, SSP.z, cmat );   % imagesc produces a better PostScript file, using PostScript fonts
pcolor( rProf, SSP.z, cmat );  ...
   shading interp; colormap( cmap );
c=colorbar('YDir', 'Reverse', 'Location', 'westoutside');
c.Label.String = 'SSP (m/s)';
set( gca, 'YDir', 'Reverse' )   % because view messes up the zoom feature
xlabel( xlab )
ylabel( 'Depth (m)' );
%title( deblank( pltitle ) )
title(titleStr)

% set up axis lengths for publication
if ( jkpsflag )
    set( gcf, 'Units', 'centimeters' )
    set( gca, 'ActivePositionProperty', 'Position', 'Units', 'centimeters' )
    
    set( gca, 'Position', [ 2 2 14.0  7.0 ] )
    %set( gcf, 'PaperPosition', [ 3 3 19.0 10.0 ] )
end
set(gcf, "Position", [10, 10, 850, 650]);
set(gca, 'FontSize', 20)

if savefig
    saveas(gcf, [pltitle '_ssp-demo.png'])
end

% % vertical slices
% figure;
% 
% iskip = round( NProf / 10 );
% iskip = max( 1, iskip );
% 
% iplot = 1;
% NPlots = round( NProf / iskip );
% for iplot = 1 : NPlots
%    iprof = 1 + ( iplot - 1 ) * iskip;
%    subplot( 1, NPlots, iplot )
%    hh = plot( cmat( :, iprof ), SSP.z );
%    set( gca, 'YDir', 'Reverse' )
%    
%    if ( iprof == 1 )
%       xlabel( 'Sound Speed (m/s)' )
%       ylabel( 'Depth (m)' )
%    end
%    
%    set( hh, 'LineWidth', 2 );
%    % axis( [ 1490 1540 0 5000 ] )
%    
%    if ( iprof > 1 )
%       set(gca,'xtick',[],'ytick',[])
%    end
% 
% end
% 
% 
% 
