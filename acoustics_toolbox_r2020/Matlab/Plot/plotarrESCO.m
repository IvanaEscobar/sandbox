function plotarrESCO( ARRFIL, irr, ird, isd )

% plot the arrivals calculated by BELLHOP
%
% usage:
% plotarr( filename, irr, ird, isd )
% where:
% irr = index of receiver range
% ird = index of receiver depth
% isd = index of source   depth
%
% mbp, April 2009

%%%% the volume attenuation in the imaginary part of Arr.deloy has been
%%%% neglected. Should be multiplied in ....

% read

narginchk( 4, 4 )

ARRFIL = [ARRFIL, '.arr'];
[ Arr, Pos ] = read_arrivals_asc( ARRFIL );
%[ Arr, Pos ] = read_arrivals_bin( ARRFIL );

% stem plot for a single receiver
figure
Narr = Arr( irr, ird, isd ).Narr;
% stem( real( Arr( irr, ird, isd ).delay( 1 : Narr ) ), abs( Arr( irr, ird, isd ).A( 1 : Narr ) ) )

[x, idsort] = sort( real( Arr( irr, ird, isd ).delay( 1 : Narr ) ) );
C = -20*log10(abs(Arr( irr, ird, isd ).A( 1 : Narr ))); C = C(idsort);
imagesc(x, 1, C);
c = colorbar;
c.Label.String = "Amplitude [dB]";
yticks([]);

%% to match BD22 
xlim([7.8 8.82]);
clim([90 130]);

set(gcf,"Position", [100, 650, 1750, 420]);

xlabel( 'Time (s)' )
%ylabel( 'Amplitude' )
title( [ 'Src_z  = ', num2str( Pos.s.z( isd ) ), ...
   ' m    Rcvr_z = ', num2str( Pos.r.z( ird ) ), ...
   ' m    Rcvr_r = ', num2str( Pos.r.r( irr ) ), ...
   ' m    Narr   = ', num2str( Narr ) ] )
set(gca, 'FontSize', 20)



% % depth-time stem plot
% figure
% for ird1 = 1 : size( Arr, 2 )
%    Narr = Arr( irr, ird1, isd ).Narr;
%    stem3( real( Arr( irr, ird1, isd ).delay( 1 : Narr ) ), Pos.r.z( ird1 ) * ones( length( Arr( irr, ird1, isd ).delay( 1 : Narr ) ), 1 ), ...
%        abs( Arr( irr, ird1, isd ).A( 1:Narr ) ) )
% hold on
% end
% 
% xlabel( 'Time (s)' )
% ylabel( 'Depth (m)' )
% title( [ 'Src_z = ', num2str( Pos.s.z( isd ) ), ' m    Rcvr_r = ', num2str( Pos.r.r( irr ) ), ' m' ] )
% set(gca, 'FontSize', 20)
% 
% % range-time stem plot
% figure
% for irr1 = 1 : size( Arr, 1 )
%    Narr = Arr( irr1, ird, isd ).Narr;
%    stem3( real( Arr( irr1, ird, isd ).delay( 1 : Narr ) ), Pos.r.r( irr1 ) * ones( length( Arr( irr1, ird, isd ).delay( 1 : Narr ) ), 1 ), ...
%        abs( Arr( irr1, ird, isd ).A( 1 : Narr ) ) )
% hold on
% end
% 
% xlabel( 'Time (s)' )
% ylabel( 'Range (m)' )
% title( [ 'Src_z = ', num2str( Pos.s.z( isd ) ), ' m    Rcvr_z = ', num2str( Pos.r.z( ird ) ), ' m' ] )
% 
% set(gca, 'FontSize', 20)