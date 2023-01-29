function plotarrESCOb( ARRFIL, irr, ird, isd )

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
% ie Jan 2023

%%%% the volume attenuation in the imaginary part of Arr.delay has been
%%%% neglected. Should be multiplied in ....

% read

narginchk( 4, 4 )

% Narrmx = 5000;
ARRFIL = [ARRFIL, '.arr'];
[ Arr, Pos ] = read_arrivals_asc( ARRFIL );
%[ Arr, Pos ] = read_arrivals_bin( ARRFIL );

% stem plot for a single receiver
figure
hold on
Narr = Arr( irr, ird, isd ).Narr;

for i=1:Narr
    if Arr.NumTopBnc(i) >= 1 && Arr.NumBotBnc(i) >=1
        stem( real( Arr( irr, ird, isd ).delay( i ) ), ...
            abs( Arr( irr, ird, isd ).A( i ) ), ...
            'k', 'MarkerSize', 15 );
    elseif Arr.NumBotBnc(i) >=1
        stem( real( Arr( irr, ird, isd ).delay( i ) ), ...
            abs( Arr( irr, ird, isd ).A( i ) ), ...
            'b' );
    elseif Arr.NumTopBnc(i) >= 1
        stem( real( Arr( irr, ird, isd ).delay( i ) ), ...
            abs( Arr( irr, ird, isd ).A( i ) ), ...
            'g', 'MarkerSize', 15 );
    else
        stem( real( Arr( irr, ird, isd ).delay( i ) ), ...
            abs( Arr( irr, ird, isd ).A( i ) ), ...
            'r' );
    end
end

%% BD22 custom vis
text(9.2, 1.1e-4, ["NArr = " num2str(Narr)], 'FontSize', 18);
hold off;
xlim([7 10]);
%%
xlabel( 'Travel time (s)' )
ylabel( 'Amplitude' )
title( [ 'Src_z  = ', num2str( Pos.s.z( isd ) ), ...
   ' m    Rcvr_z = ', num2str( Pos.r.z( ird ) )] )

set(gcf,"Position", [10, 10, 600, 500]);
set(gca,'YAxisLocation','right');
set(gca, 'FontSize', 20);

saveas(gcf, 'nesba-tm4_BD22_arrivals.png');
% % depth-time stem plot
% figure
% for ird1 = 1 : size( Arr, 2 )
%    Narr = Arr( irr, ird1, isd ).Narr;
%    stem3( real( Arr( irr, ird1, isd ).delay( 1 : Narr ) ), Pos.r.z( ird1 ) * ones( length( Arr( irr, ird1, isd ).delay( 1 : Narr ) ), 1 ), ...
%        abs( Arr( irr, ird1, isd ).A( 1:Narr ) ) )
% hold on
% end
% hold off
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
% hold off
% xlabel( 'Time (s)' )
% ylabel( 'Range (m)' )
% title( [ 'Src_z = ', num2str( Pos.s.z( isd ) ), ' m    Rcvr_z = ', num2str( Pos.r.z( ird ) ), ' m' ] )
% 
% set(gca, 'FontSize', 20)
