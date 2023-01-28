function varargout = plottauANDdelay( filroot, info, savefig )

% Plot the VARfile produced by Bellhop or Bellhop3D
% usage: plotvar( filroot, vartail, savefig )
% where rayfil is the ray file (extension is optional)
% e.g. plotscalar( 'foofoo', 'endtype', true )
%
% for BELLHOP3D files, rays in (x,y,z) are converted to (r,z) coordinates
%
% IE Jan 2023

global units jkpsflag

if ( strcmp( filroot, 'VARFIL' ) == 0 && ~contains( filroot, 'tau' ) )
  varfil = [ filroot '.tau' ]; % append extension
end

fid = fopen( varfil, 'r' );   % open the file
if ( fid == -1 )
   disp( varfil );
   error( 'No tau file exists; you must run BELLHOP first (with var ouput selected)' );
end

if ( strcmp( filroot, 'VARFIL' ) == 0 && ~contains( filroot, 'delay' ) )
  delayfil = [ filroot '.delay' ]; % append extension
end

fid1 = fopen( delayfil, 'r' );   % open the file
if ( fid == -1 )
   disp( delayfil );
   error( 'No delay file exists; you must run BELLHOP first (with var ouput selected)' );
end

%skip header of delay file
for i=1:7
    fgetl (fid1 );
end

% read header stuff
TITLE       = fgetl(  fid );
FREQ        = fscanf( fid, '%f', 1 );
Nsxyz       = fscanf( fid, '%f', 3 );
NBeamAngles = fscanf( fid, '%i', 2 );

DEPTHT      = fscanf( fid, '%f', 1 );
DEPTHB      = fscanf( fid, '%f', 1 );

Type        = fgetl( fid );
Type        = fgetl( fid );

Nsx    = Nsxyz( 1 );
Nsy    = Nsxyz( 2 );
Nsz    = Nsxyz( 3 );

Nalpha = NBeamAngles( 1 );
Nbeta  = NBeamAngles( 2 );

% Extract letters between the quotes
nchars = strfind( TITLE, '''' );   % find quotes
TITLE  = [ TITLE( nchars( 1 ) + 1 : nchars( 2 ) - 1 ) blanks( 7 - ( nchars( 2 ) - nchars( 1 ) ) ) ];
TITLE  = erase(TITLE, "BELLHOP-");
TITLE  = deblank( TITLE );  % remove whitespace

nchars = strfind( Type, '''' );   % find quotes
Type   = Type( nchars( 1 ) + 1 : nchars( 2 ) - 1 );

% read rays
figure;

% set up axis lengths for publication
if ( jkpsflag )
  set( gcf, 'Units', 'centimeters' )
  set( gca, 'ActivePositionProperty', 'Position', 'Units', 'centimeters' )
  set( gca, 'Position', [ 2 2 14.0  7.0 ] )
  %set( gcf, 'PaperPosition', [ 3 3 19.0 10.0 ] )
end

xlabel( 'Arrival time, \tau (s)' )
ylabel( 'Launch angle, \alpha (deg)' )
hold on

% angle init
dalpha = 100;
alpha0 = 200;

% this could be changed to a forever loop
for isz = 1 : Nsz
   for ibeam = 1 : Nalpha
      alphaOld = alpha0;
      alpha0    = fscanf( fid, '%f', 1 );
      %fprintf('Angle is %d\n', alpha0);
      nsteps    = fscanf( fid, '%i', 1 );
      
      NumTopBnc = fscanf( fid, '%i', 1 );
      NumBotBnc = fscanf( fid, '%i', 1 );

      %find delta alpha
      if ibeam == 2
          dalpha = abs(alphaOld - alpha0);
      end

      switch Type
         case 'rz'
            ray = fscanf( fid, '%f', [2 nsteps] );
            
            r = ray( 1, : );
            v = ray( 2, : );
         case 'xyz'
            ray = fscanf( fid, '%f', [3 nsteps] );
            
            xs = ray( 1, 1 );
            ys = ray( 2, 1 );
            r = sqrt( ( ray( 1, : ) - xs ).^2 + ( ray( 2, : ) - ys ).^2 );
            v = ray( 3, : );
      end
      
      sz=20;
      if NumTopBnc >= 1 && NumBotBnc >= 1
         ll = scatter( v(nsteps-1), alpha0, ...
             sz, 'MarkerFaceColor', 'k' );    % hits both boundaries
      elseif NumBotBnc >= 1
         ll = scatter( v(nsteps-1), alpha0, ...
             sz, 'MarkerFaceColor', 'b' );    % hits bottom only
      elseif NumTopBnc >= 1
         ll = scatter( v(nsteps-1), alpha0, ...
             sz, 'MarkerFaceColor', 'g' );    % hits surface only
      else
         ll = scatter( v(nsteps-1), alpha0, ...
             sz, 'MarkerFaceColor', 'r' );    % direct path 
      end
      ll.MarkerFaceAlpha = 0.5;
      ll.MarkerEdgeAlpha = 0.0;
      ll.HandleVisibility = 'off';
   end	% next beam
end % next source depth

%% Plot eigenray delays over all delays
for isz = 1 : Nsz
   for ibeam = 1 : Nalpha %% break when there are no more eigenrays
      alpha0    = fscanf( fid1, '%f', 1 );
      NumTopBnc = fscanf( fid1, '%i', 1 );
      NumBotBnc = fscanf( fid1, '%i', 1 );

      switch Type
         case 'rz'
            v = fscanf( fid1, '%f', 1 );
         case 'xyz'
            v = fscanf( fid1, '%f', 1 );
      end
      
      if isempty(alpha0)
          break;
      end

      sz=55;
      if NumTopBnc >= 1 && NumBotBnc >= 1
         ll = scatter( v, alpha0, ...
             sz, 'MarkerFaceColor', 'k');    % hits both boundaries
      elseif NumBotBnc >= 1
         ll = scatter( v, alpha0, ...
             sz, 'MarkerFaceColor', 'b');    % hits bottom only
      elseif NumTopBnc >= 1
         ll = scatter( v, alpha0, ...
             sz, 'MarkerFaceColor', 'g');    % hits surface only
      else
         ll = scatter( v, alpha0, ...
             sz, 'MarkerFaceColor', 'r');    % direct path 
      end
      %ll.MarkerFaceAlpha = 0.5;
      ll.MarkerEdgeAlpha = 0.5;
      ll.DisplayName = num2str(alpha0);
   end	% next beam
end % next source depth

fclose( fid ); fclose( fid1 );

title( [TITLE ': ' info]);
lgd = legend('Location','east');
%lgd.FontSize = 16;
lgdTitle = get(lgd, 'Title');
set(lgdTitle, 'String', "\alpha = ");
hold off

% fixed size for publications
if ( jkpsflag )
  set( gca, 'Units', 'centimeters' )
  set( gca, 'Position', [ 2 2 14.0  7.0 ] )
  set(gcf, 'PaperPositionMode', 'auto');
  
  set( gcf, 'Units', 'centimeters' )
  set( gcf, 'Position', [ 3 15 19.0 10.0 ] )
end
set(gcf,"Position", [10, 10, 1200, 1000]);
set(gca, 'FontSize', 20)

%savefig
if savefig
   saveas(gcf, [filroot '-tau+delay.png'])
end
