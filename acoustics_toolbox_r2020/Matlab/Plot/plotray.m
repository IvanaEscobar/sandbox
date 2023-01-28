function varargout = plotray( rayfil, titleStr, savefig )

% Plot the RAYfil produced by Bellhop or Bellhop3D
% usage: plotray( rayfil )
% where rayfil is the ray file (extension is optional)
% e.g. plotray( 'foofoo' )
%
% for BELLHOP3D files, rays in (x,y,z) are converted to (r,z) coordinates
%
% MBP July 1999

global units jkpsflag

if ( strcmp( rayfil, 'RAYFIL' ) == 0 && ~contains( rayfil, '.ray' ) )
   rayfil = [ rayfil '.ray' ]; % append extension
end
% plots a BELLHOP ray file

fid = fopen( rayfil, 'r' );   % open the file
if ( fid == -1 )
   disp( rayfil );
   error( 'No ray file exists; you must run BELLHOP first (with ray ouput selected)' );
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

set( gca, 'YDir', 'Reverse' )   % plot with depth-axis positive down

xlabel( 'Range (m)' )
units = 'km';
if ( strcmp( units, 'km' ) )
   xlabel( 'Range (km)' )
end
ylabel( 'Depth (m)' )
hold on

% axis limits
rmin = +1e9;
rmax = -1e9;

zmin = +1e9;
zmax = -1e9;

dalpha = 100;
alpha0 = 200;

% this could be changed to a forever loop
for isz = 1 : Nsz
   for ibeam = 1 : Nalpha
      alphaOld = alpha0;
      alpha0    = fscanf( fid, '%f', 1 );
      %fprintf('Angle is %d\n', alpha0);
      nsteps    = fscanf( fid, '%i', 1 );
      %fprintf('nsteps is %i\n', nsteps);
      
      NumTopBnc = fscanf( fid, '%i', 1 );
      NumBotBnc = fscanf( fid, '%i', 1 );

%       if ( isempty( nsteps ) || ibeam == Nalpha )
%           fprintf('Eigenray: # of rays <= Nalpha\nRay Count: %d\n', ibeam-1 );
%           title( strcat('No. of rays = ', num2str(Nalpha), '; No. of eigenrays = ', num2str(ibeam)) );
%           break; 
%       end

      %find delta alpha
      if ibeam == 2
          dalpha = abs(alphaOld - alpha0);
      end

      switch Type
         case 'rz'
            ray = fscanf( fid, '%f', [2 nsteps] );
            
            r = ray( 1, : );
            z = ray( 2, : );
         case 'xyz'
            ray = fscanf( fid, '%f', [3 nsteps] );
            
            xs = ray( 1, 1 );
            ys = ray( 2, 1 );
            r = sqrt( ( ray( 1, : ) - xs ).^2 + ( ray( 2, : ) - ys ).^2 );
            z = ray( 3, : );
      end
      
      if ( strcmp( units, 'km' ) )
         r = r / 1000;   % convert to km
      end
      
      if ~(mod(ibeam,10)==0)
          continue;
      end

      if NumTopBnc >= 1 && NumBotBnc >= 1
         ll = plot( r, z, 'k-' );    % hits both boundaries
         ll.Color(4) = 0.5;
      elseif NumBotBnc >= 1
         plot( r, z, 'b-' )	   % hits bottom only
      elseif NumTopBnc >= 1
         plot( r, z, 'g-' )	   % hits surface only
      else
         plot( r, z, 'r-' )    % direct path
      end
      
      % update axis limits
      rmin = min( [ r rmin ] );
      rmax = max( [ r rmax ] );

      zmin = min( [ z zmin ] );
      zmax = max( [ z zmax ] );
      if ( zmin == zmax ) % horizontal ray causes axis scaling problem
         zmax = zmin + 1;
      end
      axis( [ rmin, rmax, zmin, zmax ] )
      
      % flush graphics buffer every 10th ray
      % (does not work for an eigenray run because Nalpha is number of rays
      % traced, not number of eigenrays)
      if rem( ibeam, fix( Nalpha / 10 ) ) == 0
         drawnow
      end
   end	% next beam
end % next source depth

fclose( fid );

drawnow

title(titleStr);
hold off
zoom on

if ( nargout == 1 )
   varargout{ 1 } = findobj( 'Type', 'Line' );   % return a handle to the lines in the figure
end

% fixed size for publications
if ( jkpsflag )
   set( gca, 'Units', 'centimeters' )
   set( gca, 'Position', [ 2 2 14.0  7.0 ] )
   set(gcf, 'PaperPositionMode', 'auto');
   
   set( gcf, 'Units', 'centimeters' )
   set( gcf, 'Position', [ 3 15 19.0 10.0 ] )
end
set(gcf,"Position", [10, 10, 1600, 650]);
set(gca, 'FontSize', 20)

if savefig
    saveas(gcf, [rayfil '_ray-demo.png'])
end
