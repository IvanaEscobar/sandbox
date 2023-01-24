function varargout = plotvarANDray( filroot, vartail, alphaArr )

% Plot the RAYfil produced by Bellhop or Bellhop3D
% usage: plotvarANDray( filroot, 'q', [-15, 9] )
% where rayfil is the ray file (extension is optional)
% e.g. plotscalar( 'foofoo', 'endtype', array )
%
% for BELLHOP3D files, rays in (x,y,z) are converted to (r,z) coordinates
%
% IE January 2023

global units jkpsflag

if ( strcmp( filroot, 'RAYFIL' ) == 0 && ~contains( filroot, '.ray' ) )
  rayfil = [ filroot '.ray' ]; % append extension
end

fid = fopen( rayfil, 'r' );   % open the file
if ( fid == -1 )
   disp( rayfil );
   error( 'No ray file exists; you must run BELLHOP first (with ray ouput selected)' );
end

if ( strcmp( filroot, 'VARFIL' ) == 0 && ~contains( filroot, vartail ) )
  varfil = [ filroot '.' vartail ]; % append extension
end

fid1 = fopen( varfil, 'r' );   % open the file
if ( fid1 == -1 )
   disp( varfil );
   error( 'No var file exists; you must run BELLHOP first (with var ouput selected)' );
end

% skip header of varfil
for i = 1:7
    fgetl( fid1 );
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
%Type  = deblank( Type );  % remove whitespace

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

vmin = +1e9;
vmax = -1e9;

dalpha = 100;
alpha0 = 200;

% this could be changed to a forever loop
for isz = 1 : Nsz
   for ibeam = 1 : Nalpha
      alphaOld = alpha0;
      alpha0    = fscanf( fid, '%f', 1 );
      fprintf('Angle is %d\n', alpha0);
      nsteps    = fscanf( fid, '%i', 1 );
      
      NumTopBnc = fscanf( fid, '%i', 1 );
      NumBotBnc = fscanf( fid, '%i', 1 );

      % skip reading angle and depth in varfil
      for i = 1:2
          fgetl( fid1 );
      end

      %print intermediate info
      if ( isempty( nsteps ) || ibeam == Nalpha )
          fprintf('Eigenray: # of rays <= Nalpha\nRay Count: %d\n', ibeam-1 );
          %title( strcat('No. of rays = ', num2str(Nalpha), '; No. of eigenrays = ', num2str(ibeam)) );
          break; 
      end
        
      %find delta alpha
      if ibeam == 2
          dalpha = alphaOld - alpha0;
      end
    
      switch Type
         case 'rz'
            ray = fscanf( fid, '%f', [2 nsteps] );
            var = fscanf( fid1,'%f', [2 nsteps] );
            
            r = ray( 1, : );
            z = ray( 2, : );
            v = var( 2, : );
         case 'xyz'
            ray = fscanf( fid, '%f', [3 nsteps] );
            var = fscanf( fid1,'%f', [3 nsteps] );
            
            xs = ray( 1, 1 );
            ys = ray( 2, 1 );
            r = sqrt( ( ray( 1, : ) - xs ).^2 + ( ray( 2, : ) - ys ).^2 );
            z = ray( 3, : );
            
            us = var( 1, 1 );
            vs = var( 2, 1 );
            v = sqrt( ( var( 1, : ) - us ).^2 + ( var( 2, : ) - vs ).^2 );
      end
      
      if ( strcmp( units, 'km' ) )
         r = r / 1000;   % convert to km
      end
      
      if ~( ismember(alpha0, alphaArr) )
          continue;
      end

      yyaxis left
      if NumTopBnc >= 1 && NumBotBnc >= 1
         plot( r, z, 'k-', 'LineWidth', 3, 'DisplayName', ['\alpha= ' num2str(alpha0)] )    % hits both boundaries
      elseif NumBotBnc >= 1
         plot( r, z, 'b-', 'LineWidth', 3, 'DisplayName', ['\alpha= ' num2str(alpha0)] )	   % hits bottom only
      elseif NumTopBnc >= 1
         plot( r, z, 'g-', 'LineWidth', 3, 'DisplayName', ['\alpha= ' num2str(alpha0)] )	   % hits surface only
      else
         plot( r, z, 'r-', 'LineWidth', 3, 'DisplayName', ['\alpha= ' num2str(alpha0)] )    % direct path
      end
      
      % update axis limits
      rmin = min( [ r rmin ] );
      rmax = max( [ r rmax ] );

      zmin = min( [ z zmin ] );
      zmax = max( [ z zmax ] );
      if ( zmin == zmax ) % horizontal ray causes axis scaling problem
         zmax = zmin + 1;
      end
      xlim([rmin, rmax]);
      ylim([zmin, zmax]);
      %axis( [ rmin, rmax, zmin, zmax ] )
      
      % update var axis limits
      vmin = min( [ v vmin ] );
      vmax = max( [ v vmax ] );

      if ( vmin == vmax ) % horizontal ray causes axis scaling problem
         vmax = vmin + 1;
      end
      
      % flush graphics buffer every 10th ray
      % (does not work for an eigenray run because Nalpha is number of rays
      % traced, not number of eigenrays)
      if rem( ibeam, fix( Nalpha / 10 ) ) == 0
         drawnow
      end

      yyaxis right
      ylim( [vmin, vmax] );
      if matches(vartail, 'q1')
         ylabel('q (1)');
      else
         ylabel(vartail);
      end
      
      if NumTopBnc >= 1 && NumBotBnc >= 1
         plot( r, v, 'k--', 'HandleVisibility','off')    % hits both boundaries
      elseif NumBotBnc >= 1
         plot( r, v, 'b--', 'HandleVisibility','off')	   % hits bottom only
      elseif NumTopBnc >= 1
         plot( r, v, 'g--', 'HandleVisibility','off')	   % hits surface only
      else
         plot( r, v, 'r--', 'HandleVisibility','off')    % direct path
      end
   end	% next beam
end % next source depth

fclose( fid ); fclose(fid1);

drawnow
title( [TITLE ': \Delta\alpha = ' num2str(dalpha)] );
lgd = legend;
lgd.Location = 'south';

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
%set(gcf,"Position", [100, 650, 1750, 420]);
%presentation figure
set(gcf, "Position", [10,10, 650,750]);
set(gca, 'FontSize', 20)

% set axis colors to black and blue
ax = gca;
ax.YAxis(1).Color = 'k';
ax.YAxis(2).Color = '#4DBEEE';
ax.YAxis(2).Direction = 'reverse';

%savefig
saveas(gcf, strcat(filroot, '.png'))
