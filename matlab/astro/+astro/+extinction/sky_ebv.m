function Ebv = sky_ebv(RA,Dec,CooType,CorrectHigh, Args)
    % Get Galactic extinction for a list of coordinates
    % Input  : - J2000.0 RA or longitude [H M S] or [RAD] or sexagesimal.
    %          - J2000.0 Dec or latitude [Sign D M S] or [RAD] or sexagesimal.
    %          - Coordinates type:
    %            'eq' : J2000.0 equatorial (default).
    %            'g'  : Galactic.
    %            'ec'  : ecliptic.
    %          - true|false Correct Schlegel et al. E(B-V) when >0.1,
    %            using the Adams et al. (2013) correction (def. true)
    %          * ...,key,val,...
    %          'Map' - which extiction map to use: 'SFD98' (default) 'G24'(new)
    %          'Rv' - R_V, default is Galactic 3.08
    %          'InterpMethod' - interolation method, default = 'nearest neighbor'
    % Output : - E(B-V) [mag].
    % References: Schlegel, Finkbeiner & Davis (1998; ApJ 500, 525); Gontcharov et al. arXiv:2402.06474
    % Author: A.M. Krassilchtchikov (Feb 2024)
    % Example: [Ebv]=astro.extinction.sky_ebv(1,1); 
    arguments
        RA
        Dec
        CooType     = 'eq';
        CorrectHigh = true;
        Args.Map    = 'SFD98'; % by default we use the classical map of Schlegel, Finkbeiner & Davis
        Args.Rv     = 3.08;  
        Args.InterpMethod = 'nearest'; % interpolation method for the G24 map
    end
    
    if strcmpi(Args.Map,'SFD98')
        Ebv = sky_ebv_Schlegel(RA,Dec,CooType,CorrectHigh);
    elseif strcmpi(Args.Map,'G24')
        Ebv = sky_ebv_Gontcharov(RA,Dec,'CooType',CooType,'CorrectHigh',CorrectHigh,'Rv',Args.Rv,'InterpMethod',Args.InterpMethod);
    else
        error('Illegal input map type\n');
    end
    
end

function [Ebv,A]=sky_ebv_Schlegel(RA,Dec,CooType,CorrectHigh)
% Get Galactic extinction for a list of coordinates
% Package: AstroUtil.spec
%              Schlegel, Finkbeiner & Davis (1998) extinction maps.
%              Use AstroUtil.spec.extinction to calculate the extinction.
% Input  : - J2000.0 RA or longitude [H M S] or [RAD] or sexagesimal.
%          - J2000.0 Dec or latitude [Sign D M S] or [RAD] or sexagesimal.
%          - Coordinates type:
%            'eq' : J2000.0 equatorial (default).
%            'g'  : Galactic.
%            'ec'  : ecliptic.
%          - true|false Correct Schlegel et al. E(B-V) when >0.1,
%            using the Adams et al. (2013) correction.
%            I.e., at large E(B-V) values Schlegel et al. is probably
%            overestimating the extinction.
%            Default is true.
% Output : - E(B-V) [mag].
%            The E(B-V) is returned from the point in the map which is
%            the nearest to the input coordinates.
%          - [A_U, A_B, A_V, A_R, A_I, A_J, A_H, A_K] (assuming R=3.08).
% Reference: Schlegel, Finkbeiner & Davis (1998; ApJ 500, 525).
% Needed : coco.m
% Tested : Matlab 7.0
%     By : Eran O. Ofek                    May 2006
%    URL : hhtp://weizmann.ac.il/home/eofek/matlab/
% Example: [Ebv]=astro.extinction.sky_ebv(1,1);
% Reliable: 2
%--------------------------------------------------------------------------

% RAD = 180./pi;
% Rv  = 3.08;
% MapSize = 4096;
% StepSize = 10000;

% Path      = '~/matlab/data/GalacticExtinction/';
% FileSouth = 'SFD_dust_4096_sgp.fits';
% FileNorth = 'SFD_dust_4096_ngp.fits';
% 
% FileSouth = sprintf('%s%s',Path,FileSouth);
% FileNorth = sprintf('%s%s',Path,FileNorth);

FileSouth = 'SFD_dust_4096_sgp';
FileNorth = 'SFD_dust_4096_ngp';

if (nargin==2)
   CooType = 'eq';
   CorrectHigh = true;
elseif (nargin==3)
   CorrectHigh = true;
elseif (nargin==4)
   % do nothing
else
   error('Illegal number of input arguments');
end


RA  = celestial.coo.convertdms(RA,'gH','r');
Dec = celestial.coo.convertdms(Dec,'gD','r');

switch CooType
 case 'eq'
    % convert Equatorial to Galactic
    Coo = celestial.coo.coco([RA, Dec],'j2000.0','g');
 case 'g'
    % do nothing - already in Galactic
    Coo = [RA, Dec];
 case 'ec'
    % convert Ecliptic to galactic
    Coo = celestial.coo.coco([RA, Dec],'e','g');
 otherwise
    error('Unknown CooType Option');
end

L = Coo(:,1);    % Galactic longitude
B = Coo(:,2);    % Galactic latitude


%X = 2048.*sqrt(1 - sign(B).*sin(B)).*cos(L) + 2047.5;
%Y = -2048.*sign(B).*sqrt(1 - sign(B).*sin(B)).*sin(L) + 2047.5;
% correction to avoid values below 1 or above 4096...
X = 2047.5.*sqrt(1 - sign(B).*sin(B)).*cos(L) + 2048;
Y = -2047.5.*sign(B).*sqrt(1 - sign(B).*sin(B)).*sin(L) + 2048;
X = round(X);
Y = round(Y);

Ebv = zeros(size(L));

In = find(B<0);
Ip = find(B>=0);

if (isempty(In)==0)
   %Im = fitsread(FileSouth);
   Im = cats.maps.(FileSouth);
   for I=1:1:length(In)
      Ebv(In(I)) = Im(Y(In(I)),X(In(I)));
   end
end
if (isempty(Ip)==0)
   %Im = fitsread(FileNorth);
   Im = cats.maps.(FileNorth);
   for I=1:1:length(Ip)
      Ebv(Ip(I)) = Im(Y(Ip(I)),X(Ip(I)));
   end
end

% The Eb-v is probably overestimated when >0.1 mag
% Stanek 1998; Arce & Goodman 1999
% use correction from Adams et al. (2013):
if (CorrectHigh)
    Il = find(Ebv>0.1);
    Ebv(Il) = 0.1 + 0.65.*(Ebv(Il)-0.1);
end

% if (nargout>1)
%    A_U = optical_extinction(Ebv,'B','V','U','C',Rv);
%    A_B = optical_extinction(Ebv,'B','V','B','C',Rv);
%    A_V = optical_extinction(Ebv,'B','V','V','C',Rv);
%    A_R = optical_extinction(Ebv,'B','V','R','C',Rv);
%    A_I = optical_extinction(Ebv,'B','V','I','C',Rv);
%    A_J = optical_extinction(Ebv,'B','V','J','C',Rv);
%    A_H = optical_extinction(Ebv,'B','V','H','C',Rv);
%    A_K = optical_extinction(Ebv,'B','V','K','C',Rv);
% 
%    A = [A_U, A_B, A_V, A_R, A_I, A_J, A_H, A_K];
end



function Ebv = sky_ebv_Gontcharov(RA, Dec, Args) 
    % Get full Galactic extinction for a list of coordinates [Gontcharov et al. arXiv:2402.06474]
    % 
    % Input  : - J2000.0 RA or longitude [H M S] or [RAD] or sexagesimal.
    %          - J2000.0 Dec or latitude [Sign D M S] or [RAD] or sexagesimal.
    %          * ...,key,val,...
    %          'CooType' - Coordinates type:
    %                      'eq' : J2000.0 equatorial (default).
    %                      'g'  : Galactic.
    %                      'ec'  : ecliptic.
    %          'CorrectionHigh' - true (default) |false:  correct E(B-V) when > 0.1,
    %            using the Adams et al. (2013) correction 
    %          'Rv' - R_V, default is Galactic 3.08
    %          'InterpMethod' - interolation method, default = 'nearest neighbor'
    % Output : - E(B-V) [mag]
    % Reference: Gontcharov et al. arXiv:2402.06474
    % Author: A.M. Krassilchtchikov (Feb 2024)    
    arguments
        RA  = pi; % some placeholder values
        Dec = pi/2;
        Args.CooType = 'eq';
        Args.CorrectHigh = true;
        Args.Rv = 3.08;
        Args.InterpMethod = 'nearest';
    end
    
    RAD = 180/pi;
    
%     DataFile = '~/matlab/data/+cats/+maps/GMK2023_2D_AV.txt';
%     Data = readmatrix(DataFile);  

    FN = 'GMK2023_2D_AV';    
    Data = cats.maps.(FN);
    L0 = Data(:,1);
    B0 = Data(:,2);
    AV0 = Data(:,5);
    AV = scatteredInterpolant(L0,B0,AV0,Args.InterpMethod); % a bit slow, should replace by a faster method 

%     io.files.load1('Gontcharov24.mat');   % keeping AV in a .mat object isn't much faster: 9.6 sec vs. 11.2 sec

    RA  = celestial.coo.convertdms(RA,'gH','r');
    Dec = celestial.coo.convertdms(Dec,'gD','r');

    switch Args.CooType
     case 'eq'
        % convert Equatorial to Galactic        
        [Lon, Lat] = celestial.coo.convert_coo(RA,Dec,'J2000.0','g');
     case 'g'
        % do nothing - already in Galactic
        Lon = RA; Lat = Dec;
     case 'ec'
        % convert Ecliptic to Galactic
        [Lon, Lat] = elestial.coo.convert_coo(RA,Dec,'e','g'); 
     otherwise
        error('Unknown CooType Option');
    end

    L = Lon .* RAD;    % Galactic longitude in [deg]
    B = Lat .* RAD;    % Galactic latitude  in [deg]
    
    Ebv = AV(L,B) ./ Args.Rv;
        
    % The E(B-V) is probably overestimated when > 0.1 mag
    % Stanek 1998; Arce & Goodman 1999; use correction from Adams et al. (2013):
    if Args.CorrectHigh 
        Ind = Ebv > 0.1;
        Ebv(Ind) = 0.1 + 0.65.*(Ebv(Ind)-0.1);
    end
    
    % this map is for |B| > 13 only, so if |B| < 13 deg, replace the values by those of SFD98:
    Ind = abs(B) < 13; 
    if sum(Ind) > 0
        Ebv(Ind) = sky_ebv_Schlegel(Lon(Ind),Lat(Ind),'g');    
    end
    
end


