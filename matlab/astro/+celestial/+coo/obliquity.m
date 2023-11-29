function Obl=obliquity(JulianDay,Type)
% Calculate the obliquity of the Earth ecliptic.
% Package: celestial.coo
% Description: Calculate the obliquity of ecliptic, with respect to the 
%              mean equator of date, for a given julian day.
% Input  : - Vector of Julian Days.
%          - Caqlculation type:
%            'L' ,'1976'- IAU 1976, good from 1000-3000 AD,
%            '2000' - IAU 2000
%            'H' - Laskar expression, more accurate.
%            Default is '2000'
% Output : - Obliquity of ecliptic of date in radians.
% Tested : Matlab 5.3
%     By : Eran O. Ofek                    Aug 1999
%    URL : http://weizmann.ac.il/home/eofek/matlab/
% Example: Obl=celestial.coo.obliquity(2451545+[0:1:5]).';
% Reliable: 1
%------------------------------------------------------------------------------

RADIAN = 180./pi;

if (nargin==1)
   Type = '2000';
elseif (nargin==2)
   % do nothing
else
   error('Illigal number of input arguments');
end

T   = (JulianDay - 2451545.0)./36525.0;
switch Type
    case {'L','1976'}
    
        %Obl = 23.439291 - 0.0130042.*T - 0.00000016.*T.*T + 0.000000504.*T.*T.*T;

        % IAU 1976
        %Obl = 23.4392911111111  + (-46.8150.*T -0.00059.*T.^2 + 0.001813.*T.^3)./3600;
        Obl = 23.4392911111111  + (-46.8150 + (-0.00059 + 0.001813.*T).*T).*T./3600;
    case '2000'
        % IAU 2000
        Obl = 23.4392911111111  + (-46.84024 + (-0.00059 + 0.001813.*T).*T).*T./3600;
    case 'H'
    
        U   = T./100;
        Obl = 23.44484666666667 ...
           +   (-4680.93.*U ...
              - 1.55.*U.^2 ...
              + 1999.25.*U.^3 ...
                - 51.38.*U.^4 ...
               - 249.67.*U.^5 ...
                - 39.05.*U.^6 ...
                 + 7.12.*U.^7 ...
                + 27.87.*U.^8 ...
                 + 5.79.*U.^9 ...
                 + 2.45.*U.^10)./3600;
    
 otherwise
    error('Unknown calculation type in obliquity.m');  
end
  
Obl = Obl./RADIAN;
