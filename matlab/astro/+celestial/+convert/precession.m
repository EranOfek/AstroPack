function [ZetaA,ZA,ThetaA]=precession(JD, Type)
% Calculate the Earth precession parameters
% Package: celestial.coo
% Description: Calculate the Earth precssion parameters as a function of JD.
% Input  : - JD
%          - Type of precession:
%               '1976' - IAU 1976
%               '2000' - IAU 2000
%            Default is '2000'.
% Output : - ZetaA [radians].
%          - ZA [radians]
%          - ThetaA [radians]
% Tested : Matlab 5.3
% Author : Eran Ofek (Nov 2016)
% Example: [ZetaA,ZA,ThetaA]=celestial.convert.precession(2451545+[0:1:5]');
% Reliable: 1

arguments
    JD
    Type    = '2000';
end

InvRAD = pi./180;
T = (JD - 2451545.0)./36525.0;
 
switch Type
    case '1976'
        ZetaA  = 0.6406161.*T + 0.0000839.*T.*T + 0.0000050.*T.*T.*T;
        ZA     = 0.6406161.*T + 0.0003041.*T.*T + 0.0000051.*T.*T.*T;
        ThetaA = 0.5567530.*T - 0.0001185.*T.*T - 0.0000116.*T.*T.*T;
        ZetaA  = ZetaA.*InvRAD;
        ZA     = ZA.*InvRAD;
        ThetaA = ThetaA.*InvRAD;
    case '2000'
        % For an alternative (better) representation see Capitaine et al. 2003
        % https://www.aanda.org/articles/aa/pdf/2003/48/aa4068.pdf
        
        % Expressions in arcseconds:
        ZetaA   = 2.5976176 + 2306.0809506.*T + 0.3019015.*T.^2 + ...
                  0.0179663.*T.^3 - 0.0000327.*T.^4 - 0.0000002.*T.^5;
        ZA      = -2.5976176 + 2306.0803226.*T + 1.0947790.*T.^2 + ...
                   0.0182273.*T.^3 + 0.0000470.*T.^4 - 0.0000003.*T.^5;
        ThetaA  = 2004.1917476.*T - 0.4269353.*T.^2 - 0.0418251.*T.^3 - ...
                  0.0000601.*T.^4 - 0.0000001.*T.^5;
              
        ZetaA   = ZetaA.*InvRAD./3600;
        ZA      = ZA.*InvRAD./3600;
        ThetaA  = ThetaA.*InvRAD./3600;
    otherwise
        error('Unknown precession Type option');
end
