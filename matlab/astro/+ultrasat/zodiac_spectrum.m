function OutSpec=zodiac_spectrum(varargin)
% Get the Zodiac light spectrum
% Package: +ultrasat
% Description: Return the zodiac spectrum as adopted from the HST STIS
%              handbook. The high zodiacal ligh is defined where V=22.1
%              mag/arcsec^-2.
%              This is a thin wrapper around AstroSpec.zodiacSpectrum,
%              which holds the tabulated data.
% Input  : * Arbitrary number of pairs of arguments: ...,keyword,value,...
%            where keyword are one of the followings:
%            'Wave' - Vector of wavelength [Ang] in which to calculate
%                   spectrum. If empty, then use original data.
%                   Default is empty.
%            'BackType' - Background type. Default is 'zodi'.
%            'OutType'  - 'mat'|'astrospec'|'astspec'. Default is 'mat'.
%                   'astspec' returns the obsolete AstSpec class.
%            'InterpMethod' - Default is 'linear'.
% Output : - Zodiacal ligh spectrum
%            [wavelength(Ang), Flux(erg/cm^2/s/A/arcsec^2)]
% Reference: https://hst-docs.stsci.edu/display/STISIHB/6.6+Tabular+Sky+Backgrounds
%            but there is a discrepency with:
%            http://www.stsci.edu/hst/wfc3/design/documents/handbooks/currentIHB/c09_exposuretime08.html#389841
%            According to the HST help desk the STIS table should be used.
% Tested : Matlab R2014a
%     By : Eran O. Ofek                    Nov 2014
%    URL : http://weizmann.ac.il/home/eofek/matlab/
% Example: S=ultrasat.zodiac_spectrum;
%          % to verify normalization: synphot(Spec,'Johnson','V','Vega')
%          S=ultrasat.zodiac_spectrum('OutType','astrospec');
% See also: AstroSpec.zodiacSpectrum
% Reliable: 1
%--------------------------------------------------------------------------


DefV.Wave                 = [];
DefV.BackType             = 'zodi';   % 'zodi' | 'earthshine' | 'total' | 'all'
DefV.OutType              = 'mat';
DefV.InterpMethod         = 'linear';
InPar = InArg.populate_keyval(DefV,varargin,mfilename);

switch lower(InPar.OutType)
    case 'mat'
        OutSpec = AstroSpec.zodiacSpectrum(InPar.Wave, 'BackType',InPar.BackType, ...
                                           'InterpMethod',InPar.InterpMethod, 'OutType','mat');
    case 'astrospec'
        OutSpec = AstroSpec.zodiacSpectrum(InPar.Wave, 'BackType',InPar.BackType, ...
                                           'InterpMethod',InPar.InterpMethod, 'OutType','AstroSpec');
    case 'astspec'
        % obsolete class - kept for backward compatibility
        Mat = AstroSpec.zodiacSpectrum(InPar.Wave, 'BackType',InPar.BackType, ...
                                       'InterpMethod',InPar.InterpMethod, 'OutType','mat');
        OutSpec = AstSpec;
        OutSpec.Wave = Mat(:,1);
        OutSpec.Int  = Mat(:,2:end);
        OutSpec.WaveUnits = 'Ang';
        OutSpec.IntUnits  = 'erg*cm^-2*s^-1*Ang^-1*arcsec^-2';
        OutSpec.ObjName   = 'Zodiac spectrum';
        OutSpec.source    = 'HST STIS handbook';
        OutSpec.z         = 0;
    otherwise
        error('Unknwon OutType option');
end


end
