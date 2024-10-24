function [Mag,Cover,F]=synthetic_phot(Spec,FilterFamily,FilterName,MagSystem,varargin)
% Synthetic photometry of spectra
% Package: astro.spec
% Description: Synthetic photometry of spectra.
%              Can calculate synthetic photometry in a single band to
%              multiple spectra.
% Input  : - Spectrum [Wave, Flux, [Flux, Flux, ...]]
%            matrix with two or more columns. The first column is the
%            wavelength, while the rest are the flux of varius spectra
%            sampled at the same wavelengths.
%          - Either:
%            Filter family name - e.g., 'SDSS'. or
%            Matrix of transmission [Wave[Ang], Transmission], or
%            AstFilter object.
%          - Filter name (relevant only if previus argument is filter
%            family).
%          - Magnitude system. 'AB'|'Vega'. Default is 'AB'.
%          * Arbitrary number of pairs of arguments: ...,keyword,value,...
%            where keyword are one of the followings:
%            'Device' - 'photon' | 'bol'. Default is 'photon'.
%            'SpecFluxUnits' - Default is 'cgs/A'.
%            'SpecWaveUnits' - Default is 'A'.
%            'ColW'          - Wavelength colum. Default is 1.
%            'ColF'          - Flux column. Default is '2end'.
%                              if '2end' then select all columns except 1.
%            'InterpMethod'  - Interpolation method. Default is 'linear'.
% Output : - Magnitude
%          - Coverage factor of the transmission integral. Scalar
%            representing the first spectrum.
%          - Flux.
% License: GNU general public license version 3
%     By : Eran O. Ofek                    Aug 2019
%    URL : http://weizmann.ac.il/home/eofek/matlab/
% Example: S=astro.get_pickles;
%          SS=AstSpec.black_body(5770,(2000:1:10000)');
%          SS.Int = SS.Int.*4.*pi.*constant.SunR.^2./(4.*pi.*(10.*constant.pc).^2);
%          [Mag,Cover,F]=astro.spec.synthetic_phot([SS.Wave, SS.Int],'SDSS','g','AB');
% Reliable: 2
%--------------------------------------------------------------------------

if (nargin<4)
    MagSystem = 'AB';
end

DefV.Device               = 'photon';  % 'bol'
DefV.SpecFluxUnits        = 'cgs/A';
DefV.SpecWaveUnits        = 'A';
DefV.ColW                 = 1;
DefV.ColF                 = '2end';
DefV.InterpMethod         = 'linear';
InPar = InArg.populate_keyval(DefV,varargin,mfilename);

if (ischar(InPar.ColF))
    switch lower(InPar.ColF)
        case '2end'
            InPar.ColF = (2:size(Spec,2));
        otherwise
            error('Unknwon ColF option');
    end
end


% get filter transmission
if (AstFilter.isAstFilter(FilterFamily))
    % Filter is AstFilter
    Tran = FilterFamily.nT;
else
    if (isnumeric(FilterFamily))
        % assume transmission matrix [Wave, Tran] is provided
        Tran = FilterFamily;
    else
        % assume filter name
        Filt = AstFilter.get(FilterFamily,FilterName);
        Tran = Filt.nT;
    end
end

% convert spectrum to Ang
switch lower(InPar.SpecWaveUnits)
    case 'a'
        % do nothing
    otherwise
        Spec(:,InPar.ColW) = convert.energy(InPar.SpecWaveUnits,'A',Spec(:,InPar.ColW));
end


Spec = sortrows(Spec,InPar.ColW);
% check if transmission curve covers entire spectrum
MinSW = Spec(1,InPar.ColW);
MaxSW = Spec(end,InPar.ColW);
MinTW = Tran(1,1);
MaxTW = Tran(end,1);

        
% choose the worst resolution
DWs = min(diff(Spec(:,InPar.ColW)));
DWt = min(diff(Tran(:,1)));

if (DWt<DWs)
    % interpolate transmission grid to spectrum grid
    Fw = Spec(:,InPar.ColW)>=MinTW & Spec(:,InPar.ColW)<=MaxTW;
    Tran = [Spec(Fw,InPar.ColW), interp1(Tran(:,1),Tran(:,2),Spec(Fw,InPar.ColW),InPar.InterpMethod)];
    Spec = Spec(Fw,:);
else
    % interpolate spectrum grid to transmission grid
    Spec = [Tran(:,1), interp1(Spec(:,InPar.ColW),Spec(:,InPar.ColF),Tran(:,1),InPar.InterpMethod)];
end
Wave = Tran(:,1);


switch lower(MagSystem)
    case 'ab'
        % convert F_lambda to F_nu
        Freq     = convert.energy('A','Hz',Wave);
        SpecFnu  = convert.flux(Spec(:,InPar.ColF),InPar.SpecFluxUnits,'cgs/Hz',Wave,'A');
        
        NN = ~isnan(SpecFnu(:,1));  % coverage off all spec must be identical
        
        switch lower(InPar.Device)
            case 'bol'
                NormTran = trapz(Freq,Tran(:,2));
                NormTranNN = trapz(Freq(NN),Tran(NN,2));
                Fnu      = trapz(Freq(NN),SpecFnu(NN,:).*Tran(NN,2))./NormTranNN;
            case'photon'
                NormTran = trapz(Freq,Tran(:,2)./Freq);
                NormTranNN = trapz(Freq(NN),Tran(NN,2)./Freq(NN));
                Fnu      = trapz(Freq(NN),SpecFnu(NN,:).*Tran(NN,2)./Freq(NN))./NormTran;
            otherwise
                error('Unknown Device option');
        end
        Cover    = NormTranNN./NormTran;
        Mag      = -48.6 - 2.5.*log10(Fnu);
        F        = Fnu;
        
    case 'vega'
        
        SpecFlam = Spec(:,InPar.ColF);
        NN = ~isnan(SpecFlam(:,1));
        hc = constant.h.*constant.c;
        
        io.files.load1('vega_spec.mat');
        switch lower(InPar.Device)
            case 'bol'
                VegaF   = interp1(vega_spec(:,1),vega_spec(:,2),Wave,InPar.InterpMethod);
                NormTran = trapz(Wave,Tran(:,2));
                NormTranNN = trapz(Wave(NN),Tran(NN,2));
                Fvega   = trapz(Wave(NN),Spec(NN,InPar.ColF).*Tran(NN,2)./(VegaF(NN).*NormTranNN));
            case 'photon'
                VegaF   = interp1(vega_spec(:,1),vega_spec(:,2),Wave,InPar.InterpMethod);
                NormTran = trapz(Wave,Tran(:,2).*Wave);
                NormTranNN = trapz(Wave(NN),Tran(NN,2).*Wave(NN));
                Fvega   = trapz(Wave(NN),Spec(NN,InPar.ColF).*Tran(NN,2).*Wave(NN)./(VegaF(NN).*NormTranNN));
                
                
            otherwise
                error('Unknown Device option');           
        end
        Cover    = NormTranNN./NormTran;
        Mag      = -2.5.*log10(Fvega);
        F        = Fvega;
    otherwise
        error('Unknown MagStstem option');
end

    