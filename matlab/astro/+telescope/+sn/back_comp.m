function [BackSpec]=back_comp(varargin)
% Get spectra of background components
% Package: telescope
% Description: Get spectra of background components for S/N calculation.
%              All the components are in [erg/cm^2/s/Ang/arcsec^2].
%              The background components are: 
%              zodi, cerenkov, host galaxy, and sky
% Input  : * Arbitrary number of pairs of arguments: ...,keyword,value,...
%            where keyword are one of the followings:
%            'PsfEffAreaAS' - A mandatory parameter indicating the
%                             effective area of the PSF 4*pi*(FWHM/2.35)^2.
%                             Default is Inf. Inf will behave like no host
%                             galaxy contribution to the background.
%            'ZodiMagV'     - 
% Output : - An AstSpec object in which each element corresponds to
%            a background components.
% License: GNU general public license version 3
%     By : Eran O. Ofek                    Nov 2019
%    URL : http://weizmann.ac.il/home/eofek/matlab/
% Example: [BackSpec]=telescope.sn.back_comp
% Reliable: 
%--------------------------------------------------------------------------
RAD = 180./pi;

DefV.PsfEffAreaAS         = Inf;
DefV.ZodiMagV             =  [];%23.3;   % V mag per arcsec^2 (low zodi). if empty, uses Zodi for RA,Dec,Date
DefV.CernekovMaterial     = 'si02_suprasil_2a';  % or false
DefV.CerenkovFlux         = 'DailyMax_75flux';
DefV.CerenkovSupp         = 6;
DefV.HostType             = 'Gal_Sc.txt';
DefV.HostMag              = 25;
DefV.HostFilterFamily     = 'SDSS';
DefV.HostFilter           = 'r';
DefV.HostMagSys           = 'AB';
DefV.SkyMag               = 35;
DefV.SkyFilterFamily      = 'SDSS';
DefV.SkyFilter            = 'g';
DefV.SkyMagSys            = 'AB';
DefV.RA                   = 220./RAD; %for zodiac_bck calc
DefV.Dec                  = 66./RAD; %for zodiac_bck calc
DefV.Date                 = [21 12 2024]; %for zodiac_bck calc


DefV.Wave                 = (1000:10:25000).';  % [ang]

InPar = InArg.populate_keyval(DefV,varargin,mfilename);



BackSpec = AstroSpec(4);

% Zodiac background
[Spec]=ultrasat.zodiac_bck(InPar.RA,InPar.Dec,InPar.Date,'Wave',InPar.Wave);
if ~isempty(InPar.ZodiMagV) %calibrate to ZodiMagV, if given
    Mag   = astro.spec.synthetic_phot([Spec.Wave, Spec.Spec],'Johnson','V','Vega');
    Spec.Spec = Spec.Spec.*10.^(0.4.*(Mag - InPar.ZodiMagV));
end
%InPar.BackSpec = [Spec.Wave,Spec.Spec];
BackSpec(1) = AstroSpec({[Spec.Wave, Spec.Spec]});

% Cerenkov background 
if ~islogical(InPar.CernekovMaterial)
    [Res,~]=ultrasat.Cerenkov('FluxOption',InPar.CerenkovFlux);
    BackSpec(2) = AstroSpec({[Res.Lam(:), Res.IntFA(:)./InPar.CerenkovSupp]});
else
    % set Cerenkov spectrum to zero
    BackSpec(2) = AstroSpec({[InPar.Wave, zeros(size(InPar.Wave))]});
end

% Host galaxy background
SG = AstroSpec.specGalQSO(InPar.HostType);
% extrapolate host spectrum to the IR
SGwave = [SG.Wave;  25000];
SGflux = [SG.Flux;  SG.Flux(end)];
FactorHost = 10.^(-0.4.*(InPar.HostMag - astro.spec.synthetic_phot([SGwave, SGflux],'SDSS','r','AB')));
SGflux = SGflux.*FactorHost;
%synphot(SG,'SDSS','r','AB');
SGflux = SGflux./InPar.PsfEffAreaAS;  % surface brightness
BackSpec(3) = AstroSpec({[SGwave, SGflux]});

% Sky background
BackSpec(4) = AstroSpec({[InPar.Wave, convert.flux(InPar.SkyMag,'AB','cgs/A',InPar.Wave,'A')]});
