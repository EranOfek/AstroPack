function [Mag, Info] = satelliteMag(Args)
% Estimate satellite apparent magnitude of a satellite and trail dilution.
% Description: Estimate the apparent V-band magnitude of a satellite
%              reflecting sunlight, assuming a spherical Lambertian
%              reflector. Also calculate the FWHM crossing time, diluted
%              magnitude in one FWHM element, and read-noise dominated S/N.
% Input  : - * ...,key,val,...
%              'Albedo'        - Effective albedo. Default is 0.1.
%              'Radius'        - Effective radius [m]. Default is 1.
%              'Dist'          - Observer-satellite distance [m].
%                            Default is 1000e3.
%              'PhaseAngle'    - Sun-satellite-observer phase angle [rad].
%                            0 is full phase. Default is 0.
%              'SunSatDistAU'  - Sun-satellite distance [AU]. Default is 1.
%              'FWHM'          - Image FWHM [arcsec]. Default is 2.5.
%              'ProjVel'       - Projected velocity on sky [km/s].
%                            Default is 7.7.
%              'ExpTime'       - Exposure time [s]. Default is 30.
%              'ZP'            - Photometric zero point [mag], defined as the
%                            magnitude giving 1 e-/s. Default is 22.
%              'RN'            - Read noise [e-]. Default is 5.
% Output : - Instantaneous apparent V-band magnitude.
%          - Structure with fields:
%            .SunMag            - Adopted solar V magnitude.
%            .PhaseFunction     - Lambertian phase function.
%            .FluxRatio         - Satellite/Sun flux ratio.
%            .FWHMrad           - FWHM [rad].
%            .ProjVelMS         - Projected velocity [m/s].
%            .AngVelRadSec      - Angular velocity [rad/s].
%            .AngVelArcsecSec   - Angular velocity [arcsec/s].
%            .CrossTime         - Time to cross one FWHM [s].
%            .DilutionFactor    - min(CrossTime/ExpTime,1).
%            .Mag               - Same as first output.
%            .MagDiluted        - Magnitude diluted over ExpTime in one
%                                 FWHM element.
%            .ElectronsPerSec   - Satellite count rate [e-/s].
%            .ElectronsCross    - Electrons collected during CrossTime.
%            .SN                - Read-noise dominated S/N during CrossTime.
%            Also includes all input arguments.
% Author : ChatGPT + Eran Ofek (2026)
% Example: [Mag, Info] = celestial.earth.satelliteMag;
%          [Mag, Info] = celestial.earth.satelliteMag(Albedo=0.2, Radius=1.5, Dist=800e3);



arguments
    Args.Albedo        (1,1) double = 0.1
    Args.Radius        (1,1) double = 1.0
    Args.Dist          (1,1) double = 1000e3
    Args.PhaseAngle    (1,1) double = 0.0
    Args.SunSatDistAU  (1,1) double = 1.0
    Args.FWHM          (1,1) double = 2.5
    Args.ProjVel       (1,1) double = 7.7
    Args.ExpTime       (1,1) double = 30.0
    Args.ZP            (1,1) double = 22.0
    Args.RN            (1,1) double = 3.5
end

SunMag = -26.74;   % apparent V-band magnitude of the Sun

% Lambertian phase function
Phi = (sin(Args.PhaseAngle) + ...
      (pi - Args.PhaseAngle).*cos(Args.PhaseAngle)) ./ pi;
Phi = max(Phi, 0);

% Instantaneous satellite-to-Sun flux ratio
FluxRatio = Args.Albedo .* Phi .* Args.Radius.^2 ./ ...
            (4 .* Args.Dist.^2 .* Args.SunSatDistAU.^2);

% Instantaneous apparent magnitude
Mag = SunMag - 2.5 .* log10(FluxRatio);

% Crossing time
FWHMrad = Args.FWHM ./ 206265;
ProjVelMS = Args.ProjVel .* 1000;

AngVelRadSec = ProjVelMS ./ Args.Dist;
AngVelArcsecSec = AngVelRadSec .* 206265;

CrossTime = FWHMrad ./ AngVelRadSec;

% Exposure-time dilution
DilutionFactor = min(CrossTime ./ Args.ExpTime, 1);
MagDiluted = Mag - 2.5 .* log10(DilutionFactor);

% Electrons and read-noise dominated S/N during crossing time
ElectronsPerSec = 10.^(0.4 .* (Args.ZP - Mag));
ElectronsCross  = ElectronsPerSec .* CrossTime;
SN              = ElectronsCross ./ Args.RN;

% Output structure
Info = Args;
Info.SunMag = SunMag;
Info.PhaseFunction = Phi;
Info.FluxRatio = FluxRatio;
Info.FWHMrad = FWHMrad;
Info.ProjVelMS = ProjVelMS;
Info.AngVelRadSec = AngVelRadSec;
Info.AngVelArcsecSec = AngVelArcsecSec;
Info.CrossTime = CrossTime;
Info.DilutionFactor = DilutionFactor;
Info.Mag = Mag;
Info.MagDiluted = MagDiluted;
Info.ElectronsPerSec = ElectronsPerSec;
Info.ElectronsCross = ElectronsCross;
Info.SN = SN;

end
