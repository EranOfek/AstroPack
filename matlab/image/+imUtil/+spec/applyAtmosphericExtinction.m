function ExtMatrix = applyAtmosphericExtinction(Wave, Matrix, Args)
% Apply extinction to spectrum
% Input  : - A vector of wavelength [Ang].
%          - A matrix of spectrum (wavelength axis shuld be along the same
%            dimension as the vector of wavelength).
%          * ...,key,val,...
%            'AirMass'   - Airmass of requested output spectrum.
%                       Default is 0.
%            'InAirMass' - Airmass of reference extinction curve.
%                       Default is 1.
%            'AtmosphericExt' - Either a two column matrix of
%                       [Wave[Ang], Extinction[mag]], or a string
%                       indicating the extinction curve to load from:
%                       cats.spec.AtmoExtinction
%                       Default is 'VLT'.
%            'InterpMethod' - Interpolation method. Default is 'linear'.
% Output : - Matrix of spectrum after applying the extinction correction.
% Author : Eran Ofek (Mar 2021)
% Example: ExtMatrix = imUtil.spec.applyAtmosphericExtinction((4001:1:5000)', ones(1000,3))
%          ExtMatrix = imUtil.spec.applyAtmosphericExtinction((4001:100:5000)', ones(10,3),'AirMass',2)

arguments
    Wave           {mustBeNumeric(Wave)}                    % Ang
    Matrix         {mustBeNumeric(Matrix)}                  % Flux or counts units
    Args.AirMass   {mustBeNumeric(Args.AirMass)}   = 0;     % Output airmass
    Args.InAirMass {mustBeNumeric(Args.InAirMass)} = 1;     % airmass of extinction grid
    Args.AtmosphericExt                            = 'VLT'; % or [Wave(Wave), Extinction(Mag)] @ AM=1
    Args.InterpMethod                              = 'linear';
end


if ischar(Args.AtmosphericExt)
    AtmoExt = cats.spec.AtmoExtinction.(Args.AtmosphericExt);
elseif isnumeric(Args.AtmosphericExt) && size(rgs.AtmosphericExt,2)>1
    AtmoExt = Args.AtmosphericExt;
else
    error('Unknown AtmosphericExt option');
end

% interpolate atmospheric extinction to wave grid
IntAtmoExt = interp1(AtmoExt(:,1), AtmoExt(:,2), Wave, Args.InterpMethod);
IntAtmoExt = IntAtmoExt.*(Args.AirMass - Args.InAirMass);  % mag unit
Factor     = 10.^(-0.4.*IntAtmoExt);
ExtMatrix  = Matrix.*Factor;








