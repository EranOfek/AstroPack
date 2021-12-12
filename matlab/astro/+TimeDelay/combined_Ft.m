function Ft=combined_Ft(t,ft,A0,Alpha,Tau,InterpMethod)
% Reconstruct the combined light curve from the original light curve
% Package: +TimeDelay
% Description: Given a light curve of the source, generate the combined
%              light curve of all the images with time delays.
% Input  : - Vector of times.
%          - Vector of fluxes as a function of time for the source.
%          - Alpha_0
%          - Vector of \alpha_i
%          - vector of \tau (not including the first (zero) time delay.
%          - Interpolation method. Default is 'pchip'.
% Output : - The commbined flux ligt curve (raw vector).
% Example: Ft=TimeDelay.combined_Ft(t,ft,A0,Alpha,Tau);

if nargin<6
    InterpMethod = 'pchip';
end

t       = t(:).';
ft      = ft(:).';

Nt      = numel(t);
Tau     = [0, Tau(:).'];
Nimage = numel(Alpha);
Shifted_ft = zeros(Nimage,Nt);
for Iimage=1:1:Nimage
    Shifted_ft(Iimage,:) = Alpha(Iimage).*interp1(t,ft, t+Tau(Iimage),InterpMethod);
end

Ft = A0 + sum(Shifted_ft,1);

    