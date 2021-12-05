function [Sigma_phi,Sigma_F]=sigma_phi(w,Tau,Alpha,Gamma,Epsilon_F_hat)
% Calculate Sigma_phi and Sigma_F
% Package: +TimeDelay
% Description: calculate Sigma_phi and Sigma_F - the expectency value of a
%              the power spectrum of a system of N time delayed images of
%              the same source.
% Input  : - A vector of angular frequencies.
%          - A vector of time delays of length N-1, where N is the number
%            of images.
%          - A vector of \alpha-s, the flux normalization of each source.
%            The vector lengt should be N.
%          - Gamma. The minus of the power-law index of the source power
%            spectrum.
%            Alternatively, this can be a vector with the same length as
%            the frequencies vector. In this case, this is the power
%            spectrum of the process.
%          - Epsilon_hat_F - the error in the fourier transform of the time
%            series. Default is 0.
% Output : - A vector of \Sigma_\phi
%          - A vector of \Sigma_F
% Reference: Springer & Ofek 2021
%       By : Eran O. Ofek                 Oct 2020
% Example: [Sigma_phi,Sigma_F]=TimeDelay.sigma_phi((1:1:100)',30,[1 0.6],3,0)

if nargin<5
    Epsilon_F_hat = 0;
end

w(1) = 1;

if numel(Tau)~=(numel(Alpha)-1)
    error('Length of Tau should be equal to length of Alpha -1');
end

Tau   = [0; Tau(:)];
Alpha = Alpha(:);

Sum = sum(Alpha.^2);
Ntau = numel(Tau);
for Itau1=1:1:Ntau
    for Itau2=Itau1+1:1:Ntau
        
        Sum = Sum + Alpha(Itau1).*Alpha(Itau2).*cos(w.*(Tau(Itau1) - Tau(Itau2)));
    end
end
if numel(Gamma)==1
    PS = abs(w).^(-Gamma);
else
    % Gamma is a power spectrum
    PS = Gamma;
end
Sigma_phi = Sum.*PS;

if nargout>1
    Sigma_F = Sigma_phi + Epsilon_F_hat.^2;
end