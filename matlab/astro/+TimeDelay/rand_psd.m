function F_w=rand_psd(Psd,Gamma)
% Generate a random vector with distribution following some power spectrum.
% Package: +TimeDelay
% Description:
% Input  : - Eithr a scalar or a vector.
%            If a scalar than this is the number of points (frequencies)
%            in the generated vector.
%            If a vector then this is the vector of expectrency values in
%            each frequency.
%          - An optional scalar (in case the first input is a scalar) that
%            represents the minus power-law index of the power-spectrum
%            expectency value (P~f^-Gamma). Default is 3.
% Output : - A random realization of the requested power spectrum.
%            The ifft of this power spectrum is real.
%     By : Eran O. Ofek                  Oct 2020
% Example: F_w=TimeDelay.rand_psd((1:1:100))
%          F_w=TimeDelay.rand_psd(100,3)

if nargin<2
    Gamma = 3;
end
if numel(Psd)==1
    % number of elements
    N   = Psd;
    W   = (1:1:N).';
    Psd = W.^(-Gamma);
else
    % psd is provided
    N   = numel(Psd);
    Psd = Psd(:);
end


Nfl2 = floor(N./2);
F_w  = zeros(size(Psd));
    
F_w(2:1+Nfl2) = randn(Nfl2,1).* sqrt(Psd(2:1+Nfl2)./2) + 1j.*randn(Nfl2,1).* sqrt(Psd(2:1+Nfl2)./2);
F_w(1+floor((1+N)./2):end) = flip(conj(F_w(2:1+Nfl2)));

%F_w[(1+N)//2:] = np.flip(np.conj(F_w[1:1+N//2]))

if mod(N,2) == 0
    F_w(1+Nfl2) = 0;
end
