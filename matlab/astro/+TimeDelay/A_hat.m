function A_hat_=A_hat(Tau,Alpha,X_i,w)
% A_hat operator (Springer & Ofek 2021b)
% Package: +TimeDelay
% Input  : - Time delay tau.
%          - Vector of images Alpha
%          - Vector of images X position
%          - Vector of frequencies on which to calculate A_hat
% Output : - A_hat diagonal vector.
% Reference: Spruinger & Ofek 2021b

A_numerator = Alpha(1).*X_i(1) + Alpha(2).*X_i(2).*exp(1j.*w.*Tau);
A_denom     = Alpha(1)         + Alpha(2)        .*exp(1j.*w.*Tau);
A_hat_      = A_numerator./A_denom;
