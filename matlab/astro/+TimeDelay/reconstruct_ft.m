function f_t=reconstruct_ft(F_t,x_t,x0,x,A0,A1,ErrF_t,Errx_t)
% Reconstruct f(t) of a lensed quasar from the combined light and position
% Package: +TimeDelay
% Input  : - F_t
%          - x_t
%          - x0
%          - [x1, x2]
%          - A0
%          - A1
%          - ErrF_t. Default is 0.
%          - Errx_t. Default is 0.
% Output : - Reconstruct primary light curve
% Reference: Springer & Ofek (2020)
%      By : Eran O. Ofek        Oct 2020
% Example: f_t=TimeDelay.reconstruct_ft(F_t,x_t,x0,x,A0,A1,ErrF_t,Errx_t)

if nargin<8
    Errx_t = 0;
    if nargin<7
        ErrF_t = 0;
    end
end


f_t = ((F_t - ErrF_t).*(x_t - x(2) - Errx_t) + A0.*(x(2) - x0))./( A1.*(x(1) - x(2)));
