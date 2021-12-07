function [xtEM,FtEM]=end_matching_xt(t,xt,Ft,x0,x,A0,A,ErrF_t,Errx_t)
% Apply end-matching to a center-of-light position consistent with light curve 
% Package: +TimeDelay
% Input  : - Vectot of times.
%          - Vector of center of light positions.
%          - Vector of fluxes.
%          - x0
%          - Vector of [x1, X2]
%          - A0
%          - Vector of [A1, A2]
%          - Err(F_t)
%          - Err(x_t)
% Output : - Vector of center-of-light after the end-matching operation.
% Example: [xtEM,FtEM]=TimeDelay.end_matching(T,xt,Ft,x0,x,A0,A,ErrF_t,Errx_t)



[FtEM,~,Eta] = TimeDelay.end_matching(t,Ft);

Zeta = sum(A.*x).*Eta./sum(A);

xtEM = (xt - Zeta./Ft)./(1-Eta./Ft);

