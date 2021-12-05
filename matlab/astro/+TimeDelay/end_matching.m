function [F_t,Slope,Diff]=end_matching(T,F_t)
% Apply end-matching to a light curve such that first and last point have the same flux.
% Package: +TimeDelay
% Input  : - Vectot of times.
%          - Vector of fluxes.
% Output : - Vector of fluxes after the end-matching operation.
%          - Removed slope.
%          - Removed values from F_t to get the end matched F_t
% Example: [F_t,Slope,Diff]=TimeDelay.end_matching(T,F_t)



Slope = (F_t(end) - F_t(1))./(T(end) - T(1));
PolyPar = [Slope 0];
Diff    = polyval(PolyPar,T);
F_t     = F_t - Diff;