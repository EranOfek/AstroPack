function readNoiseFromImage
% Estimate the read noise in a single image in native units.
%   The read noise is estimated by calculating the rms in small blocks (sub
%   images). Prefereably