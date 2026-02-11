function  lsd_single_scale
% mex wrapper to the modified LSD entry point lsd_scale()
%  using Hessian instead of gradient
%
% Inputs:
%   Im   - a single precision image
%   scale - a scalar scale factor(i.e. 1/3)
%
% Outputs:
%   segs - a 7xN array of segments and additional info (see lsd.h for
%          description)
%
% Build the mex with:
% mex('LSD_single/lsd_scale_mex.c', 'LSD_single/lsd.c', '-output', 'lsd_single_scale_mex', '-R2018a');
