% Compute masked residual/std cube within a radial aperture.
% Description:
%   [Flag, ResidStd] = cubeResidStd_Radius(VecXrel, VecYrel, DX, DY, Resid, Std, FitRadius2)
%   computes, for each plane k, the squared distance
%       R2(i,j,k) = (VecXrel(j) - DX(k)).^2 + (VecYrel(i) - DY(k)).^2
%   and sets
%       Flag(i,j,k)     = R2(i,j,k) < FitRadius2
%       ResidStd(i,j,k) = Flag(i,j,k) .* Resid(i,j,k) ./ Std(1,1,k)
%   without forming the intermediate MatX/MatY/MatR2 arrays.
%
% Input  : - (VecXrel) Vector of relative X positions. Size: [N,1] or [1,N].
%            Class: single or double.
%          - (VecYrel) Vector of relative Y positions. Size: [N,1] or [1,N].
%            Must have numel(VecYrel) == numel(VecXrel).
%            Class: same as VecXrel.
%          - (DX) Vector of X offsets for each cube plane. Size: [M,1] or [1,M].
%            Class: same as VecXrel.
%          - (DY) Vector of Y offsets for each cube plane. Size: [M,1] or [1,M].
%            Must have numel(DY) == numel(DX).
%            Class: same as VecXrel.
%          - (Resid) Residual cube. Size: [N, N, M].
%            Class: same as VecXrel.
%          - (Std) Standard deviation per plane. Size: [1, 1, M].
%            Class: same as VecXrel.
%          - (FitRadius2) Squared fitting radius (R^2 threshold). Scalar.
%            Class: real scalar (double or same class as VecXrel).
%
% Output : - (Flag) Logical mask cube indicating pixels within the radius.
%            Size: [N, N, M]. Class: logical.
%          - (ResidStd) Residual normalized by Std and masked by Flag:
%            ResidStd = Flag .* Resid ./ Std
%            Size: [N, N, M]. Class: same as Resid/Std.
%
% Notes:
%   - All numeric inputs (VecXrel, VecYrel, DX, DY, Resid, Std) must be
%     either all single or all double.
%   - Std is broadcast along the first two dimensions for each plane k.
%   - The implementation is a MEX function optimized with OpenMP and
%     avoids creating the intermediate MatX, MatY, and MatR2 arrays.
%
% Compilation: mex -R2018a CXXFLAGS="$CXXFLAGS -O3 -march=native -fopenmp" LDFLAGS="$LDFLAGS -fopenmp" cubeResidStd_Radius.cpp
% Author : ChatGPT + Eran Ofek (Nov 2025)
% Example:
%   N  = 256;
%   M  = 32;
%   X  = linspace(-1, 1, N);
%   Y  = X;
%   DX = linspace(-0.3, 0.3, M);
%   DY = linspace(-0.3, 0.3, M);
%
%   [VecXrel, VecYrel, DX, DY] = deal(single(X), single(Y), single(DX), single(DY));
%   Resid = rand(N, N, M, 'single');
%   Std   = 0.1 + rand(1, 1, M, 'single');
%   FitRadius2 = single(0.2^2);
%
%   [Flag, ResidStd] = cubeResidStd_Radius(VecXrel, VecYrel, DX, DY, Resid, Std, FitRadius2);