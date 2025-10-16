% Fast MEX for smallest bounding spherical circle (cap) via Welzl algorithm
% Description: Compute the minimum-radius spherical cap that contains a set
%              of directions on the unit sphere. The input may be given as
%              longitude/latitude (radians) or as direction cosines (X,Y,Z).
%              The algorithm is an exact (up to FP precision) randomized
%              incremental Welzl solver with expected O(N) complexity.
% Input  : - If two input arguments are provided, they are:
%              * Lon : Vector of longitudes [radians].
%              * Lat : Vector of latitudes  [radians].
%           - If three input arguments are provided, they are the direction
%             cosines:
%              * X   : Vector of X components.
%              * Y   : Vector of Y components.
%              * Z   : Vector of Z components.
%           Notes:
%             * Inputs may be single or double (real). Row/column vectors
%               are accepted. X,Y,Z will be normalized to unit length.
%             * Non-finite rows are ignored. If no valid rows remain,
%               outputs are NaN.
% Output : - LonC   : Cap-center longitude [radians, in [-pi, pi]].
%          - LatC   : Cap-center latitude  [radians].
%          - Radius : Minimal angular radius [radians, in 0..pi].
%            Class of outputs matches inputs (single iff all inputs are single).
% Authors : Eran Ofek + ChatGPT (Oct 2025)
% Compilation: mex -O CXXFLAGS="$CXXFLAGS -std=c++17 -Ofast -march=native -DNDEBUG" boundingCircleSpherical_mex.cpp
% Example: % Random directions around (Lon,Lat)≈(1.0,0.4)
%          N   = 1000;
%          Lon = 1.0 + 0.1*randn(N,1);
%          Lat = 0.4 + 0.1*randn(N,1);
%          [LonC,LatC,R] = boundingCircleSpherical_mex(Lon,Lat);
%          fprintf('Center=(%.3f, %.3f) rad;  Radius=%.3f rad\n',LonC,LatC,R);
%
% Notes:
%  - Algorithm: Randomized incremental Welzl on S^2. The supporting set of
%    the optimal cap consists of ≤3 boundary points (except exact antipodes).
%  - Robustness: Antipodal (or nearly antipodal) pairs force Radius≥pi/2.
%    Degenerate triplets near a great circle fall back to the best 2-point cap.
%  - Determinism: The internal shuffle uses a fixed RNG seed, so results are
%    repeatable for the same inputs/architecture.
%  - Performance: Expected O(N) time; no recursion; no unbounded loops.
%  - Conventions: All angles are in radians. Direction cosines are assumed
%    to represent points on the unit sphere (they are normalized internally).
%  - Edge cases:
%      * Empty input (or all non-finite): outputs are NaN.
%      * Single valid point: Radius=0; center at that direction.