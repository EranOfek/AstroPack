% Fast mex for shifting images using lanczos3 interpolation
%   This function is based on a fast mex implementation of Lanczos3
%   interpolation. The function uses the seperabality of the lanczos3 and
%   SIMD and openmp.
%   For small matrices it is much faster compared to imUtil.trans.shift_fft
% Input  : - A cube of images, in which the image index is in the 3rd dim.
%          - Vector of shifts in X direction (same length as the number of
%            images).
%          - Vector of shifts in Y direction.
% Output : - A cube of shifted images.
% Author : Eran Ofek (2026 Jan) 
% Compilation: mex -R2018a CXXFLAGS='$CXXFLAGS -O3 -march=native -mavx2 -mfma -fopenmp -std=c++17' LDFLAGS='$LDFLAGS -fopenmp' shift_lanczos3.cpp
%              % for AVX512 use:
%              mex -R2018a CXXFLAGS='$CXXFLAGS -O3 -march=native -fopenmp -std=c++17' LDFLAGS='$LDFLAGS -fopenmp' shift_lanczos3.cpp
% Example: ShiftedCube=imUtil.trans.mex.shift_lanczos3(Cube,DX,DY);