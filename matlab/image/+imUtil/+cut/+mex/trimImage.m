% Fast crop/trim of images or cube by CCDSEC, using MEX
% Input  : - Image or Cube, in which the 3rd dim. is the image index.
%          - CCDSEC [Y1, Y2, X1, X2]
% Output : - Cropped image (equivalent to: Image(Y1:Y2,X1:X2,:)
% Author : Eran Ofek (2025 Sep) 
% Compilation: Linux (OpenMP): mex -O CXXFLAGS="$CXXFLAGS -std=c++11 -O3 -march=native -fopenmp" LDFLAGS="$LDFLAGS -fopenmp" trim_crop_mex.cpp
%              Windows (MSVC, OpenMP): mex -O COMPFLAGS="$COMPFLAGS /O2 /GL /arch:AVX2 /openmp" LINKFLAGS="$LINKFLAGS /LTCG" trim_crop_mex.cpp
% Example: R=imUtil.cut.mex.trimImage(Image, CCDSEC);