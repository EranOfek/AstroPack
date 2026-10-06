% Sum image values along lines passing through the stamp center (Radon).
%   For each Nyquist-sampled line angle in the range [0, 180) deg,
%   sum the image values within a strip of width LineWidth that passes
%   through the center of the image stamp. Useful for detecting
%   diffraction spikes and satellite trails passing through the center
%   of image stamps.
%   The center is at ((Nx+1)/2, (Ny+1)/2), and the angle Theta is
%   measured from the X axis (dim 2) toward the Y axis (dim 1), i.e.,
%   the line is: y - yc = tan(Theta)*(x - xc).
%   The angles are sampled on a pseudo-polar grid such that the line
%   end point moves by 1 pix along the stamp boundary:
%   tan(Theta)=k/R for |tan|<=1, and cot(Theta)=k/R for |tan|>=1,
%   where k=-R..R and R=floor(max(Nx,Ny)/2), giving 4R angles.
%   Pixels partially covered by the strip get fractional weights, so the
%   sums vary smoothly with the angle.
%   This is a MEX function (radonCenter.cpp). This file contains the
%   help, and is executed only if the MEX file was not compiled.
% Input  : - A 2D image stamp, or a cube of image stamps in which the
%            image index is in the 3rd dimension (Ny X Nx X Nim).
%            Supported classes: double, single, int8-64, uint8-64.
%            NaNs propagate - replace them before calling
%            (e.g., Cube(isnan(Cube))=0).
%          - Line width [pix] measured perpendicular to the line.
%            Must be a positive odd integer.
%            Default is 3.
% Output : - A column vector (Nang X 1) of tan(Theta), ordered by
%            increasing Theta in the range [0, 180) deg.
%            Theta=90 deg is returned as +Inf.
%            To convert to deg use: Theta = mod(atand(TanRot), 180).
%          - A matrix (Nang X Nim) of sums along the lines.
%            One column per image stamp.
%            Class is single for single input, and double otherwise
%            (accumulation is always done in double).
%          - A column vector (Nang X 1) of the effective number of
%            pixels (sum of weights) in each line.
%            For a constant image B: RadonSums = B.*Area.
%            Lines near 45 deg are longer, so use this to normalize,
%            e.g., S/N = (RadonSums - B.*Area)./(Sigma.*sqrt(Area)).
% Compile: Linux:   mex -O CXXFLAGS='$CXXFLAGS -fopenmp' CXXOPTIMFLAGS='-O3 -march=native' LDFLAGS='$LDFLAGS -fopenmp' radonCenter.cpp
%          Windows: mex -O COMPFLAGS='$COMPFLAGS /openmp /O2' radonCenter.cpp
%          macOS:   mex -O radonCenter.cpp
%          Number of threads is set by the OMP_NUM_THREADS env. variable.
% Author : Claude + Eran Ofek (2026 Oct)
% Example: Cube = randn(65,65,100,'single');
%          [TanRot, RadonSums] = radonCenter(Cube);
%          Theta = mod(atand(TanRot), 180);
%          [TanRot, RadonSums, Area] = radonCenter(Cube, 5);
%          Z = RadonSums./sqrt(Area);   % S/N for B=0, Sigma=1