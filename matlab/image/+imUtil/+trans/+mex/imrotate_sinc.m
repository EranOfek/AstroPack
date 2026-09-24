% Rotate a 2-D image around its center using separable sinc interpolation.
%
% Description:
%     Rotate a 2-D real image by a given angle around the image center using
%     inverse mapping and full separable sinc interpolation. The output image
%     has the same size and class as the input image.
%
%     The interpolation assumes that the image is zero outside its boundaries.
%     Therefore, no padding is applied explicitly, but pixels outside the input
%     image contribute zero to the interpolated value.
%
%     The rotation is performed relative to the image center
%         Cx = (Nx + 1)./2
%         Cy = (Ny + 1)./2
%     where Nx and Ny are the number of columns and rows in the input image.
%
%     For each output pixel, the corresponding position in the input image is
%     calculated using inverse rotation, and the image value is evaluated using
%     separable sinc interpolation:
%
%         sinc(X) = sin(pi.*X)./(pi.*X)
%
%     This interpolation is accurate but computationally expensive, especially
%     for large images, because the kernel has infinite support.
% Input  : - (Image) A 2-D real image of class single or double.
%          - (Rotation) Rotation angle in degrees, measured counter clockwise.
% Output : - (NewImage) The rotated image. Same size and class as Image.
% Author : ChatGPT + Eran Ofek (Apr 2026)
% Compilation: mex -R2018a CXXFLAGS='$CXXFLAGS -O3 -march=native -fopenmp' LDFLAGS='$LDFLAGS -fopenmp' imrotate_sinc.cpp
% Example:
%{
        G=imUtil.kernel2.gauss([1.5 3 0.5]);
        NG=imrotate_sinc(G,2);
        NNG=imrotate_sinc(NG,-2);
        max(abs(G-NNG),[],'all')./max(G,[],'all')
%}