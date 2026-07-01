% Mex function tp calculate the background and variance of an image using the SExtractor/Bertin method.
% Description: Estimate the spatially-varying background of an image and
%              its variance, using the SExtractor algorithm described in
%              Bertin & Arnouts (1996; A&AS 117, 393). The image is divided
%              into a grid of meshes (tiles). In each mesh the pixel
%              distribution is iteratively clipped at 3 sigma; if the sigma
%              changed by less than 20% during the clipping the mesh is
%              considered uncrowded and the clipped mean is adopted,
%              otherwise the mode (2.5*median - 1.5*mean) is used. The
%              resulting low-resolution background and rms maps are median
%              filtered and bicubically (natural spline) interpolated back
%              to the full image size. The median in each mesh is evaluated
%              only when the mesh is crowded. This is the help and
%              pure-Matlab fallback for the compiled MEX backBertin.cpp;
%              once the MEX is compiled it shadows this file for execution.
% Input  : - An image (a 2-D matrix), class double or single. NaN pixels
%            are treated as masked and are ignored.
%          - Background mesh size [pix]. Either a scalar (square mesh) or a
%            two-element vector [SizeRows, SizeCols].
%            Default is 128.
%          - Size of the median filter applied to the low-resolution
%            background and rms maps, in mesh units (odd recommended).
%            Default is 3.
%          - Median-filter threshold. A mesh node is replaced by its local
%            median only if it differs from it by more than this value.
%            Default is 0 (i.e., always replace).
% Output : - The background image, B(x,y), with the same size as the input
%            image (class double).
%          - (Optional) the background variance image, sigma_back^2(x,y),
%            with the same size as the input image. It equals the square of
%            the natural-spline-interpolated, median-filtered rms map.
%            Computed only when requested.
%          - (BackSmall) Background in mesh prior to interpolating to the
%            full image.
%          - (VarSmall) Variance in mesh prior to interpolating to the
%            full image.
% Compilation: mex -R2018a CXXFLAGS="$CXXFLAGS -O3 -fopenmp" LDFLAGS="$LDFLAGS -fopenmp" backBertin.cpp
% Author : Eran Ofek (Jun 2026)
% Example: Image          = randn(1024,768).*3 + 100;
%          [Back,Var]     = imUtil.background.mex.backBertin(Image);
%          [Back,Var,BackSmall,VarSmall]     = imUtil.background.mex.backBertin(Image,64,3,0);
%          Back           = imUtil.background.mex.backBertin(single(Image),[128 256]);