% Insert non-overlapping parts of sub-images (in a cube) into a full image.
% Package: +imUtil/+cut/+mex
% Description: Given a cube of sub-images and the definitions of the
%              non-overlapping regions, reconstruct the full image by
%              copying, for each sub-image, only its non-overlapping part
%              into the output image.
% Input  : - A 3-D cube of sub-images of size NySub x NxSub x Nsub.
%            Can be of any non-complex numeric or logical class.
%          - Sub_CCDSEC matrix of size Nsub-by-4 in the format:
%            [Xmin Xmax Ymin Ymax]
%            giving the location of each sub-image in the full-image
%            reference frame.
%          - NoOverlapCCDSEC matrix of size Nsub-by-4 in the format:
%            [Xmin Xmax Ymin Ymax]
%            giving, in the full-image reference frame, the non-overlapping
%            region that should be copied from each sub-image.
%          - NewNoOverlapCCDSEC matrix of size Nsub-by-4 in the format:
%            [Xmin Xmax Ymin Ymax]
%            giving, in the local reference frame of each sub-image, the
%            location of the corresponding non-overlapping region.
% Output : - A 2-D full image. The output image size is inferred from
%            Sub_CCDSEC:
%            [max(Sub_CCDSEC(:,4)), max(Sub_CCDSEC(:,2))]
%            The output class is identical to the input cube class.
% Notes   : - For each image index I, the function performs the equivalent
%            of:
%            FullImage(NoOverlapCCDSEC(I,3):NoOverlapCCDSEC(I,4), ...
%                      NoOverlapCCDSEC(I,1):NoOverlapCCDSEC(I,2)) = ...
%                Cube(NewNoOverlapCCDSEC(I,3):NewNoOverlapCCDSEC(I,4), ...
%                     NewNoOverlapCCDSEC(I,1):NewNoOverlapCCDSEC(I,2), I);
%          - X corresponds to columns, Y corresponds to rows.
%          - Coordinates follow MATLAB convention (1-based, inclusive).
%          - Assumes that all output pixels are covered by the set of
%            NoOverlapCCDSEC regions.
%          - Intended as a fast MEX routine for reconstructing a full image
%            from a cube of cutouts.
% Author: ChatGPT + Eran Ofek (Apr 2026)
% Compilation: mex -R2018a CXXFLAGS='$CXXFLAGS -O3 -march=native -mtune=native -fopenmp' LDFLAGS='$LDFLAGS -fopenmp' cube2image.cpp
% Example: VX=(1:1:1716); VY=VX.';
%          Im=VX.*1.1+VY.*1.2;
%          [Sub_CCDSEC, NSub, NoOverlapCCDSEC, NewNoOverlapCCDSEC, CentersXY] = imUtil.cut.gridSubImage([1716 1716], [256 256]);
%          % Old version: Sub=imUtil.cut.partition_subimage(Im,Sub_CCDSEC);
%          % opposite function:
%          Cube=imUtil.cut.mex.image2cube(Im,Sub_CCDSEC);
%          FullImage = imUtil.cut.mex.cube2image(Cube, Sub_CCDSEC, NoOverlapCCDSEC, NewNoOverlapCCDSEC);