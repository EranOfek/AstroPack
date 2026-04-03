% Extract sub-images from an image into a cube.
% Package: +imUtil/+cut/+mex
% Description: Given a 2-D image and a list of CCDSEC definitions, extract
%              the corresponding sub-images and return them as a 3-D cube.
%              Each plane in the output cube corresponds to one row in
%              Sub_CCDSEC.
% Input  : - A 2-D image matrix. Can be of any non-complex numeric or
%             logical class.
%          - Sub_CCDSEC matrix of size Nsub-by-4 in the format:
%            [Xmin Xmax Ymin Ymax]
%            Coordinates are in MATLAB convention (1-based, inclusive).
%            All rows must define sub-images of identical size.
% Output : - A 3-D cube of extracted sub-images of size:
%            NySub x NxSub x Nsub
%            where:
%            NxSub = Xmax - Xmin + 1
%            NySub = Ymax - Ymin + 1
%            The output class is identical to the input image class.
% Notes   : - The i-th output plane is equivalent to:
%            Image(Sub_CCDSEC(i,3):Sub_CCDSEC(i,4), ...
%                  Sub_CCDSEC(i,1):Sub_CCDSEC(i,2))
%          - X corresponds to columns, Y corresponds to rows.
%          - This function assumes that all requested cutouts have the same
%            size. Otherwise a cube output is not possible.
%          - Intended as a fast MEX replacement for repeated MATLAB
%            indexing.
% Author: ChatGPT + Eran Ofek (Apr 2026)
% Compilation: mex -R2018a CXXFLAGS='$CXXFLAGS -O3 -march=native -mtune=native -fopenmp' LDFLAGS='$LDFLAGS -fopenmp' image2cube.cpp
% Example: VX=(1:1:1716); VY=VX.';
%          Im=VX.*1.1+VY.*1.2;
%          [Sub_CCDSEC, NSub, NoOverlapCCDSEC, NewNoOverlapCCDSEC, CentersXY] = imUtil.cut.gridSubImage([1716 1716], [256 256]);
%          % Old version: Sub=imUtil.cut.partition_subimage(Im,Sub_CCDSEC);
%          Cube=imUtil.cut.mex.image2cube(Im,Sub_CCDSEC);
%          % opposite function:
%          FullImage = imUtil.cut.mex.cube2image(Cube, Sub_CCDSEC, NoOverlapCCDSEC, NewNoOverlapCCDSEC);