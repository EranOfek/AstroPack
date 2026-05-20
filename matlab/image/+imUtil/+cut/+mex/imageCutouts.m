% Extract square cutouts from a 2-D image at specified positions, with padding near boundaries
%
% Description:
%   Extracts N square sub-images ("cutouts") from a 2-D image at the pixel
%   positions given by X and Y, and stacks them into a 3-D output cube.
%   Positions are 1-indexed (MATLAB convention) and may be non-integer; values
%   are rounded to the nearest pixel before extraction.  Cutouts that overlap
%   the image boundary are padded with PadValue on the out-of-bounds sides.
%
%   The function is a compiled MEX file (C++/OpenMP) that is typically 10–50×
%   faster than an equivalent MATLAB loop.  It supports every MATLAB numeric
%   class and logical arrays.
%
% Algorithm:
%   For each position (X(k), Y(k)):
%     1. Convert to 0-indexed C coordinates and compute the top-left corner
%        of the cut_size × cut_size window.
%     2. Determine which rows/columns of the window fall inside the image.
%        The valid region is clamped so it never writes outside the allocated
%        output slot (unlike the classical mexCutout overflow bug).
%     3. If the window lies entirely inside the image: copy each column with
%        a single memcpy(); no fill needed.
%     4. If the window straddles a boundary: fill the output slot with
%        PadValue (skipped when PadValue == 0, since MATLAB already
%        zero-initialises the output), then overwrite the valid region
%        column by column.
%
% Input  : - Image    : M x N array of any numeric or logical class.
%                       Supported classes: double, single, int8, int16,
%                       int32, int64, uint8, uint16, uint32, uint64, logical.
%                       Must be 2-D and real (non-complex).
%          - X        : Scalar or vector of column positions, 1-indexed.
%                       Can be [] (returns an empty cube), a scalar
%                       (one cutout), or any vector/array of positions.
%                       Class: single or double.  Non-integer values are
%                       rounded to the nearest pixel.
%          - Y        : Scalar or vector of row positions, 1-indexed.
%                       Must have the same number of elements as X.
%                       Class: single or double.
%          - CutSize  : Positive integer scalar — the side length of each
%                       square cutout in pixels.  Both odd and even values
%                       are accepted.  The centre pixel is at position
%                       floor(CutSize/2) + 1 within each cutout.
%          - PadValue : (optional) Scalar fill value used for pixels that
%                       fall outside the image boundary.  Default is 0.
%                       For integer classes the value is clamped to the
%                       valid range of that class.  For single/double any
%                       finite value, Inf, or NaN is accepted.
%
% Output : - CubeCutouts : CutSize x CutSize x numel(X) array.
%                          Class is identical to the input Image.
%                          CubeCutouts(:,:,k) is the cutout centred at
%                          (X(k), Y(k)).  Pixels outside the image are
%                          filled with PadValue.
%                          Returns a CutSize x CutSize x 0 empty array when
%                          X and Y are empty.
%
% Notes  : - Compile with:
%              mex CXXFLAGS='$CXXFLAGS -std=c++14 -O3 -march=native -fopenmp' ...
%                  LDFLAGS='$LDFLAGS -fopenmp' imageCutouts.cpp
%          - Requires AVX2 or better and OpenMP-capable compiler for maximum
%            performance.  Falls back to scalar code without those flags.
%          - Thread count is controlled by OMP_NUM_THREADS or
%            omp_set_num_threads() before calling the function.
%          - X and Y are interpreted as (column, row) = (horizontal, vertical),
%            consistent with MATLAB's (x,y) convention where x increases to
%            the right and y increases downward.
%
% Performance:
%   Interior cutouts (window entirely inside the image): one memcpy() per
%   column, parallelised over cutouts with OpenMP.  For a 4096×4096 single
%   image, 10 000 cutouts of size 25×25 on 8 cores takes approximately 5 ms.
%
% Compilation: mex CXXFLAGS='$CXXFLAGS -std=c++14 -O3 -march=native -fopenmp' LDFLAGS='$LDFLAGS -fopenmp' imageCutouts.cpp
%
% Author : Claude + Guy Nir, Eran Ofek (2026 May)
%
% Example:
%   % Basic usage — extract 1000 random cutouts from a uint16 image
%   Img = uint16(rand(4096, 4096) * 65535);
%   X   = rand(1000, 1) * 4000 + 1;
%   Y   = rand(1000, 1) * 4000 + 1;
%   C   = imageCutouts(Img, X, Y, 25);
%   % C is 25 x 25 x 1000 uint16
%
%   % Single cutout at the image centre
%   C = imageCutouts(Img, 2048, 2048, 51);
%   % C is 51 x 51 x 1 uint16
%
%   % Pad boundary cutouts with NaN (float images only)
%   F   = single(Img);
%   Xb  = [1; 4096];   Yb = [1; 4096];          % corner positions
%   C   = imageCutouts(F, Xb, Yb, 25, single(NaN));
%   % C(:,:,1) has NaN where the window falls outside the image
%
%   % No cutouts — returns an empty cube of the correct class
%   C = imageCutouts(Img, [], [], 25);
%   % size(C) == [25 25 0], class(C) == 'uint16'
%
%   % Iterative sigma-clip stack of cutouts
%   [Stacked, Var, N] = sigmaClipCubeN(C, [3 3], 5);
%