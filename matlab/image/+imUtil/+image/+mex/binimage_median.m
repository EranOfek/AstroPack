% Bin an image by integer factors using the median of each box (MEX).
% Description: Shrink an image by the integer factors given in BinSizeXY,
%              replacing each bin box by the median of its pixels. The
%              median is robust to a minority of bad pixels in the box
%              (e.g. a 2x2 box with one hot pixel returns the median of
%              the three good pixels) but biases compact sources whose
%              peak occupies most of the box. NaN pixels are ignored; a
%              box with no finite pixel returns NaN. The image is trimmed
%              (bottom/right) to an integer number of bins. Implemented as
%              a C++ MEX; boxes are computed in parallel when the MEX is
%              built with OpenMP. This .m file only provides the help and
%              a not-compiled error; the compiled MEX takes precedence.
% Input  : - An image matrix [M x N], class single or double.
%          - Bin size [X(columns), Y(rows)], or a scalar applied to both
%            dimensions. Class double, positive integers.
% Output : - The binned image [M/Y x N/X], same class as the input: the
%            median of the finite pixels in each box.
% Author : Claude + Eran Ofek (Jun 2026)
% Example: Image          = 100 + randn(256).*5;
%          Image(128,128) = 5000;        % one hot pixel
%          BinImage       = binimage_median(Image, [2 2])