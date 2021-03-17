function Res=conv2_fast(Mat1,Mat2)
% convolve two 2-D matrices using either fft or conv, whichever faster.
% Package: imUtil.filter
% Description: Convolve two matrices in which the second matrix is smaller
%              or equal in size to the first matrix.
%              If the second matrix is smaller then it is post-padded with 
%              zeros. The convolution is done using either
%              imUtil.filter.fft or conv2, which ever is estimated to be
%              faster.
% Input  : - A matrix.
%          - A matrix.
% Output : - Convolution between the two matrices.
% License: GNU general public license version 3
% Tested : Matlab R2015b
%     By : Eran O. Ofek                    Apr 2016
%    URL : http://weizmann.ac.il/home/eofek/matlab/
% Example: imUtil.filter.conv2_fast(rand(1000,1000),rand(1000,1000));
% Reliable: 2
%--------------------------------------------------------------------------

Threshold = 0.004;  % requires calibration
if (numel(Mat2)./numel(Mat1))>Threshold
    % use fft
    Res = imUtil.filter.conv2_fft(Mat1,Mat2);
else
    Res = conv2(Mat1,Mat2,'same');
end

