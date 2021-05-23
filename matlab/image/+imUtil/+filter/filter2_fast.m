function Res=filter2_fast(Mat1, Mat2, UseFFT)
% Source/template detection in 2D images by filtering (cross-correlation) 
% Package: imUtil.filter
% Description: 2D filtering (cross correkation) of an image (matrix) with a
%              template (filter). The program chooeses which algorith to
%              use, either filtering via convolution or fft, which ever
%              supposed to be faster given the filter size compared to the
%              matrix size.
% Input  : - A matrix.
%          - A filter.
%          - UseFFT: [] - auto, otherwise logical. Default is [].
% Output : - Convolution between the two matrices.
% License: GNU general public license version 3
% Tested : Matlab R2015b
%     By : Eran O. Ofek                    Apr 2016
%    URL : http://weizmann.ac.il/home/eofek/matlab/
% Example: imUtil.filter.filter2_fast(rand(1000,1000),rand(1000,1000));
% Reliable: 2
%--------------------------------------------------------------------------


arguments
    Mat1
    Mat2
    UseFFT    = [];
end

Threshold = 0.004;  % requires calibration
if isempty(UseFFT)
    % auto method
    UseFFT = (numel(Mat2)./numel(Mat1))>Threshold;
end

if UseFFT
    % use fft
    Res = imUtil.filter.filter2_fft(Mat1,Mat2);
else
    % note Mat2 is the first argument
    Res = filter2(Mat2,Mat1,'same');
end

