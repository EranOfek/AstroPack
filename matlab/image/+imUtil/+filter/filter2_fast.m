function Res=filter2_fast(Mat1, Mat2, UseFFT, PadMethod)
% Source/template detection in 2D images by filtering (cross-correlation) 
% Package: imUtil.filter
% Description: 2D filtering (cross correkation) of an image (matrix) with a
%              template (filter). The program chooeses which algorith to
%              use, either filtering via convolution or fft, which ever
%              supposed to be faster given the filter size compared to the
%              matrix size.
% Input  : - A matrix.
%          - A filter.
%          - UseFFT: Method to use. [] - auto, otherwise logical. Default is [].
%          - Padding before operation (padded region will be removed from
%            output).
%            '' - do nothing (Default).
%            'circular' - circular boundry conditions.
%            'replicate' - relpicate nearest edge value.
%            'symmetric' - mirror reflection boundry conditions.
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
    PadMethod         = ''; % 'symmetric'; % circular | replicate | symmetric
end

Threshold = 0.004;  % requires calibration
if isempty(UseFFT)
    % auto method
    UseFFT = (numel(Mat2)./numel(Mat1))>Threshold;
end

if ~isempty(PadMethod)
    % pad Mat1
    PadSize = size(Mat2);
    Mat1    = padarray(Mat1, PadSize, PadMethod);
end

if UseFFT
    % use fft
    Res = imUtil.filter.filter2_fft(Mat1,Mat2);
else
    % note Mat2 is the first argument
    Res = filter2(Mat2,Mat1,'same');
end


if ~isempty(PadMethod)
    % unpad the Result
    Res = imUtil.filter.unpad_array(Res, PadSize);
end
