function Res=conv2_fast(Mat1, Mat2, UseFFT, PadMethod)
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
% Example: imUtil.filter.conv2_fast(rand(1000,1000),rand(1000,1000));
%          
% Reliable: 2
%--------------------------------------------------------------------------

arguments
    Mat1
    Mat2
    UseFFT            = [];
    PadMethod         = ''; % 'symmetric'; % circular | replicate | symmetric
end

Threshold = 0.004;  % requires calibration
SizeThresh = 2500;
if isempty(UseFFT)
    % auto method
    UseFFT = (numel(Mat2)./numel(Mat1))>Threshold && numel(Mat1)>SizeThresh;
end

if ~isempty(PadMethod)
    % pad Mat1
    PadSize = size(Mat2);
    Mat1    = padarray(Mat1, PadSize, PadMethod);
end

if UseFFT
    % use fft
    Res = imUtil.filter.conv2_fft(Mat1,Mat2);
else
    Res = conv2(Mat1,Mat2,'same');
end

if ~isempty(PadMethod)
    % unpad the Result
    Res = imUtil.filter.unpad_array(Res, PadSize);
end
