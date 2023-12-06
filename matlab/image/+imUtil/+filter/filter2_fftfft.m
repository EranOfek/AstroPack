function Res=filter2_fftfft(Mat1,Mat2,Is1fft,Is2fft)
% cross-correlate a 2-D matrix with bank using fft. The input maybe ffted
% Package: imUtil.filter
% Description: 2D Cross correlation (filtering) of two matrices, or a
%              matrix with a template bank of matrices.
%              The second matrix is the "filter" and should be smaller or
%              equal in size to the first matrix. Note that in filter2 the
%              order of the inputs is reversed.
%              Unlike imUtil.filter2_fft in which both inputs are in the
%              space domain, here the input matrices may be either in the
%              space domain or frequency domaon.
% Input  : - A matrix.
%          - A matrix (filter), or a cube in which the third dimension is
%            the template index.
%          - A logical flag indicating if the first input matrix is ffted.
%            Default is false.
%          - A logical flag indicating if the second input matrix is ffted.
%            Default is false.
%            If true, then the size of the second matrix must be equal to
%            the size of the first matrix.
% Output : - Cross-correlation between the matrices.
%            If the second matrix is a cube, then will return a cube which
%            size is equal to the size of the second matrix. The third
%            dimension of the cube is the filter index.
% License: GNU general public license version 3
% Tested : Matlab R2015b
%     By : Eran O. Ofek                    Apr 2016
%    URL : http://weizmann.ac.il/home/eofek/matlab/
% Example: imUtil.filter.filter2_fftfft(rand(1000,1000),rand(1000,1000));
% Reliable: 2
%--------------------------------------------------------------------------

arguments
    Mat1
    Mat2
    Is1fft(1,1) logical    = false;
    Is2fft(1,1) logical    = false;
end


Size1 = size(Mat1);
Size2 = size(Mat2);

if (any(Size2(1:2)>Size1(1:2)))
    error('Second matrix must be equal or smaller than first matrix - use conv_fft2.m instead');
end


% using the 'conj' method (from imUtil.filter.filter2_fft)
% This result is equivalent to the rot option

if Is1fft
    FftMat1 = Mat1;
else
    FftMat1 = fft2(Mat1);
end

if Is2fft
    if all(Size1(1:2)==Size2(1:2))
        FftMat2 = Mat2;
    else
        error('In case of Is2fft=true, Mat1 and Mat2 must have the same size');
    end
else
    if ~all(Size2(1:2)==Size1(1:2))
        Mat2 = padarray(Mat2,Size1-Size2,'post');
    end
    FftMat2 = fft2(Mat2);
end

[~,~,Nbank] = size(FftMat2);
if Nbank==1
    Res  = ifftshift(ifft2(FftMat1.* conj(FftMat2)));
    Res  = circshift(Res,[-1 -1]);
    
    %Res  = [Res, Res(:,1)];
    %Res  = [Res; Res(1,:)];
    %Res  = Res(2:end,2:end);
else
    % template bank
    Res = zeros(size(FftMat2));

    for Ibank=1:1:Nbank
        Tmp  = ifftshift(ifft2(FftMat1.* conj(FftMat2(:,:,Ibank))));

        %circshift(Tmp,[-1 -1]);
        %Tmp  = [Tmp, Tmp(:,1)];
        %Tmp  = [Tmp; Tmp(1,:)];
        %Tmp  = Tmp(2:end,2:end);

        Res(:,:,Ibank) = circshift(Tmp,[-1 -1]);
    end
end
        