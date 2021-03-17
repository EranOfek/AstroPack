function Res=conv2_fft(Mat1,Mat2)
% convolve two 2-D matrices using fft.
% Package: imUtil.filter
% Description: Convolve two matrices in which the second matrix is smaller
%              or equal in size to the first matrix.
%              If the second matrix is smaller then it is post-padded with 
%              zeros. The convolution is done using fft.
% Input  : - A matrix.
%          - A matrix.
% Output : - Convolution between the two matrices (or filtered image).
% License: GNU general public license version 3
% Tested : Matlab R2015b
%     By : Eran O. Ofek                    Apr 2016
%    URL : http://weizmann.ac.il/home/eofek/matlab/
% Example: imUtil.filter.conv2_fft(rand(1000,1000),rand(1000,1000));
% Reliable: 2
%--------------------------------------------------------------------------


Size1 = size(Mat1);
Size2 = size(Mat2);

if (any(Size2>Size1))
    error('Second matrix must be equal or smaller than first matrix - use conv_fft2.m instead');
end

% padding
MaxSize = max(Size1,Size2);
MinSize = min(Size1,Size2);
Size    = MaxSize + floor(MinSize.*0.5);
Mat1    = padarray(Mat1,Size-Size1,'post');
Mat2    = padarray(Mat2,Size-Size2,'post');
Sh1     = floor(MinSize.*0.5);


% convolution
Res = ifft2(fft2(Mat1).*fft2(Mat2));      
% remove padding
Res = Res(1+Sh1(1):end,1+Sh1(2):end);



% if (Size1(1)<Size2(1)),
%      Res = Res(1:end-1,:);
% elseif (Size1(1)>Size2(1)),
%      Res = Res(2:end,:);
% end
% if (Size1(2)<Size2(2)),
%      Res = Res(:,1:end-1);
% elseif (Size1(2)>Size2(2)),
%      Res = Res(:,2:end);    
% end


