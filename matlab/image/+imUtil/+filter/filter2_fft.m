function Res=filter2_fft(Mat1,Mat2,Method)
% cross-correlate (filter) two 2-D matrices using fft.
% Package: imUtil.filter
% Description: 2D Cross correlation (filtering) of two matrices.
%              The second matrix is the "filter" and should be smaller or
%              equal in size to the first matrix. Note that in filter2 the
%              order of the inputs is reversed.
%              Unlike filter2, this function calculate the
%              cross-correlation using fft and hence is much faster in
%              cases in which the filter matrix is large.
% Input  : - A matrix.
%          - A matrix (or filter).
%          - Method:
%            'rot' - rotate by 180 and convolve using fft (slowest).
%            'conj' - use conj, and make sure that result is not shifted.
%                   For kernels that go to zeros at the edge, this is
%                   equilavlent to rot, with the exceprtion of the image
%                   boundries.
%            'conjpad' - like conj but the padding is done in the fft
%                   stage (slower than 'conj').
%            'Conjsh' - Use conj, but don't shift by +1.
%                   The output is shifted (cyclicaly) by +1 compared to the
%                   output of 'rot' and 'conj'. (fastest).
%            Default is 'conj'.
% Output : - Convolution between the two matrices (or filtered image).
% License: GNU general public license version 3
% Tested : Matlab R2015b
%     By : Eran O. Ofek                    Apr 2016
%    URL : http://weizmann.ac.il/home/eofek/matlab/
% Example: imUtil.filter.filter2_fft(rand(1000,1000),rand(1000,1000));
% Reliable: 2
%--------------------------------------------------------------------------

arguments
    Mat1
    Mat2
    Method         = 'conj';
end

Size1 = size(Mat1,[1 2]);
Size2 = size(Mat2,[1 2]);

if (any(Size2>Size1))
    error('Second matrix must be equal or smaller than first matrix - use conv_fft2.m instead');
end

switch lower(Method)
    case 'rot'
        % This is working and equivalent to filter2 (up to roundoff errors)
        Mat2 = rot90(Mat2,2);  % rotate so that convolution will be equivalent to filtering
    
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

    case 'conj'
        % This result is equivalent to the rot option
                
        Mat2 = padarray(  (Mat2),Size1-Size2,'post');
        Mat2 = circshift(Mat2,-floor(Size2.*0.5));
        
        % not clear to me why this line is not correct:
        %Res  = ifftshift(ifftshift(ifft2(fft2(Mat1).* conj(fft2(Mat2))),2),1);
        
        % not clear why ifftshift doens't swap the 3rd dim??
        Res  = ifftshift(ifft2(fft2(Mat1).* conj(fft2(Mat2))));
        Res  = abs(Res);
        %Res  = circshift(Res,[-1 -1]);
        
        %Res  = circshift(Res,floor(size(Mat2)));
        Res  = fftshift(Res);
        
    case 'conjold'
        % This result is equivalent to the rot option
        % use to work - no clue what happened
        
        Mat2 = padarray(  (Mat2),Size1-Size2,'post');
        Res  = ifftshift(ifft2(fft2(Mat1).* conj(fft2(Mat2))));
        Res  = circshift(Res,[-1 -1]);
        
        % old
        %Res  = [Res, Res(:,1)];
        %Res  = [Res; Res(1,:)];
        %Res  = Res(2:end,2:end);
        
        % old old
        %Res  = padarray(Res,[1 1],'post');
        %Res  = fftshift(Res);
        %Res  = Res(1:Size1(1),1:Size1(2));

    case 'conjpad'
        % This result is equivalent to the rot option
        
        %Mat2 = padarray(  (Mat2),Size1-Size2,'post');
        %Res  = ifftshift(ifft2(fft2(Mat1).* conj(fft2(Mat2))));
        Res  = ifftshift(ifft2(fft2(Mat1).* conj(fft2(Mat2,Size1(1),Size1(2)))));
        Res  = [Res, Res(:,1)];
        Res  = [Res; Res(1,:)];
        Res  = Res(2:end,2:end);
        %Res  = padarray(Res,[1 1],'post');
        %Res  = fftshift(Res);
        %Res  = Res(1:Size1(1),1:Size1(2));

        
    case 'conjsh'
        % The result is shifted by +1 in both axes compared with conj,
        % and rot methods:

        Mat2 = padarray(  (Mat2),Size1-Size2,'post');
        Res  = ifftshift(ifft2(fft2(Mat1).* conj(fft2(Mat2))));

        
    otherwise
        error('Unknown Method option');
end

