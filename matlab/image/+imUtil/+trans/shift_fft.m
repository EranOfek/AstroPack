function [ShiftedImage,NY,NX,Nr,Nc]=shift_fft(Image,DX,DY,NY,NX,Nr,Nc)
% Shift Image using the sub pixel Fourier shift theorem (sinc interp.)
% Package: imUtil.image
% Description: Shift an image using the FFT shift thorem. This works well
%              when the image does not contain sharp artifacts.
%              Sharp artifacts will produce ringing.
%              Note that the shift is defined on the content of the image,
%              rather than the image boundries - e.g., the stars will be
%              shifted in the requested direction.
% Input  : - An image (2D matrix), or a cube of images, in which the image
%            index is in the 3rd dimension.
%          - X shift to apply to input image, or a vector (if image is
%            cube).
%          - Y shift to apply to input image, or a vector (if image is
%            cube).
%          - NY (supply for faster performences). See output.
%          - NX (supply for faster performences). See output.
%          - Nr (supply for faster performences). See output.
%          - Nc (supply for faster performences). See output.
% Output : - Shifted image with the same size as the input image.
%          - NY
%          - NX
%          - Nr
%          - Nc
% See also: ImUtil.Im.imagefft_shift_fft.m, SIM/image_shift_fft.m,
%           SIM/imagefft_shift_fft.m
% License: GNU general public license version 3
% Tested : Matlab R2015b
%     By : Eran O. Ofek                    May 2016
%    URL : http://weizmann.ac.il/home/eofek/matlab/
% Example: ShiftedImage=imUtil.trans.shift_fft(Image,1.22,-3.1);
%          PSF=imUtil.kernel2.gauss([2;2.5]);
%          SI2_1 =imUtil.trans.shift_fft(PSF(:,:,1),1.22,-3.1);
%          SI2_2 =imUtil.trans.shift_fft(PSF(:,:,2),1.22,-3.1);
%          SI3   =imUtil.trans.shift_fft(PSF,[1.22;1.22],[-3.1;-3.1]);
% Reliable: 2
%--------------------------------------------------------------------------

Algo = 3;


if Algo==0
    % new for cubes
    error('not working');
    
    [NY,NX] = size(Image);
    if (0.5*NX==floor(0.5*NX))
        % NX is even 
        PadX = 0;
    else
        PadX = 1;
        NX   = NX + 1;
    end
    if (0.5*NY==floor(0.5*NY))
        % NX is even 
        PadY = 0;
    else
        PadY = 1;
        NY   = NY + 1;
    end
    
    Image = padarray(Image,[PadX PadY],0,'post');

    % Kernel for X dimension
    OperX = fft([0 1 zeros(1,NX-2)]);
    KernelX = fftshift(exp(1i.*DX(:).*phase(OperX)), 2);
    KernelX = KernelX./KernelX(1);
    KernelX(NX.*0.5+1) = 1;
    %KernelX = ifft(KernelX);

    % Kernel for Y dimension
    OperY = fft([0 1 zeros(1,NY-2)]);
    KernelY = fftshift(exp(1i.*DY(:).*phase(OperY)),1).';
    KernelY = KernelY./KernelY(1);
    KernelY(NY.*0.5+1) = 1;
    %KernelY = ifft(KernelY);

    
    SX = ifft( fft(Image,[],2) .*KernelX ,[],2);
    % need to take the real part as there is some residual imaginary
    % part due to computer precision errors
    ShiftedImage=real(ifft( fft(SX,[],1).* KernelY,[],1));
    
    %ShiftedImage=real(ifft( bsxfun(@times,fft(SX,[],1), KernelY) ,[],1));
    
    if (PadX==1)
        ShiftedImage = ShiftedImage(:,1:end-1,:);
    end
    if (PadY==1)
        ShiftedImage = ShiftedImage(1:end-1,:,:);
    end
    Nr = [];
    Nc = [];
    
    
    
elseif Algo==1
    % new
  
    [NY,NX] = size(Image);
    if (0.5*NX==floor(0.5*NX))
        % NX is even 
        PadX = 0;
    else
        PadX = 1;
        NX   = NX + 1;
    end
    if (0.5*NY==floor(0.5*NY))
        % NX is even 
        PadY = 0;
    else
        PadY = 1;
        NY   = NY + 1;
    end
    Image = padarray(Image,[PadX PadY],0,'post');

    % Kernel for X dimension
    OperX = fft([0 1 zeros(1,NX-2)]);
    KernelX = fftshift(exp(1i.*DX.*phase(OperX)));
    KernelX = KernelX./KernelX(1);
    KernelX(NX.*0.5+1) = 1;
    %KernelX = ifft(KernelX);

    % Kernel for Y dimension
    OperY = fft([0 1 zeros(1,NY-2)]);
    KernelY = fftshift(exp(1i.*DY.*phase(OperY))).';
    KernelY = KernelY./KernelY(1);
    KernelY(NY.*0.5+1) = 1;
    %KernelY = ifft(KernelY);

    
    SX = ifft( bsxfun(@times,fft(Image,[],2),KernelX) ,[],2);
    % need to take the real part as there is some residual imaginary
    % part due to computer precision errors
    ShiftedImage=real(ifft( bsxfun(@times,fft(SX,[],1), KernelY) ,[],1));
    
    %Kernel = KernelY*KernelX;
    %fftKernel = KernelY*KernelX;
    
    %ShiftedImage = real(ifft2(fft2(Image).*fft2(Kernel)));
    %ShiftedImage = real(ifft2(fft2(Image).*fftKernel));
    if (PadX==1)
        ShiftedImage = ShiftedImage(:,1:end-1);
    end
    if (PadY==1)
        ShiftedImage = ShiftedImage(1:end-1,:);
    end
    Nr = [];
    Nc = [];
elseif Algo==2
    % new / without the padding
  
    [NY,NX] = size(Image);
   
    % Kernel for X dimension
    OperX = fft([0 1 zeros(1,NX-2)]);
    KernelX = fftshift(exp(1i.*DX.*phase(OperX)));
    KernelX = KernelX./KernelX(1);
    KernelX(floor(NX.*0.5+1)) = 1;
    %KernelX = ifft(KernelX);

    % Kernel for Y dimension
    OperY = fft([0 1 zeros(1,NY-2)]);
    KernelY = fftshift(exp(1i.*DY.*phase(OperY))).';
    KernelY = KernelY./KernelY(1);
    KernelY(floor(NY.*0.5+1)) = 1;
    %KernelY = ifft(KernelY);

    
    %SX = ifft( bsxfun(@times,fft(Image,[],2),KernelX) ,[],2);
    SX = ifft(fft(Image,[],2).*KernelX, [], 2);
    
    % need to take the real part as there is some residual imaginary
    % part due to computer precision errors
    %ShiftedImage=real(ifft( bsxfun(@times,fft(SX,[],1), KernelY) ,[],1));
    ShiftedImage=real(ifft( fft(SX,[],1).*KernelY ,[],1));
    
    Nr = [];
    Nc = [];
    
elseif Algo==3
    % new / without the padding / for cube
  
    [NY,NX, Nim] = size(Image);  % must ask for Nim, otherwise wrong results
   
    DX = DX(:);
    DY = DY(:);
    
    % Kernel for X dimension
    OperX = fft([0 1 zeros(1,NX-2)]);
%     KernelX = fftshift(exp(1i.*DX.*phase(OperX)),2);
    KernelX = fftshift(exp(1i.*DX.*unwrap(angle(OperX))),2);

    KernelX = KernelX./KernelX(:,1);
    KernelX(:,floor(NX.*0.5+1)) = 1;
    %KernelX = ifft(KernelX);

    % Kernel for Y dimension
    OperY = fft([0 1 zeros(1,NY-2)]);
%     KernelY = fftshift(exp(1i.*DY.*phase(OperY)),2);
    KernelY = fftshift(exp(1i.*DY.*unwrap(angle(OperY))),2);

    KernelY = KernelY./KernelY(:,1);
    KernelY(:,floor(NY.*0.5+1)) = 1;
    %KernelY = ifft(KernelY);
    KernelY = KernelY.';
    
    KernelX = permute(KernelX,[3 2 1]);  % e.g., size is 1x15x2
    KernelY = permute(KernelY,[1 3 2]);  %e.g., size is 15x1x2
    
    %SX = ifft( bsxfun(@times,fft(Image,[],2),KernelX) ,[],2);
    SX = ifft(fft(Image,[],2).*KernelX, [], 2);
    
    % need to take the real part as there is some residual imaginary
    % part due to computer precision errors
    %ShiftedImage=real(ifft( bsxfun(@times,fft(SX,[],1), KernelY) ,[],1));
    ShiftedImage=real(ifft( fft(SX,[],1).*KernelY ,[],1));
    
    Nr = [];
    Nc = [];
    
    
elseif Algo==4
    % old
    
    %function [ShiftedImage,NY,NX,Nr,Nc]=image_shift_fft(Image,DX,DY,NY,NX,Nr,Nc)
    % old algorithm - better
    Phase = 2;

    % add bias to avoid negative numbers
    MinVal = min(Image(:))+1;
    %MinVal = 0;
    Image  = Image + MinVal;

    % [NY1,NX1] = size(Image);
    % Image = padarray(Image,[NY1 NX1],0,'both');
    % [NY,NX,Nim] = size(Image);

    Nim = 1;
    if (nargin<4)
        % NY, NX, Nr, Nc are not provided by user
        [NY,NX,Nim] = size(Image);

        Nr = ifftshift((-fix(NY/2):ceil(NY/2)-1));
        Nc = ifftshift((-fix(NX/2):ceil(NX/2)-1));
        [Nc,Nr] = meshgrid(Nc,Nr);
    end

    % Fourier Transform shift theorem
    if (Nim==1)  
        ShiftedImage = ifft2(fft2(Image).*exp(-1i.*2.*pi.*(DY.*Nr./NY + DX.*Nc./NX))).*exp(-1i.*Phase);
    else
        % Image is cube
        error('Cube images not supported yet');
        %ShiftedImage = ifft2(fft2(Image).*exp(-1i.*2.*pi.*(  bsxfun(@times,DY,shiftdim(Nr,-1))./NY + bsxfun(@times,DX,shiftdim(Nc,-1))./NX))).*exp(-1i.*Phase);
    end

    % add bias value to image
    ShiftedImage = abs(ShiftedImage) - MinVal;
    %ShiftedImage = ShiftedImage(NY1+1:2*NY1, NX1+1:2*NX1);
    
elseif Algo == 5
    
    [NY,NX,Nim] = size(Image);
    
    % FFT of the image
    Image_fft = fft2(Image); 

    % Create the Fourier space coordinates (u, v)
    [u, v] = meshgrid(0:(NX-1), 0:(NY-1));

    % Shift the frequencies to be centered
    u = ifftshift(u - floor(NX/2));
    v = ifftshift(v - floor(NY/2));

    % Compute the phase shift using the Fourier shift theorem and shift the image
    PhaseShift = exp(-1i * 2 * pi * (u * DX / NX + v * DY / NY));
    Image_fft_shifted = Image_fft .* PhaseShift;
        
    % Create a low-pass filter
    Sigma  = 10;  % adjust the sigma for smoothness; lower sigma = smoother image
    Filter = exp(-(u.^2 + v.^2) / (2 * Sigma^2)); % a Gaussian filter (properly normalized!)
    
    % Filter the shifted image (in the frequency domain):
    Image_fft_shifted_filtered = Image_fft_shifted .* Filter;
    % this does not work by itself, need to dig? 
%     Image_fft_shifted_filtered = imUtil.psf.suppressEdges(Image_fft_shifted, 'Fun',@imUtil.kernel2.cosbell, 'FunPars', [5, 8]);

    % Pad the image with zeros in the frequency domain:
    % (this is for odd-sized Nx = Ny matrices only!)
    Nzer = NX; % number of additional rows and columns
    Nnew = NX+Nzer;
    Nh   = (NX+1)/2;
    Image_fft_shifted_filtered_padded = repmat(0,Nnew,Nnew);
    Image_fft_shifted_filtered_padded(1:Nh,1:Nh)                 = Image_fft_shifted_filtered(1:Nh,1:Nh);
    Image_fft_shifted_filtered_padded(Nh+Nzer:Nnew,1:Nh)         = Image_fft_shifted_filtered(Nh:NX,1:Nh);
    Image_fft_shifted_filtered_padded(1:Nh,Nh+Nzer:Nnew)         = Image_fft_shifted_filtered(1:Nh,Nh:NX);
    Image_fft_shifted_filtered_padded(Nh+Nzer:Nnew,Nh+Nzer:Nnew) = Image_fft_shifted_filtered(Nh:NX,Nh:NX);
    
    % Inverse FFT to get the shifted and filtered image
    % Take the real part, since ifft2 may introduce a small imaginary part due to numerical precision
    ShiftedImage = real(ifft2(Image_fft_shifted_filtered));  
    
    ShiftedImage_padded = 4.*imresize(real(ifft2(Image_fft_shifted_filtered_padded)),0.5);
    
else
    error('Unknown Algo');
end
