function [ShiftedImage]=shift_fft1(Image, DY, Dim)
    % 1-D sub pixel shift using sinc (Shannon) interpolation.
    %   Given a 1-D vector or a 2-D image, shift columns (Dim=1) or rows
    %   (Dim=2) using sinc (Shannon) interpolation.
    % Package: imUtil.trans
    % Input  : - A vector or 2-D matrix.
    %          - A scalar or vector of shift to apply to input image, along the
    %            required dimension.
    %          - Dimension along to perform the shift.
    %            1 will apply the shift along the columns.
    %            Default is 1.
    % Output : - Shifted data.
    % Author : Eran Ofek (May 2023)
    % See also: imUtil.trans.shift_fft (2-D shifts)
    % Example: Image=normpdf((-12:1:12)',0,2)*ones(1,1000);
    %          DY=(1:1:1000).*0.003;
    %          ShiftedImage=imUtil.trans.shift_fft1(Image,DY);
    %          ShiftedImage=imUtil.trans.shift_fft1(Image,3);
    
    arguments
        Image
        DY
        Dim     = 1;
    end
    
    if Dim==2
        Image = Image.';
    end
    
    [NY,Nim] = size(Image);  % must ask for Nim, otherwise wrong results
   
    DY = DY(:).';
    
    OperY = fft([0 1 zeros(1,NY-2)]).';
    KernelY = fftshift(exp(1i.*DY.*unwrap(angle(OperY))),1);

    KernelY = KernelY./KernelY(1,:);
    KernelY(floor(NY.*0.5+1),:) = 1;
        
    ShiftedImage=real(ifft( fft(Image,[],1).*KernelY ,[],1));
    
end
