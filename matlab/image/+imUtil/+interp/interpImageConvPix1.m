function Conv2 = interpImageConvPix1(Image, Kernel, PosIJ)
    % Interpolate over nan using 2D local convolution.
    %   OBSOLETE: use: imUtil.interp.interpImageConvPix
    % Input  : - 
    % Example: [MatX, MatY] = meshgrid((1:1:100),(1:1:100));
    %          Image = MatX.*MatY + randn(100,100).*0.01;
    %          Image1 = Image;
    %          Kernel     = imUtil.kernel2.gauss; %ones(11,11)./121;
    %          I = [3;99;45;46;60]; J = [4;2;23;23;99];
    %          K = sub2ind(size(Image),I,J);
    %          Image1(K) = NaN;
    %          Conv2 = imUtil.filter.interpImageConvPix(Image1, Kernel);
    %          [Conv2(K), Image(K)]
    %
    %          Image = rand(1000,1000);
    %          K = randi(1e6,1000,1);
    %          Image(K) = NaN;
    %          Kernel     = imUtil.kernel2.gauss; %ones(11,11)./121;
    %          Conv2 = imUtil.interp.interpImageConvPix1(Image, Kernel);
    
    
    arguments
        Image
        Kernel
        PosIJ     = [];
    end
    
    if isempty(PosIJ)
        %[Inan, Jnan] = find(isnan(Image));
        K = find(isnan(Image));
        [Inan, Jnan] = imUtil.image.ind2sub_fast(size(Image), K);
        
        % a bit slower
        %ISnan = find(isnan(Image));
        %[Inan, Jnan] = imUtil.image.ind2sub_fast(size(Image), ISnan);
        
        PosIJ = [Inan Jnan];
    end
    
    Conv2 = Image;
        
    SizeIm    = size(Image); 
    SizeK     = size(Kernel);
    HalfSizeK = floor(SizeK.*0.5);
    
    Npos = size(PosIJ, 1);
    for Ipos=1:1:Npos
    
        Iim1 = PosIJ(Ipos, 1) - HalfSizeK(1); % + 1;
        Iim2 = PosIJ(Ipos, 1) + HalfSizeK(1); % - 1;
        Ik1  = 1;
        Ik2  = SizeK(1);
        if Iim1<1
            Iim1 = 1;
            Ik1  = HalfSizeK(1) - PosIJ(Ipos, 1) + 2;
        end

        if Iim2>SizeIm(1)
            Iim2 = SizeIm(1);
            Ik2  = HalfSizeK(1) + (SizeIm(1)-PosIJ(Ipos, 1)) + 1;
        end

        Jim1 = PosIJ(Ipos, 2) - HalfSizeK(2); % + 1;
        Jim2 = PosIJ(Ipos, 2) + HalfSizeK(2);
        Jk1  = 1;
        Jk2  = SizeK(2);
        if Jim1<1
            Jim1 = 1;
            Jk1  = HalfSizeK(2) - PosIJ(Ipos, 2) + 2;
        end

        if Jim2>SizeIm(2)
            Jim2 = SizeIm(2);
            Jk2  = HalfSizeK(2) + (SizeIm(2)-PosIJ(Ipos, 2)) + 1;
        end

        %[Iim1, Iim2]
        %[Ik1, Ik2]

        %Ik2 - Ik1
        %Iim2 - Iim1

        SubIm      = Image(Iim1:Iim2, Jim1:Jim2);
        SubKernel  = Kernel(Ik1:Ik2, Jk1:Jk2);
        NormKernel = sum(SubKernel, 'all', 'omitnan');

        Sum2 = sum(SubIm.*SubKernel, 'all', 'omitnan');
        IsN = isnan(SubIm);
        SubKernel(IsN) = NaN;
        Norm2 = sum(SubKernel, 'all', 'omitnan');

        Conv2(PosIJ(Ipos, 1), PosIJ(Ipos, 2)) = Sum2./Norm2 .* NormKernel;
    end
    
end