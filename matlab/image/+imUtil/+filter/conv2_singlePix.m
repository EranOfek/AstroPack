function conv2_singlePix(Image, Kernel, PosIJ)
    %
    
    % Example: [MatX, MatY] = meshgrid((1:1:100),(1:1:100));
    %          Image = MatX.*MatY + randn(100,100).*0.01;
    %          Image(98,2) = NaN;
    %          Kernel     = ones(11,11)./121;
    
    PosIJ = [98 2];
    
    Conv2 = Image;
        
    SizeIm    = size(Image); 
    SizeK     = size(Kernel);
    HalfSizeK = floor(SizeK.*0.5);
    
    Iim1 = PosIJ(1) - HalfSizeK(1); % + 1;
    Iim2 = PosIJ(1) + HalfSizeK(1); % - 1;
    Ik1  = 1;
    Ik2  = SizeK(1);
    if Iim1<1
        Iim1 = 1;
        Ik1  = SizeK(1) - PosIJ(1) - 1;
    end
    
    if Iim2>SizeIm(1)
        Iim2 = SizeIm(1);
        Ik2  = SizeK(1) - (SizeIm(1)-PosIJ(1)) - 1;
    end
    
    Jim1 = PosIJ(2) - HalfSizeK(2); % + 1;
    Jim2 = PosIJ(2) + HalfSizeK(2);
    Jk1  = 1;
    Jk2  = SizeK(2);
    if Jim1<1
        Jim1 = 1;
        Jk1  = HalfSizeK(2)+1 - (PosIJ(2)-1);
    end
    
    if Jim2>SizeIm(2)
        Jim2 = SizeIm(2);
        Jk2  = SizeK(2) - (SizeIm(2)-PosIJ(2));
    end
    
    %[Iim1, Iim2]
    %[Ik1, Ik2]
    
    %Ik2 - Ik1
    %Iim2 - Iim1
    
    SubIm     = Image(Iim1:Iim2, Jim1:Jim2);
    SubKernel = Kernel(Ik1:Ik2, Jk1:Jk2);
    
    Sum2 = sum(SubIm.*SubKernel, 'all', 'omitnan');
    IsN = isnan(SubIm);
    SubKernel(IsN) = NaN;
    Norm2 = sum(SubKernel, 'all', 'omitnan');
    
    Conv2(PosIJ(1), PosIJ(2)) = Sum2./Norm2;
    
    
    
    
end