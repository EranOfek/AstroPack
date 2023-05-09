function traceLocalMax(Image, Args)
    %
    % 
    
    arguments
        Image
        StartPos         = [];
        Args.DimWave     = 2;
        Args.BinSize     = [1 10];
        Args.BinFun      = 'medfilt2';  % 'medfilt2' | 'conv2' | 'max'
        Args.MaxShift    = 2;
    end
    
    if Args.DimWave==1
        Image = Image.';
    end
    
    [Nspat, Nwave] = size(Image);
    
    switch Args.BinFun
        case 'medfilt2'
            BinImage = medfilt2(Image, Args.BinSize);
        case 'conv2'
            BinImage = conv2(Image, ones(Args.BinSize)./prod(Args.BinSize));
        case 'max'
            BinImage = Image;
        otherwise
            error('Unknown BinFun option');
    end
    
    
            
    
    
    
    
   
    
    
    
    
end
