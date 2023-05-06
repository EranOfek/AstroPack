function moment1d(Array, Pos, Args)
    %
    
    arguments
        Array
        Pos
        Args.Dim = 1;
        Args.WinHalfSize = 7;
    end
    
    if Args.Dim==2
        Array = Array.';
    end
    
    [Nspat, Nwave] = size(Array);
    if numel(Pos)==1
        Pos = Pos.*ones(1, Nwave);
    end
    
    if numel(Pos)~=Nwave
        error('Number of Pos elemenst must be equal to the number of wavelength');
    end
    
    RoundedPos = round(Pos);
    
    Cutout = specUtil.trace.image2cutouts1d(R, Pos, 'Dim',1, 'WinHalfSize',Args.WinHalfSize);
    
    
    
    
    
end
    