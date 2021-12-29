function psfPhotCube(Cube, Args)
    %
   
    arguments
        Cube
        Args.Xinit   = [];
        Args.Yinit   = [];
        Args.PSF     = 1.5;  % scalar will generate a Gaussian PSF - normalized to 1
        Args.MaxIter = 10;
    end
    
    [Ny, Nx, Nim] = size(Cube);
    Xcenter = Nx.*0.5 + 0.5;
    Ycenter = Ny.*0.5 + 0.5;
    
    if numel(Args.PSF)==1
        Args.PSF = imUtil.kernel2.gauss(Args.PSF);
    end
    
    % measure and subtract background
    
    
    WeightedPSF = sum(Args.PSF.^2, [1 2]); % for flux estimation
    
    X = Args.Xinit;
    Y = Args.Yinit;
    
    Ind = 0;
    NotConverged = true;
    while Ind<Args.MaxIter && NotConverged
        Ind = Ind + 1;
        
        DX = X - Xcenter;
        DY = Y - Ycenter;
        
        Chi2 = internalCalcChi2(Cube, PSF, DX, DY, WeightedPSF);
        
        % calc Chi2 gradient
        
    
        
    end
    
    
    
end

function Chi2 = internalCalcChi2(Cube, PSF, DX, DY, WeightedPSF)
    % Return Chi2 for specific PSF and Cube
    % shift PSF
    ShiftedPSF = imUtil.trans.shift_fft(PSF, DX, DY);
    WeightedFlux = sum(Cube.*ShiftedPSF, [1 2], 'omitnan')./WeightedPSF;
    Resid = Cube - WeightedFlux.*ShiftedPSF;
    
    % search / remove outliers

    Chi2  = sum( (Resid./Std).^2, [1 2], 'omitnan');
end
