function psfPhotCube(Cube, Args)
    %
   
    arguments
        Cube
        Args.Xinit      = [];
        Args.Yinit      = [];
        Args.PSF        = 1.5;  % scalar will generate a Gaussian PSF - normalized to 1
        Args.MaxIter    = 10;
        Args.Std        = [];   % vector or cube
        Args.SmallStep  = 1e-4;
        Args.MaxStep    = 1;
    end
    
    if ndims(Args.Std)<3
        Std = permute(Args.Std(:),[3 2 1]);
    else
        Std = Args.Std;
    end
    
    
    [Ny, Nx, Nim] = size(Cube);
    Xcenter = Nx.*0.5 + 0.5;
    Ycenter = Ny.*0.5 + 0.5;
    Dof     = Nx.*Ny - 3;
    
    if numel(Args.PSF)==1
        Args.PSF = imUtil.kernel2.gauss(Args.PSF);
    end
    
    % measure and subtract background
    
    
    WeightedPSF = sum(Args.PSF.^2, [1 2]); % for flux estimation
    
    X = Args.Xinit;
    Y = Args.Yinit;
    
    StepX = 0;
    StepY = 0;
    Ind   = 0;
    NotConverged = true;
    while Ind<Args.MaxIter && NotConverged
        Ind = Ind + 1;
        
        DX = X - Xcenter + StepX;
        DY = Y - Ycenter + StepY;
        
        % calc \chi2 and gradient
        Chi2    = internalCalcChi2(Cube, Std, PSF, DX,                DY,                WeightedPSF);
        Chi2_Dx = internalCalcChi2(Cube, Std, PSF, DX+Args.SmallStep, DY,                WeightedPSF);
        Chi2_Dx = internalCalcChi2(Cube, Std, PSF, DX,                DY+Args.SmallStep, WeightedPSF);
        
        Chi2mDof = (Chi2 - Dof).*Args.SmallStep;
        FlagNeg  = Chi2mDof < 0;
        Chi2mDof(FlagNeg) = abs(Chi2mDof).*0.5;
        
        StepX = min(Chi2mDof./(Chi2 - Chi2_Dx), Args.MaxStep);
        StepY = min(Chi2mDof./(Chi2 - Chi2_Dy), Args.MaxStep);
        
        % stoping criteria
        
        
    end
    
    
    
end

function Chi2 = internalCalcChi2(Cube, Std, PSF, DX, DY, WeightedPSF)
    % Return Chi2 for specific PSF and Cube
    % shift PSF
    ShiftedPSF = imUtil.trans.shift_fft(PSF, DX, DY);
    WeightedFlux = sum(Cube.*ShiftedPSF, [1 2], 'omitnan')./WeightedPSF;
    Resid = Cube - WeightedFlux.*ShiftedPSF;
    
    % search / remove outliers

    Chi2  = sum( (Resid./Std).^2, [1 2], 'omitnan');
end
