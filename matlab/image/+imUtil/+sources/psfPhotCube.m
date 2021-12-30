function psfPhotCube(Cube, Args)
    %
    % Example: P=imUtil.kernel2.gauss;
    %          Ps=imUtil.trans.shift_fft(P,0.4,0.7);
    %          imUtil.sources.psfPhotCube(Ps, 'PSF',P)
   
    arguments
        Cube
        Args.Xinit      = [];
        Args.Yinit      = [];
        Args.PSF        = 1.5;  % scalar will generate a Gaussian PSF - normalized to 1
        Args.MaxIter    = 10;
        Args.Std        = 0.001;   % vector or cube
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
    
    if isempty(Args.Xinit)
        Args.Xinit = Xcenter;
    end
    if isempty(Args.Yinit)
        Args.Yinit = Ycenter;
    end
    
    if numel(Args.PSF)==1
        Args.PSF = imUtil.kernel2.gauss(Args.PSF);
    end
    
    % measure and subtract background
    
    
    WeightedPSF = sum(Args.PSF.^2, [1 2]); % for flux estimation
    
    X = Args.Xinit;
    Y = Args.Yinit;
    
    StepX = 0;
    StepY = 0;
    DX = X - Xcenter + StepX;
    DY = Y - Ycenter + StepY;
        
    VecD = [0, Args.SmallStep, 2.*Args.SmallStep];
    Ind   = 0;
    NotConverged = true;
    while Ind<Args.MaxIter && NotConverged
        Ind = Ind + 1;
        
        %Xcenter + StepX
        %Ycenter + StepY
        DX
        DY
        
        % calc \chi2 and gradient
        Chi2     = internalCalcChi2(Cube, Std, Args.PSF, DX,                   DY,                WeightedPSF);
        Chi2_Dx  = internalCalcChi2(Cube, Std, Args.PSF, DX+Args.SmallStep,    DY,                WeightedPSF);
        Chi2_Dx2 = internalCalcChi2(Cube, Std, Args.PSF, DX+Args.SmallStep.*2, DY,                WeightedPSF);
        ParX     = polyfit(VecD, [Chi2, Chi2_Dx, Chi2_Dx2], 2);
        DX       = DX -ParX(2)./(2.*ParX(1));
        
        %Chi2     = internalCalcChi2(Cube, Std, Args.PSF, DX, DY,                  WeightedPSF);
        Chi2_Dy  = internalCalcChi2(Cube, Std, Args.PSF, DX, DY+Args.SmallStep,   WeightedPSF);
        Chi2_Dy2 = internalCalcChi2(Cube, Std, Args.PSF, DX, DY+Args.SmallStep.*2,WeightedPSF);
        ParY     = polyfit(VecD, [Chi2, Chi2_Dy, Chi2_Dxy], 2);
        DY       = DY -ParY(2)./(2.*ParY(1));
        
        
        Chi2_Dy = internalCalcChi2(Cube, Std, Args.PSF, DX,                DY+Args.SmallStep, WeightedPSF);
        
        Chi2mDof = (Chi2 - Dof).*Args.SmallStep;
        FlagNeg  = Chi2mDof < 0;
        Chi2mDof(FlagNeg) = abs(Chi2mDof).*0.5;
        
        StepX = min(Chi2mDof./(Chi2 - Chi2_Dx), Args.MaxStep);
        StepY = min(Chi2mDof./(Chi2 - Chi2_Dy), Args.MaxStep);
        DX    = DX + StepX;
        DY    = DY + StepY;
        
        % stoping criteria
        %Ind
        %StepX
        %StepY
        
        
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
