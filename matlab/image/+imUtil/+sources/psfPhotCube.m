function Result = psfPhotCube(Cube, Args)
    % 
    % Example: P=imUtil.kernel2.gauss;
    %          Ps=imUtil.trans.shift_fft(P,0.4,0.7);
    %          imUtil.sources.psfPhotCube(Ps, 'PSF',P)
    %          P=imUtil.kernel2.gauss(1.5.*ones(4,1));
    %          Ps=imUtil.trans.shift_fft(P,[0.4;0.7;-1.1;3.6],[0.7;-0.2;-0.9;-2.6]);
    %          Ps = Ps.*permute([100 110 200 300],[1 3 2]) + randn(15,15);
    %          Result = imUtil.sources.psfPhotCube(Ps, 'PSF',P(:,:,1))
   
    arguments
        Cube
        Args.Xinit      = [];
        Args.Yinit      = [];
        Args.PSF        = 1.5;  % scalar will generate a Gaussian PSF - normalized to 1
        Args.MaxIter    = 10;
        Args.Std        = 1;   % vector or cube
        Args.SmallStep  = 1e-4;
        Args.MaxStep    = 1;
        Args.ConvThresh = 1e-4;
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
    H    = VecD.'.^[0, 1, 2];
    Ind   = 0;
    NotConverged = true;
    while Ind<Args.MaxIter && NotConverged
        Ind = Ind + 1;
       
        % calc \chi2 and gradient
        Chi2     = internalCalcChi2(Cube, Std, Args.PSF, DX,                   DY,                WeightedPSF);
        Chi2_Dx  = internalCalcChi2(Cube, Std, Args.PSF, DX+Args.SmallStep,    DY,                WeightedPSF);
        Chi2_Dx2 = internalCalcChi2(Cube, Std, Args.PSF, DX+Args.SmallStep.*2, DY,                WeightedPSF);
        
        %ParX     = polyfit(VecD, [Chi2, Chi2_Dx, Chi2_Dx2], 2);
        ParX     = H\[Chi2.'; Chi2_Dx.'; Chi2_Dx2.'];
        
        %Chi2     = internalCalcChi2(Cube, Std, Args.PSF, DX, DY,                  WeightedPSF);
        Chi2_Dy  = internalCalcChi2(Cube, Std, Args.PSF, DX, DY+Args.SmallStep,   WeightedPSF);
        Chi2_Dy2 = internalCalcChi2(Cube, Std, Args.PSF, DX, DY+Args.SmallStep.*2,WeightedPSF);
        
        %ParY     = polyfit(VecD, [Chi2, Chi2_Dy, Chi2_Dy2], 2);
        ParY     = H\[Chi2.'; Chi2_Dy.'; Chi2_Dy2.'];
        
        StepX    = -ParX(2,:)./(2.*ParX(3,:));
        StepY    = -ParY(2,:)./(2.*ParY(3,:));
        
        NotMinimaX = ParX(3,:)<0;
        NotMinimaY = ParY(3,:)<0;
        
        % reverse sign for maxima...
        StepX(NotMinimaX) = -StepX(NotMinimaX);
        StepY(NotMinimaY) = -StepY(NotMinimaY);
        
        
        StepX    = sign(StepX).*min(abs(StepX), Args.MaxStep);
        StepY    = sign(StepY).*min(abs(StepY), Args.MaxStep);
        
        DX       = DX + StepX;
        DY       = DY + StepY;
         
        % stoping criteria
        if max(abs(StepX))<Args.ConvThresh && max(abs(StepX))<Args.ConvThresh
            NotConverged = false;
        end
        
        
    end
    % final fit and return flux
    [Result.Chi2, Result.Flux]  = internalCalcChi2(Cube, Std, Args.PSF, DX,                   DY,                WeightedPSF);
    
    Result.DX = DX(:);
    Result.DY = DY(:);
    Result.Xinit = Args.Xinit;
    Result.Yinit = Args.Yinit;
    Result.Xcenter = Xcenter;
    Result.Ycenter = Ycenter;
    
    
end

function [Chi2,WeightedFlux] = internalCalcChi2(Cube, Std, PSF, DX, DY, WeightedPSF)
    % Return Chi2 for specific PSF and Cube
    % shift PSF
    ShiftedPSF = imUtil.trans.shift_fft(PSF, DX, DY);
    WeightedFlux = sum(Cube.*ShiftedPSF, [1 2], 'omitnan')./WeightedPSF;
    Resid = Cube - WeightedFlux.*ShiftedPSF;
    
    % search / remove outliers

    Chi2  = sum( (Resid./Std).^2, [1 2], 'omitnan');
    Chi2  = squeeze(Chi2);
    
    if nargout>1
        WeightedFlux = squeeze(WeightedFlux);
    end
end
