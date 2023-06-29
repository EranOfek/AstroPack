function psfChi2_RangeIter(Cube, Std, PSF, DX, DY, Args)
    %
    
    arguments
        Cube
        Std
        PSF
        Args.WeightedPSF     = [];
        Args.FitRadius2      = [];
        Args.VecXrel         = [];
        Args.VecYrel         = [];
        Args.SumArgs cell    = {'omitnan'};
        
        Args.HalfRange       = 1;
        Args.GridPointsX     = cosd((0:60:359));
        Args.GridPointsY     = sind((0:60:359));
        Args.H               = [];
    end
    
    
    if isempty(Args.WeightedPSF)
        Args.WeightedPSF = sum(PSF.^2, [1 2]); % for flux estimation
    end
    
    if isempty(Args.VecXrel)
        % assume bith VecXrel and VecYrel are empty:
    
        [Ny, Nx, Nim] = size(Cube);
        Xcenter = Nx.*0.5 + 0.5;
        Ycenter = Ny.*0.5 + 0.5;
        Dof     = Nx.*Ny - 3;
    
        VecXrel = (1:1:Nx) - Xcenter;
        VecYrel = (1:1:Ny) - Ycenter;
    end
    
    
    
    if numel(Args.DX)==1 && Args.DX==0
        DX = [];
        DY = [];
    else
        DX = Args.DX;
        DY = Args.DY;
    end
    
    Args.GridPointX = [0, Args.GridPointX];
    Args.GridPointY = [0, Args.GridPointY];
    
    Npoint = numel(Args.GridPointX);
    
    [Chi2, Flux, Dof] = imUtil.psf.psfChi2(Cube, Std, PSF,...
                                           'DX',DX,...
                                           'DY',DY,...
                                           'MinFlux',Args.MinFlux,...
                                           'WeightedPSF',Args.WeightedPSF,...
                                           'FitRadius2',Args.FitRadius2,...
                                           'VecXrel',Args.VecXrel,...
                                           'VecYrel',Args.VecYrel,...
                                           'SumArgs',Args.SumArgs);
                                                               
    Nsrc = numel(Chi2);
    MatChi2 = zeros(Npoint, Nsrc);
    MatChi2(1,:) = Chi2(:).';
    for Igrid=2:1:Npoint
        
        [Chi2, Flux] = imUtil.psf.psfChi2(Cube, Std, PSF,...
                                           'DX',DX + Args.GridPointX(Igrid),...
                                           'DY',DY + Args.GridPointY(Igrid),...
                                           'MinFlux',Args.MinFlux,...
                                           'WeightedPSF',Args.WeightedPSF,...
                                           'FitRadius2',Args.FitRadius2,...
                                           'VecXrel',Args.VecXrel,...
                                           'VecYrel',Args.VecYrel,...
                                           'SumArgs',Args.SumArgs);
        MatChi2(Igrid,:) = Chi2(:).';
    end                                                                     
  
    % fot chi^2 surface for all sources
    
   
    
    
    
                                                           
    if isempty(Args.DX)
        % assume both DX and DY are empty
        Args.DX = 0;
        Args.DY = 0;
        ShiftedPSF = PSF;
    else
        ShiftedPSF = imUtil.trans.shift_fft(PSF, Args.DX, Args.DY);
    end
    
    
    
end