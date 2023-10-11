function psfPhot(Cube, Args)
    %
   
    arguments
        Cube
        Args.SubBack logical          = true;
        Args.Std                      = [];   % vector or cube
        Args.Back                     = [];
        Args.backgroundCubeArgs cell  = {};
        
        Args.FitRadius                = 3;
        Args.Xinit                    = [];
        Args.Yinit                    = [];
        
        Args.PSF                      = [];   % PSF stamp - if given override fun
        Args.PSFFun                   = @imUtil.kernel2.gauss;   % alternative PSF function
        Args.PSFFunArgs cell          = {2};
    end
    
    if Args.SubBack
        if isempty(Args.Back) || isempty(Args.Std)
            % calculate background and std
            [Back, Std] = imUtil.sources.backgroundCube(Cube, Args.backgroundCubeArgs{:}, 'Squeeze',false);
        else
            if ndims(Args.Back)<3
                Back = permute(Args.Back(:), [3 2 1]);
            else
                Back = Args.Back;
            end
            if ndims(Args.Std)<3
                Std = permute(Args.Std(:),[3 2 1]);
            else
                Std = Args.Std;
            end
        end

        % subtract background
        Cube = Cube - Back;
    end
    
    FitRadius2 = Args.FitRadius.^2;
    
    % Calculate the coordinates of PSF centers
    [Ny, Nx, Nim] = size(Cube);
    Xcenter = (Nx-1).*0.5;
    Ycenter = (Ny-1).*0.5;
    Dof     = Nx.*Ny - 3;
    
    VecXrel = (1:1:Nx) - Xcenter;
    VecYrel = (1:1:Ny) - Ycenter;
    
    if isempty(Args.Xinit)
        Args.Xinit = Xcenter;
    end
    if isempty(Args.Yinit)
        Args.Yinit = Ycenter;
    end
    
    if isempty(Args.PSF)
        Args.PSF = Args.PSFFun(Args.PSFFunArgs{:});
    end
    
    %% got here
    
    % adaptive conversion threshold 
    if isempty(Args.SN)
        ConvThresh = Args.ConvThresh;
    else
        ConvThresh = 0.1./Args.SN;
    end

    WeightedPSF = sum(Args.PSF.^2, [1 2]); % for flux estimation
    
    X = Args.Xinit;
    Y = Args.Yinit;
    
    StepX = 0;
    StepY = 0;
    DX = X - Xcenter + StepX;
    DY = Y - Ycenter + StepY;
        
    AdditionalIter=false;
    UseSourceNoise=false;
    switch lower(Args.UseSourceNoise)

        case 'all'
            UseSourceNoise=true;
        case 'off'
            UseSourceNoise=false;
        case 'last'
            AdditionalIter = true;
            UseSourceNoise=false;
    end

    VecD = [0, Args.SmallStep, 2.*Args.SmallStep];
    H    = VecD.'.^[0, 1, 2];
    Ind   = 0;
    NotConverged = true;
    StdBack = Std;
    while Ind<Args.MaxIter && NotConverged
        Ind = Ind + 1;
        
        if UseSourceNoise && Ind>1
            % Add source noise to Std
            % source noise can be treated as scalar or a matrix
            
            Std = Args.Std 
        end
            
        [X1,Y1,MinChi2,Flux0,Dof,H, Result] = imUtil.psf.psfChi2_RangeIter(Cube, Std, PSF, Args)
        
        
        if UseSourceNoise && Ind>2
            [~, Flux, ShiftedPSF]  = internalCalcChi2(Cube, Std, Args.PSF, DX, DY, WeightedPSF, VecXrel, VecYrel, FitRadius2);
            Std = sqrt(Flux.*ShiftedPSF+StdBack.^2);
        end
        % AppFlux is approximate flux
        [StepX,StepY,AppFlux]  = gradDescentPSF(Cube, Std, Args.PSF, DX, DY, WeightedPSF, VecXrel, VecYrel, FitRadius2,H,Args.SmallStep,Args.MaxStep);
       
        % stoping criteria
        ConvergeFlag = abs(StepX)<ConvThresh & abs(StepY)<ConvThresh;
        if all(ConvergeFlag)
            NotConverged = false;
        end
        
    end
    % final fit and return flux
    
end