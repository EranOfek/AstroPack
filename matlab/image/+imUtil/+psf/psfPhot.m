function Result = psfPhot(Cube, Args)
    %
    % Example: imUtil.psf.psfPhot
   
    arguments
        Cube
        Args.SubBack logical          = true;
        Args.Std                      = [];   % vector or cube
        Args.Back                     = [];
        Args.backgroundCubeArgs cell  = {};
        
        Args.FitRadius                = 3;
        Args.Xinit                    = [];
        Args.Yinit                    = [];
        Args.PsfPeakVal               = [];
        
        Args.PSF                      = [];   % PSF stamp - if given override fun
        Args.PSFFun                   = @imUtil.kernel2.gauss;   % alternative PSF function
        Args.PSFFunArgs cell          = {2};
        
        % imUtil.psf.psfChi2_RangeIter
        Args.RadiusRange     = 1;
        Args.MaxStep_RadiusRangeUnits = 1;
        Args.GridPointsX     = cosd((0:60:359));
        Args.GridPointsY     = sind((0:60:359));
        
        Args.MaxIter = 20;
        Args.UseSourceNoise logical = true;
        
        Args.SN = [];  % is this needed?
        Args.ConvThresh = 1e-4;
        
        Args.ZP         = 25; 
    end
    
    if isempty(Args.PSF)
        Args.PSF = Args.PSFFun(Args.PSFFunArgs{:});
    end
    RadiusPSF = (size(Args.PSF,1)-1).*0.5;
    
    % treat the case of a single input image - convert to Cube
    if ismatrix(Cube) && ~isempty(Args.Xinit) && ~isempty(Args.Yinit)
        % Input is a matrix
        % select stamps around Xinit, Yint
        [Cube, RoundX, RoundY, X, Y] = imUtil.cut.image2cutouts(Cube, Args.Xinit, Args.Yinit, RadiusPSF);
    end
    
    
    % Calculate the coordinates of PSF centers
    [Ny, Nx, Nim] = size(Cube);
    Xcenter = (Nx+1).*0.5;
    Ycenter = (Ny+1).*0.5;
    Dof     = Nx.*Ny - 3;
    
    VecXrel = (1:1:Nx) - Xcenter;
    VecYrel = (1:1:Ny) - Ycenter;
    
    if isempty(Args.Xinit)
        Args.Xinit = Xcenter;
    end
    if isempty(Args.Yinit)
        Args.Yinit = Ycenter;
    end
    
    if isempty(Args.PsfPeakVal)
        Args.PsfPeakVal = squeeze(Cube(Args.Xinit, Args.Yinit, :));
    end
    
    
    if Args.SubBack || Args.UseSourceNoise
        if isempty(Args.Back) || isempty(Args.Std)
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
            
        if Args.SubBack    
            % subtract background
            Cube = Cube - Back;
        end
    end
    
    FitRadius2 = Args.FitRadius.^2;
        
    %% got here
    
    % adaptive conversion threshold 
    if isempty(Args.SN)
        ConvThresh = Args.ConvThresh;
    else
        ConvThresh = 0.5./Args.SN;
    end

    WeightedPSF = sum(Args.PSF.^2, [1 2]); % for flux estimation
    
    X = Args.Xinit;
    Y = Args.Yinit;
    
    StepX = 0;
    StepY = 0;
    DX = X - Xcenter + StepX;
    DY = Y - Ycenter + StepY;
        
%    AdditionalIter=false;
%    UseSourceNoise=false;
%     switch lower(Args.UseSourceNoise)
% 
%         case 'all'
%             UseSourceNoise=true;
%         case 'off'
%             UseSourceNoise=false;
%         case 'last'
%             AdditionalIter = true;
%             UseSourceNoise=false;
%     end

    %VecD  = [0, Args.SmallStep, 2.*Args.SmallStep];
    H     = []; %VecD.'.^[0, 1, 2];
    Ind   = 0;
    NotConverged = true;
    StdBack = Std;
    Flux0   = 0;
    
    X1 = 0;
    Y1 = 0;
    RadiusRange = Args.RadiusRange;
    
    if Args.UseSourceNoise
        % Add source noise to Std
        % source noise can be treated as scalar or a matrix
        
        StdIter = sqrt(pi.*FitRadius2.*Std.^2 + permute(Args.PsfPeakVal(:),[3 2 1]));
    else
        StdIter = Std;
    end
    
    while Ind<Args.MaxIter && NotConverged
        
        Ind = Ind + 1;
            
        X1prev = X1;
        Y1prev = Y1;
        
        [X1,Y1,MinChi2,Flux0,Dof,H, Result] = imUtil.psf.psfChi2_RangeIter(Cube, StdIter, Args.PSF,...
                                                                           'DX',X1,...
                                                                           'DY',Y1',...
                                                                           'MinFlux',[],...
                                                                           'WeightedPSF',WeightedPSF,...
                                                                           'FitRadius2',FitRadius2,...
                                                                           'VecXrel',VecXrel,...
                                                                           'VecYrel',VecYrel,...
                                                                           'RadiusRange',RadiusRange,...
                                                                           'MaxStep_RadiusRangeUnits',Args.MaxStep_RadiusRangeUnits,...
                                                                           'GridPointsX',Args.GridPointsX,...
                                                                           'GridPointsY',Args.GridPointsY,...
                                                                           'H',H); 
        %
        RadiusRange = RadiusRange./2;
        
         [X1, Y1, sqrt(((X1 - X1prev).^2 + (Y1 - Y1prev).^2)), ((X1 - X1prev).^2 + (Y1 - Y1prev).^2)<ConvThresh.^2, Flux0/1e3, MinChi2./Dof]
         Ind % deb
        
        if all( ((X1 - X1prev).^2 + (Y1 - Y1prev).^2)<ConvThresh.^2)
            NotConverged = false;
        end
        
    end
    % final fit and return flux
    % do we really need to fit once more ? 
    
    Result.Flux = squeeze(Flux0);
    Result.SNm  = sign(Result.Flux).*abs(Result.Flux)./sqrt(abs(Result.Flux) + (squeeze(StdBack)).^2);  % S/N for measurments
    Result.Mag  = convert.luptitude(Result.Flux, 10.^(0.4.*Args.ZP));
    Result.DX = X1;
    Result.DY = Y1;
    Result.Xinit = Args.Xinit;
    Result.Yinit = Args.Yinit;
    Result.Xcenter = Xcenter;
    Result.Ycenter = Ycenter;
    Result.ConvergeFlag = 1-NotConverged;
    Result.Niter   = Ind;
    Result.Dof     = Dof;
    Result.Chi2    = MinChi2;
    
end