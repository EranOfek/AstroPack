function [Result, CubePsfSub] = psfPhotCube(Cube, Args)
    % The core function for PSF-fitting photometry.
    %   The input of this function is a cube of stamps of sources, and a
    %   PSF to fit.
    %   The fit is for only, flux and position.
    %   The function fits all the stamps simultanously.
    %   The flux fit is fitted linearly, while the positions are fitted
    %   using a one-directional steepest descent style method.
    %   In each iteration the PSF is shifted using fft-sub-pixels-shift.
    % Input  : - A background subtracted cube of stamps around sources.
    %            The third dimesnion is the stamp index.
    %            The code is debugged only for an odd-size PSF and stamps.
    %          * ...,key,val,...
    %            'PSF' - A PSF stamp to fit. If this is a scalar, then will
    %                   use a Gaussian PSF, which sigma-width is given by
    %                   the scalar. Default is 1.5.
    %            'Back' - Either a vector (element per stamp), or a cube
    %                   (the same size as the input cube) of background in the
    %                   cube. If empty (or Std is empty, then will
    %                   recaculate the background and std using
    %                   imUtil.sources.backgroundCube
    %                   This background will be subtracted prior to
    %                   fit, and will be returned to the CubePsfSub output.
    %                   If 0, then do not subtract background.
    %                   Default is []
    %            'Std' - Like 'Back', but for the std. Default is [].
    %            'FitRadius' - Radius around source center to fit.
    %                   This can be used in order to exclude regions
    %                   outside the stellar core.
    %                   Default is 3.
    %            'backgroundCubeArgs' - A cell array of additional
    %                   arguments to pass to imUtil.sources.backgroundCube
    %                   Default is {}.
    %
    %            'Xinit' - A vector of initial X position for the PSF
    %                   position in the stamps. If empty, then 
    %                   use size/2 + 0.5. Default is [].
    %            'Yinit' - Like 'Xinit' but for the Y position.
    %                   Default is [].
    %
    %            Fitting-related parameters:
    %            'SmallStep' - Gradient step size. Default is 1e-4 (pix).
    %            'MaxStep' - Maximum step size in each iteration.
    %                   Default is 0.2.
    %            'ConvThresh' - Convergence threshold. Default is 1e-4.
    %            'MaxIter' - Max number of iterations. Default is 10.
    %            'SN' - Vector S/N to use for the conversion. (not useful)
    %                   Default is [].
    %            'UseSourceNoise' - A string indicating if implement
    %                   source noise in the fit. The function use the 
    %                   last estimator of the psf flux by the current best 
    %                   fit fromthe previous step. 
    %                   'all' - use from the second iteration and on.
    %                   'last' - use only in the last (additional) iteration. 
    %                   'off' - only background noise. 
    %                   Default is 'last'.
    %            'ZP' - ZP for magnitude calculations. Default is 25.
    % Output : - A structure with the following fields:
    %            .Chi2 - Vector of \chi^2 (element per stamp).
    %            .Dof - The number of degrees of freedom in the fit.
    %                   This is the stamp area minus 3.
    %            .Flux - Vector of fitted fluxes.
    %            .SNm - S/N for measurment, assuming gain=1 (Poisson
    %                   errors).
    %                   Note that if the source is negativem then S/N is
    %                   also negative.
    %            .DX - Vector of fitted X positions relative the Xcenter.
    %            .DY - Vector of fitted Y positions relative the Xcenter.
    %            .Xinit - Xinit
    %            .Yinit - Yinit
    %            .Xcenter - Stamp X center.
    %            .Ycenter - Stamp Y center.
    %            .ConvergeFlag - A vector of logicals (one per stamp)
    %                   indicating if the PSF fitting for the stamp
    %                   converged.
    %            .Niter - Number of iterations used.
    %            .Mag   - Magnitude (luptitude).
    %          - The input cube, after subtracting the fitted PSF from each
    %            stamp. If the background is provided, then it is returned
    %            to the stamps.
    % Author : Eran Ofek (Dec 2021)
    % Example: P=imUtil.kernel2.gauss;
    %          Ps=imUtil.trans.shift_fft(P,0.4,0.7);
    %          [R,S]=imUtil.sources.psfPhotCube(Ps, 'PSF',P)
    %          P=imUtil.kernel2.gauss(1.5.*ones(4,1));
    %          Ps=imUtil.trans.shift_fft(P,[0.4;0.7;-1.1;3.6],[0.7;-0.2;-0.9;-2.6]);
    %          Ps = Ps.*permute([100 110 200 300],[1 3 2]) + randn(15,15);
    %          Result = imUtil.sources.psfPhotCube(Ps, 'PSF',P(:,:,1))
   
    arguments
        Cube
        Args.PSF        = 1.5;  % scalar will generate a Gaussian PSF - normalized to 1
        Args.Std        = [];   % vector or cube
        Args.Back       = [];
        Args.FitRadius  = 3;
        Args.backgroundCubeArgs cell = {};
        
        Args.Xinit      = [];
        Args.Yinit      = [];
        
        Args.SmallStep  = 0.1; %3e-3; %3e-3; %1e-3;
        Args.MaxStep    = 0.15;
        Args.ConvThresh = 1e-3;
        Args.MaxIter    = 8;

        Args.SN         = [];
        Args.UseSNR     = false;
        
        Args.UseSourceNoise = 'last'; %'off';
        Args.ZP         = 25; 
        
        Args.Verbous logical = false;
    end
    
    % warning('BUG: convergence is not very good - need a better algorithm')
    
    % background treatment
    % 1. do nothing
    % 2. subtract and return
    % 3. measure in first image [NaN] - same for std!
    % 4. fit?
    
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
    
    FitRadius2 = Args.FitRadius.^2;
    
    [Ny, Nx, Nim] = size(Cube);
    Xcenter = Nx.*0.5 + 0.5;
    Ycenter = Ny.*0.5 + 0.5;
%     Dof     = Nx.*Ny - 3;
    
    VecXrel = (1:1:Nx) - Xcenter;
    VecYrel = (1:1:Ny) - Ycenter;
    
    if isempty(Args.Xinit)
        Args.Xinit = Xcenter;
    end
    if isempty(Args.Yinit)
        Args.Yinit = Ycenter;
    end
    
    if numel(Args.PSF)==1
        Args.PSF = imUtil.kernel2.gauss(Args.PSF);
    end    
    
    % the small step and conversion threshold may depend on the SNR: 
    if isempty(Args.SN) || ~Args.UseSNR  
        SmallStep  = Args.SmallStep;
        ConvThresh = Args.ConvThresh;
    else
        ConvThresh = max(0.5./Args.SN, Args.ConvThresh); 
%         SmallStep  = min(Args.SmallStep.*0.2.*Args.SN, 0.5*Args.MaxStep);
%         Args.FloorStep = 1e-3;
        SmallStep  = max(Args.SmallStep./Args.SN, 1e-3); 
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

    if isempty(Args.SN) || ~Args.UseSNR 
        VecD = [0, SmallStep, 2.*SmallStep];
        H    = VecD'.^[0, 1, 2];
    else
        H    = zeros(3,3,Nim);
        VecD = [0*[1:Nim]', SmallStep, 2.*SmallStep];
        for i = 1:Nim
            A = squeeze(VecD(i,:)');
            H(:,:,i) = A.^[0, 1, 2];
        end
    end
    Ind   = 0;
    NotConverged = true;
    StdBack = Std;
    while Ind<Args.MaxIter && NotConverged
        Ind = Ind + 1;
        if UseSourceNoise && Ind>2
            [~, Flux, ShiftedPSF]  = internalCalcChi2(Cube, Std, Args.PSF, DX, DY, WeightedPSF, VecXrel, VecYrel, FitRadius2);
            Std = sqrt(Flux.*ShiftedPSF+StdBack.^2);
        end
        % AppFlux is approximate flux
        [StepX,StepY,AppFlux]  = gradDescentPSF(Cube, Std, Args.PSF, DX, DY, WeightedPSF, VecXrel, VecYrel, FitRadius2,H,SmallStep,Args.MaxStep);
        %{
        % calc \chi2 and gradient
        %Chi2     = internalCalcChi2(Cube, Std, Args.PSF, DX,                   DY, WeightedPSF, VecXrel, VecYrel, FitRadius2);
        %Chi2_Dx  = internalCalcChi2(Cube, Std, Args.PSF, DX+SmallStep,    DY, WeightedPSF, VecXrel, VecYrel, FitRadius2);
        %Chi2_Dx2 = internalCalcChi2(Cube, Std, Args.PSF, DX+SmallStep.*2, DY, WeightedPSF, VecXrel, VecYrel, FitRadius2);
                
        %ParX     = polyfit(VecD, [Chi2, Chi2_Dx, Chi2_Dx2], 2);
        %ParX     = H\[Chi2.'; Chi2_Dx.'; Chi2_Dx2.'];
        
        %Chi2     = internalCalcChi2(Cube, Std, Args.PSF, DX, DY,                  WeightedPSF);
        %Chi2_Dy  = internalCalcChi2(Cube, Std, Args.PSF, DX, DY+SmallStep,   WeightedPSF, VecXrel, VecYrel, FitRadius2);
        %Chi2_Dy2 = internalCalcChi2(Cube, Std, Args.PSF, DX, DY+SmallStep.*2,WeightedPSF, VecXrel, VecYrel, FitRadius2);
        
        %ParY     = polyfit(VecD, [Chi2, Chi2_Dy, Chi2_Dy2], 2);
        %ParY     = H\[Chi2.'; Chi2_Dy.'; Chi2_Dy2.'];
        
        %StepX    = -ParX(2,:)./(2.*ParX(3,:));
        %StepY    = -ParY(2,:)./(2.*ParY(3,:));
        
        %NotMinimaX = ParX(3,:)<0;
        %NotMinimaY = ParY(3,:)<0;
        
        % reverse sign for maxima...
        %StepX(NotMinimaX) = -StepX(NotMinimaX);
        %StepY(NotMinimaY) = -StepY(NotMinimaY);
        
        %StepX    = sign(StepX).*min(abs(StepX), Args.MaxStep);
        %StepY    = sign(StepY).*min(abs(StepY), Args.MaxStep);
        %}
        DX       = DX + StepX;
        DY       = DY + StepY;
        
        % if nargout>2
        %     OutDebug(Ind).X = DX;
        %     OutDebug(Ind).Y = DY;
        %     OutDebug(Ind).F = AppFlux;
        % end
        
%         fprintf('StepX: %s, StepY %s \n',StepX,StepY);
        
        % stoping criteria
        ConvergeFlag = abs(StepX')<ConvThresh & abs(StepY')<ConvThresh;
        if all(ConvergeFlag)
            NotConverged = false;
        end
        
        if Args.Verbous && Ind > 1
            fprintf('Iterations: %d of %d, Converged %d of %d\n',Ind, Args.MaxIter,sum(ConvergeFlag),length(ConvergeFlag));
        end
        
    end
    % final fit and return flux
    if AdditionalIter
        [~, Flux, ShiftedPSF]  = internalCalcChi2(Cube, Std, Args.PSF, DX, DY, WeightedPSF, VecXrel, VecYrel, FitRadius2);
        Std = sqrt(Flux.*ShiftedPSF+StdBack.^2);
        [StepX,StepY]  = gradDescentPSF(Cube, Std, Args.PSF, DX, DY, WeightedPSF, VecXrel, VecYrel, FitRadius2,H,SmallStep,Args.MaxStep);
        DX       = DX + StepX;
        DY       = DY + StepY;
    end
    [Result.Chi2, Flux, ShiftedPSF, Dof]  = internalCalcChi2(Cube, Std, Args.PSF, DX, DY, WeightedPSF, VecXrel, VecYrel, FitRadius2);
    if isempty(Dof)
        Result.Dof  = Nx.*Ny - 3;
    else
        Result.Dof  = Dof;
    end
    
%     if Args.Verbous
%         fprintf('Iterations: %d of %d, Converged %d of %d\n',Ind, Args.MaxIter,sum(ConvergeFlag),length(ConvergeFlag));
%     end
    
    Result.Flux = squeeze(Flux);
    % SNm can be negaive if source is negative
    Result.SNm  = sign(Result.Flux).*abs(Result.Flux)./sqrt(abs(Result.Flux) + (squeeze(StdBack)).^2);  % S/N for measurments
    Result.Mag  = convert.luptitude(Result.Flux, 10.^(0.4.*Args.ZP));
    Result.DX = DX(:);
    Result.DY = DY(:);
    Result.Xinit = Args.Xinit;
    Result.Yinit = Args.Yinit;
    Result.Xcenter = Xcenter;
    Result.Ycenter = Ycenter;
    Result.ConvergeFlag = ConvergeFlag;
    Result.Niter   = Ind;
    
    if nargout>1
        % subtract best fit PSFs from cube
        CubePsfSub = Cube - ShiftedPSF.*Flux;
        if ~isempty(Args.Back)
            % return the background
            CubePsfSub = CubePsfSub + Back;
        end
    end
end

% Internal functions


function [StepX,StepY,AppFlux]  = gradDescentPSF(Cube, Std, PSF, DX, DY, WeightedPSF, VecXrel, VecYrel, FitRadius2,H,SmallStep,MaxStep)
% Return the next gradient Descent step for the PSF's position fitting.

        [Chi2,AppFlux]     = internalCalcChi2(Cube, Std, PSF, DX,                   DY, WeightedPSF, VecXrel, VecYrel, FitRadius2);
        Chi2_Dx  = internalCalcChi2(Cube, Std, PSF, DX+SmallStep',    DY, WeightedPSF, VecXrel, VecYrel, FitRadius2);
        Chi2_Dx2 = internalCalcChi2(Cube, Std, PSF, DX+SmallStep'.*2, DY, WeightedPSF, VecXrel, VecYrel, FitRadius2);
                
        %ParX     = polyfit(VecD, [Chi2, Chi2_Dx, Chi2_Dx2], 2);
        Nim = size(H,3);
        if Nim == 1
            ParX     = H\[Chi2.'; Chi2_Dx.'; Chi2_Dx2.'];
        else
            ParX = zeros(3,Nim);            
            for i = 1:Nim
                ParX(:,i) = H(:,:,i)\[Chi2(i); Chi2_Dx(i); Chi2_Dx2(i)];
            end
        end
        
        %Chi2     = internalCalcChi2(Cube, Std, Args.PSF, DX, DY,                  WeightedPSF);
        Chi2_Dy  = internalCalcChi2(Cube, Std, PSF, DX, DY+SmallStep',   WeightedPSF, VecXrel, VecYrel, FitRadius2);
        Chi2_Dy2 = internalCalcChi2(Cube, Std, PSF, DX, DY+SmallStep'.*2,WeightedPSF, VecXrel, VecYrel, FitRadius2);
        
        %ParY     = polyfit(VecD, [Chi2, Chi2_Dy, Chi2_Dy2], 2);        
        if Nim == 1
            ParY     = H\[Chi2.'; Chi2_Dy.'; Chi2_Dy2.'];
        else
            ParY = zeros(3,Nim);            
            for i = 1:Nim
                ParY(:,i) = H(:,:,i)\[Chi2(i); Chi2_Dy(i); Chi2_Dy2(i)];
            end
        end
        
        StepX    = -ParX(2,:)./(2.*ParX(3,:));
        StepY    = -ParY(2,:)./(2.*ParY(3,:));
        
        NotMinimaX = ParX(3,:)<0;
        NotMinimaY = ParY(3,:)<0;
        
        % reverse sign for maxima...
        StepX(NotMinimaX) = -StepX(NotMinimaX);
        StepY(NotMinimaY) = -StepY(NotMinimaY);
        
        StepX    = sign(StepX).*min(abs(StepX), MaxStep);
        StepY    = sign(StepY).*min(abs(StepY), MaxStep);





end

function [Chi2,WeightedFlux, ShiftedPSF, Dof] = internalCalcChi2(Cube, Std, PSF, DX, DY, WeightedPSF, VecXrel, VecYrel, FitRadius2)
    % Return Chi2 for specific PSF and Cube
    % shift PSF
    
    
    
    FluxMethod = 'wsumall'; %'medall';
    
    % Shifting PSF is safer, because of the fft on a smooth function is
    % more reliable.
    ShiftedPSF = imUtil.trans.shift_fft(PSF, DX, DY);
    
    switch FluxMethod
        case 'wsumall'
            WeightedFlux = sum(Cube.*ShiftedPSF, [1 2], 'omitnan')./WeightedPSF;
        case 'meanall'
            WeightedFlux = mean(Cube./ShiftedPSF, [1 2], 'omitnan'); %./WeightedPSF;
        case 'medall'
            WeightedFlux = median(Cube./ShiftedPSF, [1 2], 'omitnan'); %./WeightedPSF;    
        case 'med'
            
        otherwise
            error('Unknown FluxMethod option');
    end
    Resid = Cube - WeightedFlux.*ShiftedPSF;
    
    % FFU: search / remove outliers

    if isempty(FitRadius2)
        % use the entire stamp
        ResidStd = Resid./Std;
        Dof      = [];
    else
        MatX     = permute(VecXrel - DX(:),[3 2 1]);
        MatY     = permute(VecYrel - DY(:),[2 3 1]);
        MatR2    = MatX.^2 + MatY.^2;
        Flag     = MatR2<FitRadius2;
        ResidStd = Flag.*Resid./Std;
        Dof      = squeeze(sum(Flag,[1 2]) - 3);
    end
    
    Chi2  = sum( ResidStd.^2, [1 2], 'omitnan');
    %Chi2  = sum( (Resid./Std).^2, [1 2], 'omitnan');
    Chi2  = squeeze(Chi2);
     
end
