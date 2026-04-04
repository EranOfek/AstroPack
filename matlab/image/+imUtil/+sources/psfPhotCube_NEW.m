function [Result, CubePsfSub] = psfPhotCube(Cube, Args)
    % The core function for PSF-fitting photometry.
    %
    % FFU: use: imUtil.sources.mex.annulus_median
    % 
    %   The input of this function is a cube of stamps of sources, and a
    %   PSF to fit.
    %   The fit is for only, flux and position.
    %   The function fits all the stamps simultanously.
    %   The flux fit is fitted linearly, while the positions are fitted
    %   using one of the following methods:
    %     '1D'   - separate 1D quadratic steps in X and Y.
    %     '2D'   - local 2D quadratic approximation to chi2.
    %     '2DGN' - 2D Gauss-Newton step using PSF derivatives.
    %   In each iteration the PSF is shifted using fft-sub-pixels-shift or
    %   lanczos3 shift.
    %
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
    %            'AnnulusRad' - If ShiftMethod=1, then this is the
    %                   background annulus width. Default is 3.
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
    %            'Method' - Minimization method:
    %                   '1D'   - original-style 1D per-axis quadratic step.
    %                   '2D'   - local 2D quadratic fit to chi2.
    %                   '2DGN' - 2D Gauss-Newton step.
    %                   Default is '1D'.
    %            'SmallStep' - Gradient step size. Default is 0.10 (pix).
    %            'MaxStep' - Maximum step size in each iteration.
    %                   Default is 0.2.
    %            'FloorStep' - Minimal S/N-scaled SmallStep.
    %                   Default is 3e-3.
    %            'ConvThresh' - Convergence threshold. Default is 1e-3.
    %            'MaxIter' - Max number of iterations. Default is 8.
    %            'SN' - Vector S/N to use for the conversion.
    %                   Default is [].
    %            'UseSNR' - Scale SmallStep and ConvThresh using SN.
    %                   Default is true.
    %            'UseSourceNoise' - A string indicating if implement
    %                   source noise in the fit. The function use the
    %                   last estimator of the psf flux by the current best
    %                   fit from the previous step.
    %                   'all' - use from the second iteration and on.
    %                   'last' - use only in the last (additional) iteration.
    %                   'off' - only background noise.
    %                   Default is 'last'.
    %            'ZP' - ZP for magnitude calculations. Default is 25.
    % Output : - A structure with the following fields:
    %            .Chi2 - Vector of \chi^2 (element per stamp).
    %            .Dof - Vector of degrees of freedom per stamp.
    %            .Flux - Vector of fitted fluxes.
    %            .FluxErr - Vector of fitted flux uncertainties.
    %            .SNm - S/N for measurement.
    %            .DX - Vector of fitted X positions relative the Xcenter.
    %            .DY - Vector of fitted Y positions relative the Ycenter.
    %            .Xinit - Xinit
    %            .Yinit - Yinit
    %            .Xcenter - Stamp X center.
    %            .Ycenter - Stamp Y center.
    %            .ConvergeFlag - A matrix of logicals (iter x stamp)
    %                   indicating if the PSF fitting for the stamp
    %                   converged in each iteration.
    %            .Niter - Number of iterations used.
    %            .Mag   - Magnitude (luptitude).
    %            .ShiftedPSF - Shifted PSF stamps.
    %            .Back - Background used in the fit.
    %            .StdBack - Background-noise estimate used in the fit.
    %          - The input cube, after subtracting the fitted PSF from each
    %            stamp. If background was subtracted, then it is returned
    %            to the stamps.
    % Author : Eran Ofek (Dec 2021); revised

    arguments
        Cube
        Args.PSF        = 1.5
        Args.Std        = []
        Args.Back       = []
        Args.FitRadius  = 3
        Args.AnnulusRad = 3
        Args.backgroundCubeArgs cell = {}
    
        Args.Xinit      = []
        Args.Yinit      = []
    
        Args.Method     = '1D'; %char {mustBeMember(Args.Method, {'1D','2D','2DGN'})} = '1D'
        Args.SmallStep  = 0.10
        Args.MaxStep    = 0.20
        Args.FloorStep  = 3e-3
        Args.ConvThresh = 1e-3
        Args.MaxIter    = 8
    
        Args.SN         = []
        Args.UseSNR     = true
    
        Args.UseSourceNoise  = 'last'
        Args.ZP         = 25
    
        Args.Verbous logical = false
        Args.ShiftMethod = 1; % 1 lanczos3, 2-fft
        Args.UseMex      = true;
    end

    Args.UseSourceNoise = lower(Args.UseSourceNoise);
    if ~ismember(Args.UseSourceNoise, {'all','last','off'})
        error('UseSourceNoise must be ''all'', ''last'', or ''off''');
    end

    [Ny, Nx, Nim] = size(Cube);

    if numel(Args.PSF) == 1
        Args.PSF = imUtil.kernel2.gauss(Args.PSF);
    end

    Xcenter = Nx .* 0.5 + 0.5;
    Ycenter = Ny .* 0.5 + 0.5;

    VecXrel = (1:Nx) - Xcenter;
    VecYrel = (1:Ny) - Ycenter;

    if isempty(Args.Xinit)
        Args.Xinit = Xcenter;
    end
    if isempty(Args.Yinit)
        Args.Yinit = Ycenter;
    end

    Args.Xinit = localExpandInit(Args.Xinit, Nim);
    Args.Yinit = localExpandInit(Args.Yinit, Nim);

    if isempty(Args.FitRadius)
        FitRadius2 = [];
    else
        FitRadius2 = Args.FitRadius.^2;
    end

    [CubeFit, Back, StdBack, BackgroundWasSubtracted] = localPrepareBackground(Cube, Args);
    Std = StdBack;

    [SmallStep, ConvThresh] = localPrepareStepControl(Args, Nim);

    DX = Args.Xinit(:).' - Xcenter;
    DY = Args.Yinit(:).' - Ycenter;

    ConvergeFlag = false(Args.MaxIter, Nim);
    AppFlux      = nan(Args.MaxIter, Nim);

    AdditionalIter = false;
    UseSourceNoise = false;
    switch lower(Args.UseSourceNoise)
        case 'all'
            UseSourceNoise = true;
        case 'off'
            UseSourceNoise = false;
        case 'last'
            AdditionalIter = true;
            UseSourceNoise = false;
    end

    Ind = 0;
    Active = true(1, Nim);

    while Ind < Args.MaxIter && any(Active)
        Ind = Ind + 1;

        if UseSourceNoise && Ind > 1
            [~, FluxTmp, ShiftedPSFTmp] = internalCalcChi2( ...
                CubeFit, Std, Args.PSF, DX, DY, VecXrel, VecYrel, FitRadius2, Args.ShiftMethod, Args.UseMex);
            Std = localUpdateStdWithSourceNoise(FluxTmp, ShiftedPSFTmp, StdBack);
        end

        switch upper(Args.Method)
            case '1D'
                [StepX, StepY, AppFlux(Ind,:)] = gradDescentPSF1D( ...
                    CubeFit, Std, Args.PSF, DX, DY, VecXrel, VecYrel, ...
                    FitRadius2, SmallStep, Args.MaxStep, Args.ShiftMethod, Args.UseMex);

            case '2D'
                [StepX, StepY, AppFlux(Ind,:)] = gradDescentPSF2D( ...
                    CubeFit, Std, Args.PSF, DX, DY, VecXrel, VecYrel, ...
                    FitRadius2, SmallStep, Args.MaxStep, Args.ShiftMethod, Args.UseMex);

            case '2DGN'
                [StepX, StepY, AppFlux(Ind,:)] = gradDescentPSF2DGN( ...
                    CubeFit, Std, Args.PSF, DX, DY, VecXrel, VecYrel, ...
                    FitRadius2, SmallStep, Args.MaxStep, Args.ShiftMethod, Args.UseMex);
        end

        StepX(~Active) = 0;
        StepY(~Active) = 0;

        DX = DX + StepX;
        DY = DY + StepY;

        ConvergeFlag(Ind,:) = abs(StepX) < ConvThresh(:).' & abs(StepY) < ConvThresh(:).';
        Active = ~ConvergeFlag(Ind,:);

        if Args.Verbous
            fprintf('Iter: %2.0d of %d, converged: %d of %d\n', ...
                Ind, Args.MaxIter, sum(ConvergeFlag(Ind,:)), Nim);
        end
    end

    if AdditionalIter
        [~, FluxTmp, ShiftedPSFTmp] = internalCalcChi2( ...
            CubeFit, Std, Args.PSF, DX, DY, VecXrel, VecYrel, FitRadius2, Args.ShiftMethod, Args.UseMex);

        Std = localUpdateStdWithSourceNoise(FluxTmp, ShiftedPSFTmp, StdBack);

        switch upper(Args.Method)
            case '1D'
                [StepX, StepY] = gradDescentPSF1D( ...
                    CubeFit, Std, Args.PSF, DX, DY, VecXrel, VecYrel, ...
                    FitRadius2, SmallStep, Args.MaxStep, Args.ShiftMethod, Args.UseMex);
            case '2D'
                [StepX, StepY] = gradDescentPSF2D( ...
                    CubeFit, Std, Args.PSF, DX, DY, VecXrel, VecYrel, ...
                    FitRadius2, SmallStep, Args.MaxStep, Args.ShiftMethod, Args.UseMex);
            case '2DGN'
                [StepX, StepY] = gradDescentPSF2DGN( ...
                    CubeFit, Std, Args.PSF, DX, DY, VecXrel, VecYrel, ...
                    FitRadius2, SmallStep, Args.MaxStep, Args.ShiftMethod, Args.UseMex);
        end

        DX = DX + StepX;
        DY = DY + StepY;
    end

    [Chi2, Flux, ShiftedPSF, Dof, FluxErr] = internalCalcChi2( ...
        CubeFit, Std, Args.PSF, DX, DY, VecXrel, VecYrel, FitRadius2, Args.ShiftMethod, Args.UseMex);

    Result = struct();
    Result.Chi2         = Chi2(:);
    Result.Dof          = Dof(:);
    Result.Flux         = Flux(:);
    Result.FluxErr      = FluxErr(:);
    Result.SNm          = Flux(:) ./ FluxErr(:);
    Result.Mag          = convert.luptitude(Result.Flux, 10.^(0.4 .* Args.ZP));
    Result.DX           = DX(:);
    Result.DY           = DY(:);
    Result.Xinit        = Args.Xinit(:);
    Result.Yinit        = Args.Yinit(:);
    Result.Xcenter      = Xcenter;
    Result.Ycenter      = Ycenter;
    Result.ConvergeFlag = ConvergeFlag;
    Result.Niter        = Ind;
    Result.ShiftedPSF   = ShiftedPSF;
    Result.Back         = Back;
    Result.StdBack      = StdBack;

    if nargout > 1
        CubePsfSub = CubeFit - ShiftedPSF .* reshape(Flux, 1, 1, []);
        if BackgroundWasSubtracted
            CubePsfSub = CubePsfSub + Back;
        end
    end
end


%% =========================================================================
% Internal functions
% =========================================================================

function Init = localExpandInit(Init, Nim)
    if isscalar(Init)
        Init = repmat(Init, Nim, 1);
    else
        Init = Init(:);
        if numel(Init) ~= Nim
            error('Xinit/Yinit must be scalar or have one element per stamp');
        end
    end
end


function [CubeFit, Back, StdBack, BackgroundWasSubtracted] = localPrepareBackground(Cube, Args)
    if isequal(Args.Back, 0)
        Back = zeros(size(Cube), 'like', Cube);

        if isempty(Args.Std)
            [~, StdBack] = imUtil.sources.backgroundCube(Cube, 'AnnulusRad',Args.AnnulusRad, Args.backgroundCubeArgs{:}, 'Squeeze', false);
        else
            StdBack = localExpandToCubeOrStamp(Args.Std, size(Cube), 'Std', Cube);
        end

        CubeFit = Cube;
        BackgroundWasSubtracted = false;
        return;
    end

    if isempty(Args.Back) && isempty(Args.Std)
        [Back, StdBack] = imUtil.sources.backgroundCube(Cube, 'AnnulusRad',Args.AnnulusRad, Args.backgroundCubeArgs{:}, 'Squeeze', false);
        CubeFit = Cube - Back;
        BackgroundWasSubtracted = true;
        return;
    end

    if ~isempty(Args.Back) && isempty(Args.Std)
        Back = localExpandToCubeOrStamp(Args.Back, size(Cube), 'Back', Cube);
        [~, StdBack] = imUtil.sources.backgroundCube(Cube, 'AnnulusRad',Args.AnnulusRad, Args.backgroundCubeArgs{:}, 'Squeeze', false);
        CubeFit = Cube - Back;
        BackgroundWasSubtracted = true;
        return;
    end

    if isempty(Args.Back) && ~isempty(Args.Std)
        [Back, ~] = imUtil.sources.backgroundCube(Cube, 'AnnulusRad',Args.AnnulusRad, Args.backgroundCubeArgs{:}, 'Squeeze', false);
        StdBack = localExpandToCubeOrStamp(Args.Std, size(Cube), 'Std', Cube);
        CubeFit = Cube - Back;
        BackgroundWasSubtracted = true;
        return;
    end

    Back    = localExpandToCubeOrStamp(Args.Back, size(Cube), 'Back', Cube);
    StdBack = localExpandToCubeOrStamp(Args.Std,  size(Cube), 'Std',  Cube);

    CubeFit = Cube - Back;
    BackgroundWasSubtracted = true;
end


function Value = localExpandToCubeOrStamp(ValueIn, CubeSize, NameStr, LikeValue)
    Ny  = CubeSize(1);
    Nx  = CubeSize(2);
    Nim = CubeSize(3);

    if isscalar(ValueIn)
        Value = repmat(cast(ValueIn, 'like', LikeValue), Ny, Nx, Nim);
        return;
    end

    if isvector(ValueIn)
        if numel(ValueIn) ~= Nim
            error('%s vector must contain one element per stamp', NameStr);
        end
        Value = cast(reshape(ValueIn(:), 1, 1, Nim), 'like', LikeValue);
        return;
    end

    if isequal(size(ValueIn), CubeSize)
        Value = cast(ValueIn, 'like', LikeValue);
        return;
    end

    error('%s must be scalar, vector of length Nim, or a cube of size(Cube)', NameStr);
end


function [SmallStep, ConvThresh] = localPrepareStepControl(Args, Nim)
    if isempty(Args.SN) || ~Args.UseSNR
        SmallStep  = repmat(Args.SmallStep, Nim, 1);
        ConvThresh = repmat(Args.ConvThresh, Nim, 1);
    else
        SN = Args.SN(:);
        if numel(SN) ~= Nim
            error('SN must contain one element per stamp');
        end
        SN_Pos = max(0.5,SN); % to avoid negative S/N
        ConvThresh = max(0.5 ./ SN_Pos, Args.ConvThresh);
        SmallStep  = max(Args.SmallStep ./ SN_Pos, Args.FloorStep);
    end
end


function Std = localUpdateStdWithSourceNoise(Flux, ShiftedPSF, StdBack)
    Flux3 = reshape(Flux, 1, 1, []);
    ModelVar = Flux3 .* ShiftedPSF;
    ModelVar = max(ModelVar, 0);
    Var = max(ModelVar + StdBack.^2, eps(class(StdBack)));
    Std = sqrt(Var);
end


function [StepX, StepY, AppFlux] = gradDescentPSF1D( ...
    Cube, Std, PSF, DX, DY, VecXrel, VecYrel, FitRadius2, SmallStep, MaxStep, ShiftMethod, UseMex)

    Nim = numel(DX);
    StepRef = SmallStep(1);
    VecD = [0; StepRef; 2 .* StepRef];
    H = VecD .^ [0 1 2];

    if all(abs(SmallStep - StepRef) < eps(max(abs(SmallStep))))
        Scale = 1;
    else
        Ratio = SmallStep ./ StepRef;
        Scale = [ones(1, Nim); Ratio.'; (Ratio.^2).'];
    end

    [Chi2_0, AppFlux] = internalCalcChi2( ...
        Cube, Std, PSF, DX, DY, VecXrel, VecYrel, FitRadius2, ShiftMethod, UseMex);

    Chi2_X1 = internalCalcChi2( ...
        Cube, Std, PSF, DX + SmallStep.', DY, VecXrel, VecYrel, FitRadius2, ShiftMethod, UseMex);

    Chi2_X2 = internalCalcChi2( ...
        Cube, Std, PSF, DX + 2 .* SmallStep.', DY, VecXrel, VecYrel, FitRadius2, ShiftMethod, UseMex);

    UseBacklash = false;
    if UseBacklash
        ParX = H \ [Chi2_0.'; Chi2_X1.'; Chi2_X2.']; % slower
    else
        d0 = VecD(1);
        d1 = VecD(2);
        d2 = VecD(3);
        
        f0 = Chi2_0(:).';
        f1 = Chi2_X1(:).';
        f2 = Chi2_X2(:).';
        
        Den0 = (d0 - d1) .* (d0 - d2);
        Den1 = (d1 - d0) .* (d1 - d2);
        Den2 = (d2 - d0) .* (d2 - d1);
        
        ParX = zeros(3, numel(f0), 'like', f0);
        
        ParX(1,:) = f0 .* ( d1 .* d2 ./ Den0 ) + ...
                    f1 .* ( d0 .* d2 ./ Den1 ) + ...
                    f2 .* ( d0 .* d1 ./ Den2 );
        
        ParX(2,:) = f0 .* (-(d1 + d2) ./ Den0 ) + ...
                    f1 .* (-(d0 + d2) ./ Den1 ) + ...
                    f2 .* (-(d0 + d1) ./ Den2 );
        
        ParX(3,:) = f0 .* ( 1 ./ Den0 ) + ...
                    f1 .* ( 1 ./ Den1 ) + ...
                    f2 .* ( 1 ./ Den2 );
    end


    if ~isscalar(Scale)
        ParX = ParX ./ Scale;
    end

    Chi2_Y1 = internalCalcChi2( ...
        Cube, Std, PSF, DX, DY + SmallStep.', VecXrel, VecYrel, FitRadius2, ShiftMethod, UseMex);

    Chi2_Y2 = internalCalcChi2( ...
        Cube, Std, PSF, DX, DY + 2 .* SmallStep.', VecXrel, VecYrel, FitRadius2, ShiftMethod, UseMex);

    if UseBacklash
        ParY = H \ [Chi2_0.'; Chi2_Y1.'; Chi2_Y2.']; % slower
    else
        f1 = Chi2_Y1(:).';
        f2 = Chi2_Y2(:).';
        
        Den0 = (d0 - d1) .* (d0 - d2);
        Den1 = (d1 - d0) .* (d1 - d2);
        Den2 = (d2 - d0) .* (d2 - d1);
        
        ParY = zeros(3, numel(f0), 'like', f0);
        
        ParY(1,:) = f0 .* ( d1 .* d2 ./ Den0 ) + ...
                    f1 .* ( d0 .* d2 ./ Den1 ) + ...
                    f2 .* ( d0 .* d1 ./ Den2 );
        
        ParY(2,:) = f0 .* (-(d1 + d2) ./ Den0 ) + ...
                    f1 .* (-(d0 + d2) ./ Den1 ) + ...
                    f2 .* (-(d0 + d1) ./ Den2 );
        
        ParY(3,:) = f0 .* ( 1 ./ Den0 ) + ...
                    f1 .* ( 1 ./ Den1 ) + ...
                    f2 .* ( 1 ./ Den2 );
    end

    if ~isscalar(Scale)
        ParY = ParY ./ Scale;
    end

    StepX = localParabolaStep(ParX, MaxStep);
    StepY = localParabolaStep(ParY, MaxStep);
end


function [StepX, StepY, AppFlux] = gradDescentPSF2D( ...
    Cube, Std, PSF, DX, DY, VecXrel, VecYrel, FitRadius2, SmallStep, MaxStep, ShiftMethod, UseMex)

    Nim = numel(DX);

    [F0, AppFlux] = internalCalcChi2( ...
        Cube, Std, PSF, DX, DY, VecXrel, VecYrel, FitRadius2, ShiftMethod, UseMex);

    sx = SmallStep(:).';
    sy = SmallStep(:).';

    Fxp = internalCalcChi2(Cube, Std, PSF, DX + sx, DY,      VecXrel, VecYrel, FitRadius2, ShiftMethod, UseMex);
    Fxm = internalCalcChi2(Cube, Std, PSF, DX - sx, DY,      VecXrel, VecYrel, FitRadius2, ShiftMethod, UseMex);
    Fyp = internalCalcChi2(Cube, Std, PSF, DX,      DY + sy, VecXrel, VecYrel, FitRadius2, ShiftMethod, UseMex);
    Fym = internalCalcChi2(Cube, Std, PSF, DX,      DY - sy, VecXrel, VecYrel, FitRadius2, ShiftMethod, UseMex);

    Fpp = internalCalcChi2(Cube, Std, PSF, DX + sx, DY + sy, VecXrel, VecYrel, FitRadius2, ShiftMethod, UseMex);
    Fpm = internalCalcChi2(Cube, Std, PSF, DX + sx, DY - sy, VecXrel, VecYrel, FitRadius2, ShiftMethod, UseMex);
    Fmp = internalCalcChi2(Cube, Std, PSF, DX - sx, DY + sy, VecXrel, VecYrel, FitRadius2, ShiftMethod, UseMex);
    Fmm = internalCalcChi2(Cube, Std, PSF, DX - sx, DY - sy, VecXrel, VecYrel, FitRadius2, ShiftMethod, UseMex);

    if UseMex
        [StepX, StepY] = imUtil.sources.mex.psfPhotCube_step2d_mex( ...
                    F0, Fxp, Fxm, Fyp, Fym, Fpp, Fpm, Fmp, Fmm, sx, sy, MaxStep);
    else

        Gx  = (Fxp - Fxm) ./ (2 .* sx);
        Gy  = (Fyp - Fym) ./ (2 .* sy);
    
        Hxx = (Fxp - 2 .* F0 + Fxm) ./ (sx.^2);
        Hyy = (Fyp - 2 .* F0 + Fym) ./ (sy.^2);
        Hxy = (Fpp - Fpm - Fmp + Fmm) ./ (4 .* sx .* sy);
    
        StepX = zeros(1, Nim);
        StepY = zeros(1, Nim);
    
        for Iim = 1:Nim
            Hmat = [Hxx(Iim), Hxy(Iim); Hxy(Iim), Hyy(Iim)];
            Gvec = [Gx(Iim); Gy(Iim)];
    
            if all(isfinite(Hmat), 'all') && all(isfinite(Gvec))
                if det(Hmat) > 0 && Hxx(Iim) > 0 && Hyy(Iim) > 0
                    Step = -Hmat \ Gvec;
                else
                    Step = zeros(2,1);
                    if isfinite(Hxx(Iim)) && Hxx(Iim) ~= 0
                        Step(1) = -Gx(Iim) ./ Hxx(Iim);
                    end
                    if isfinite(Hyy(Iim)) && Hyy(Iim) ~= 0
                        Step(2) = -Gy(Iim) ./ Hyy(Iim);
                    end
                end
            else
                Step = [0; 0];
            end
    
            Step(1) = sign(Step(1)) .* min(abs(Step(1)), MaxStep);
            Step(2) = sign(Step(2)) .* min(abs(Step(2)), MaxStep);
    
            if ~isfinite(Step(1)); Step(1) = 0; end
            if ~isfinite(Step(2)); Step(2) = 0; end
    
            StepX(Iim) = Step(1);
            StepY(Iim) = Step(2);
        end
    end
end


function [StepX, StepY, AppFlux] = gradDescentPSF2DGN( ...
    Cube, Std, PSF, DX, DY, VecXrel, VecYrel, FitRadius2, SmallStep, MaxStep, ShiftMethod, UseMex)

    Nim = numel(DX);

    [~, Flux, ShiftedPSF] = internalCalcChi2( ...
        Cube, Std, PSF, DX, DY, VecXrel, VecYrel, FitRadius2, ShiftMethod, UseMex);

    AppFlux = Flux(:).';

    SX = reshape(SmallStep(:), 1, 1, []);
    SY = SX;

    PSF_Xp = localShiftPSF(PSF, DX + SmallStep(:).', DY,                  ShiftMethod);
    PSF_Xm = localShiftPSF(PSF, DX - SmallStep(:).', DY,                  ShiftMethod);
    PSF_Yp = localShiftPSF(PSF, DX,                  DY + SmallStep(:).', ShiftMethod);
    PSF_Ym = localShiftPSF(PSF, DX,                  DY - SmallStep(:).', ShiftMethod);

    if UseMex
         [StepX, StepY] = imUtil.sources.mex.psfPhotCube_step2dgn_mex( ...
                                Cube, Std, ShiftedPSF, PSF_Xp, PSF_Xm, PSF_Yp, PSF_Ym, ...
                                SmallStep(:), SmallStep(:), Flux(:), DX(:), DY(:), VecXrel(:), VecYrel(:), ...
                                FitRadius2, MaxStep);
    else

        dPdx = (PSF_Xp - PSF_Xm) ./ (2 .* SX);
        dPdy = (PSF_Yp - PSF_Ym) ./ (2 .* SY);
    
        [Ny, Nx, ~] = size(Cube);
        if isempty(FitRadius2)
            Flag = true(Ny, Nx, Nim);
        else
            MatX = reshape(VecXrel, 1, Nx, 1) - reshape(DX, 1, 1, []);
            MatY = reshape(VecYrel, Ny, 1, 1) - reshape(DY, 1, 1, []);
            MatR2 = MatX.^2 + MatY.^2;
            Flag = MatR2 < FitRadius2;
        end
    
        W = 1 ./ max(Std.^2, eps(class(Std)));
        if size(W,1)==1 && size(W,2)==1
            W = repmat(W, size(Cube,1), size(Cube,2), 1);
        end
        
        Flux3 = reshape(Flux, 1, 1, []);
        Resid = Cube - ShiftedPSF .* Flux3;
        
        Jx = Flux3 .* dPdx;
        Jy = Flux3 .* dPdy;
    
       
    
        StepX = zeros(1, Nim, 'like', Flux);
        StepY = zeros(1, Nim, 'like', Flux);
    
        for Iim = 1:Nim
            Wi  = W(:,:,Iim);
            Fi  = Flag(:,:,Iim);
    
            Ri  = Resid(:,:,Iim);
            Jxi = Jx(:,:,Iim);
            Jyi = Jy(:,:,Iim);
    
            Wi  = Wi(Fi);
            Ri  = Ri(Fi);
            Jxi = Jxi(Fi);
            Jyi = Jyi(Fi);
    
            Good = isfinite(Wi) & isfinite(Ri) & isfinite(Jxi) & isfinite(Jyi);
            Wi   = Wi(Good);
            Ri   = Ri(Good);
            Jxi  = Jxi(Good);
            Jyi  = Jyi(Good);
    
            if isempty(Wi)
                continue;
            end
    
            A11 = sum(Wi .* Jxi .* Jxi);
            A12 = sum(Wi .* Jxi .* Jyi);
            A22 = sum(Wi .* Jyi .* Jyi);
    
            B1  = sum(Wi .* Jxi .* Ri);
            B2  = sum(Wi .* Jyi .* Ri);
    
            A = [A11, A12; A12, A22];
            B = [B1; B2];
    
            if all(isfinite(A), 'all') && all(isfinite(B)) && rcond(A) > 1e-10
                Step = A \ B;
            else
                Step = [0; 0];
                if isfinite(A11) && A11 > 0
                    Step(1) = B1 ./ A11;
                end
                if isfinite(A22) && A22 > 0
                    Step(2) = B2 ./ A22;
                end
            end
    
            Step(1) = sign(Step(1)) .* min(abs(Step(1)), MaxStep);
            Step(2) = sign(Step(2)) .* min(abs(Step(2)), MaxStep);
    
            if ~isfinite(Step(1)); Step(1) = 0; end
            if ~isfinite(Step(2)); Step(2) = 0; end
    
            StepX(Iim) = Step(1);
            StepY(Iim) = Step(2);
        end
    end
end


function Step = localParabolaStep(Par, MaxStep)
    Curv = Par(3,:);
    Grad = Par(2,:);

    Step = zeros(size(Grad));

    Good = abs(Curv) > 0 & isfinite(Curv) & isfinite(Grad);
    Step(Good) = -Grad(Good) ./ (2 .* Curv(Good));

    BadMin = Good & (Curv < 0);
    Step(BadMin) = -Step(BadMin);

    Step(~isfinite(Step)) = 0;
    Step = sign(Step) .* min(abs(Step), MaxStep);
end


function [Chi2, Flux, ShiftedPSF, Dof, FluxErr] = internalCalcChi2( ...
    Cube, Std, PSF, DX, DY, VecXrel, VecYrel, FitRadius2, ShiftMethod, UseMex)

    [Ny, Nx, Nim] = size(Cube);

    ShiftedPSF = localShiftPSF(PSF, DX, DY, ShiftMethod);


    if UseMex
        [Chi2, Flux, Dof, FluxErr] = imUtil.sources.mex.psfPhotCube_chi2flux_mex( ...
                            Cube, Std, ShiftedPSF, DX, DY, VecXrel, VecYrel, FitRadius2);
    else

        if isempty(FitRadius2)
            Flag = true(Ny, Nx, Nim);
        else
            MatX = reshape(VecXrel, 1, Nx, 1) - reshape(DX, 1, 1, []);
            MatY = reshape(VecYrel, Ny, 1, 1) - reshape(DY, 1, 1, []);
            MatR2 = MatX.^2 + MatY.^2;
            Flag  = MatR2 < FitRadius2;
        end
    
        W = 1 ./ max(Std.^2, eps(class(Std)));
    
        Num = sum(Flag .* W .* Cube .* ShiftedPSF, [1 2], 'omitnan');
        Den = sum(Flag .* W .* ShiftedPSF.^2,      [1 2], 'omitnan');
    
        Den = max(Den, eps(class(Den)));
        Flux = squeeze(Num ./ Den);
    
        Flux3 = reshape(Flux, 1, 1, []);
        Resid = Cube - ShiftedPSF .* Flux3;
    
        ResidStd = Flag .* Resid ./ Std;
        Chi2 = squeeze(sum(ResidStd.^2, [1 2], 'omitnan'));
    
        Dof = squeeze(sum(Flag, [1 2], 'omitnan') - 3);
        FluxErr = sqrt(1 ./ squeeze(Den));
    end
end


function ShiftedPSF = localShiftPSF(PSF, DX, DY, ShiftMethod)
    if isempty(DX)
        ShiftedPSF = zeros([size(PSF), 0], 'like', PSF);
        return;
    end

    switch ShiftMethod
        case 1
            ShiftedPSF = imUtil.trans.mex.shift_lanczos3(PSF, DX, DY);
        case 2
            ShiftedPSF = imUtil.trans.shift_fft(PSF, DX, DY);
        otherwise
            error('Uknkonw  ShiftMethod option');
    end
end