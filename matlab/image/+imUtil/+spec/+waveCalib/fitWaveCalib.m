function [Result] = fitWaveCalib(PixPos, WavePos, Args)
    % Robust polynomial fitting of pixel position and wavelength.
    %       Will fit a polynomial function between a matched line list of
    %       pixel positions and wavelength. The fitting include a RANSAC
    %       step that will remove strong outliers, and a sigma clipping
    %       step that will do the final clipping.
    %       The function will also return a function that enable converting
    %       pixel positions to wavelenth.
    % Input  : - A vector of pixel positions.
    %            Alternatively, if WavePos (second input arg) is empty,
    %            then this is a two column matrix of [PixPos, WavePos].
    %          - A vector of wavelength positions, corresponding to the
    %            pixels positions.
    %            Default is [].
    %          * ...,key,val,...
    %            'PolyOrder' - A vector of polynomials order to fit.
    %                   Default is [0 1 2].
    %            'Niter' - Number of sigma clipping iterations.
    %                   If 1, then no sigma clipping.
    %                   Default is 2.
    %            'SigmaClip' - [Low, High] Sigma clipping in units of std.
    %                   Default is [3 3].
    %            'StdFun' - Function handle for the std/robust std
    %                   calculation.
    %                   Default is @(x) 1.253.*mad(x)
    %
    %            RANSAC parameters:
    %            'Nsim' - Number of RANSAC simulations.
    %                   If empty, then skip the RANSAC outlier removal.
    %                   Default is 30.
    %            'Nrem' - Number of points to remove on each RANSAC
    %                   simulation. Default is 2.
    %            'MinNunique' - Minimum nuber of unique data points
    %                   required in each RANSAC simulation.
    %                   If empty, then will set it to number oof parameters
    %                   (i.e., numel(PolyOrder)) + 1.
    %                   Default is [].
    %            'SigmaClipRANSAC' - The [Low, High] sigma clipping for the
    %                   RANSAC strong outliers removal.
    %                   Default is [20 20].
    % Output : - A structure containing the following fields:
    %            .Par - Best fit parameters.
    %            .ParErr - Best fit parameters errots.
    %            .MidPixPos - Mid point of PixPos used in the unity
    %                   normalization.
    %            .HalfRangePixPos - Half range of PixPos used in the unity
    %                   normalization.
    %            .PolyOrder - Vector of polynomial orders.
    %            .ResidAll - Vector of all residuals (including unused bad
    %                   points).
    %            .Flag - Vector of logicals indicating which points were
    %                   used in the final fit.
    %            .Std - RMS of final fit.
    %            .RStd - Robust rms, calculated using the StdFun.
    %            .pix2wave - A function handle for converting pixel
    %                   positions to wavelength.
    %                   call this using Result.pix2wave(PixPosVector, Result)
    %                   where Result is the output of this function, and
    %                   PixPosVector is a vector of pixels position.
    %
    % Author : Eran Ofek (2023 Dec) 
    % Example: % last point is outlier
    %          PixPos = [100  4000; 200 5000; 300 6000; 350 6500; 450 7500; 490 7900; 150 4100]
    %          PixPos = PixPos +randn(size(PixPos)).*1;
    %          R = imUtil.spec.waveCalib.fitWaveCalib(PixPos,[])
    %          % use the pix2wave function:
    %          R.pix2wave(PixPos(:,1), R) - PixPos(:,2)
    
    arguments
        PixPos
        WavePos                = [];
        Args.PolyOrder         = [0 1 2];
        Args.Niter             = 3;
        Args.SigmaClip         = [3 3];
        
        Args.StdFun            = @(x) 1.253.*mad(x);
        
        Args.Nsim              = 30;  % if empty, skip RANSAC
        Args.Nrem              = 2;
        Args.MinNunique        = [];  % if empty, use Npar + 1
        Args.SigmaClipRANSAC   = [20 20];
    end
    
    if isempty(WavePos)
        % assume PixPos is a two column matrix
        if size(PixPos,2)==2
            WavePos = PixPos(:,2);
            PixPos  = PixPos(:,1);
        else
            error('If WavePos is empty, then PixPos must contains two columns [PixPos, WavePos]');
        end
    end
    
    % convert to column vectors
    PixPos  = PixPos(:);
    WavePos = WavePos(:);
    
    % normalize PixPos to unity:
    MinPixPos       = min(PixPos);
    MaxPixPos       = max(PixPos);
    HalfRangePixPos = MaxPixPos - MinPixPos;
    MidPixPos       = MinPixPos + HalfRangePixPos;
    NormPixPos      = (PixPos - MidPixPos)./HalfRangePixPos;
    
    % design matrix
    Nlines = numel(PixPos);
    H      = NormPixPos.^(Args.PolyOrder(:).');
    
    % RANSAC iteration:
    % select Args.Nrem points to remove 
        
    if isempty(Args.Nsim)
        % skip RANSAC
        Flag   = true(Nlines,1);
    else
        if isempty(Args.MinNunique)
            Args.MinNunique = numel(Args.PolyOrder) + 1;
        end
        if (Nlines-Args.Nrem)>numel(Args.PolyOrder)
            [Flag, ~, ~] = tools.math.fit.ransacLinearModel(H, WavePos, 'NptSim',Nlines-Args.Nrem,...
                                                                    'MinNunique',Args.MinNunique,...
                                                                    'Nsim',Args.Nsim,...
                                                                    'NsigmaClip',Args.SigmaClipRANSAC);
        end
    end
        
    
    for Iiter=1:1:Args.Niter
        [Par, ParErr] = lscov(H(Flag,:), WavePos(Flag));
        Resid         = WavePos - H*Par;     % O-C residuals of all points
        ResidGood     = Resid(Flag);         % O-C residuals of good points only
        Std           = Args.StdFun(ResidGood);
        Z             = Resid./Std;  % residuals in units of std
        if Iiter<Args.Niter
            % do not do this at last iteration
            % selet good (within sigma clip range)
            Flag          = Flag & Z<Args.SigmaClip(2) & Z>-abs(Args.SigmaClip(1));
        end
    end
        
    Result.Par       = Par;
    Result.ParErr    = ParErr;
    Result.MidPixPos = MidPixPos;
    Result.HalfRangePixPos = HalfRangePixPos;    
    Result.PolyOrder       = Args.PolyOrder;
    
    Result.ResidAll  = Resid;
    Result.Flag      = Flag;
    Result.Std       = std(ResidGood);
    Result.RStd      = Std;

    Result.pix2wave  = @(PixPos, R) ( ((PixPos(:)-R.MidPixPos)./R.HalfRangePixPos) .^(R.PolyOrder(:).')) * R.Par;
    
end
