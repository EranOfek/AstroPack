function Result=lsqRelPhot(InstMag, Args)
    % Perform relative photometry calibration using the linear least square method.
    %   This function solves the following linear problem:
    %   m_ij = z_i + M_j + alpha*C + ... (see Ofek et al. 2011).
    %   By default will perfporm two iterations, where in the second
    %   iteration, the errors will be taken from the magnitude vs. std
    %   diagram, and stars with bad measurments will be removed.
    % Input  : - A matrix of instrumental magnitude, in which the epochs
    %            are in the rows, and stars are in the columns.
    %            If empty, then run a simulation.
    %          * ...,key,val,...
    %            'MagErr' - A scalar, or a matrix of errors in the
    %                   instrumental magnitudes.
    %            'Method' - LSQ solver method: 'lscov'.
    %                   Default is 'lscov'.
    %            'Algo' - ALgorithm for the lscov function: 'chol'|'orth'.
    %                   Default is 'chol'.
    %            'Niter' - Number of iterations for error estimation and
    %                   bad source removal. Default is 2.
    %            'MaxStarStd' - In the second iteration, remove stars with
    %                   std larger than this value. Default is 0.1.
    %            'UseMagStdErr' - If true, then in the second iteration
    %                   will replace the MagErr with the errors (per star)
    %                   estimated from the mag vs. std plot.
    %                   Default is true.
    %            'CalibMag' - A vector of calibrated magnitude for all the
    %                   stars. You can use NaN for unknown/not used
    %                   magnitudes. If empty, then do not calibrate.
    %                   Default is [].
    %            'Sparse' - Use sparse matrices. Default is true.
    %            
    %            'ZP_PrefixName' - In the column names cell of the design matrix, this is the
    %                   prefix of the images zero point.
    %            'MeanMag_PrefixName' - In the column names cell of the design matrix, this is the
    %                   prefix of the stars mean magnitudes.
    %            'StarProp' - A cell array of vectors. Each vector
    %                   must be of the length equal to the number of stars.
    %                   Each vector in the cell array will generate a new
    %                   column in the design matrix with a property per
    %                   star (e.g., color of each star).
    %                   Default is {}.
    %            'StarPropNames' - A cell array of names for the StarProp
    %                   column names. If this is a string than will be the
    %                   string prefix, with added index. Default is 'SP'.
    %            'ImageProp' - Like StarProp but for the images.
    %                   E.g., airmass.
    %                   Default is {}.
    %            'ImagePropNames' - Like StarPropNames, but for the images.
    %                   Default is 'IP'.
    %            'ThresholdSigma' - Threshold in sigmas (std) for flagging good
    %                   data (used by imUtil.calib.resid_vs_mag).
    %                   Default is 3.
    %            'resid_vs_magArgs' - A cell array of arguments to pass to 
    %                   imUtil.calib.resid_vs_mag
    %                   Default is {}.
    % Output : - A structure with the following fields:
    %            .Par   - All fitted parameters.
    %            .ParErr - Error in all fitted parameters.
    %            .ParZP - Fitted ZP parameters
    %            .ParMag - Fitted mean mag parameters.
    %            .Resid - All residuals.
    %            .Flag - Logical flags indicating which stars where used in
    %                   the solution.
    %            .NusedMeas - Number of used measurments.
    %            .StdResid - Std of used residuals.
    %            .RStdResid - Robust std of used residuals..
    %            .Stdstar - Std of each star measurments used in the
    %                   solution over all epochs.
    %            .StdImage - Std of each image measurments used in the
    %                   solution over all stars.
    %            .AssymStd - Assymptoic rms in the mag vs. std plot,
    %                   estimated from the minimum of the plot.
    %                   (Return NaN if Niter=1).
    %            .MagAssymStd - Magnitude of the assymptotic rms.
    %                   (Return NaN if Niter=1).
    %            .ColNames - Column names of the solution.
    % Author : Eran Ofek (Jun 2023)
    % Example: imUtil.calib.lsqRelPhot; % simulation mode
    
    arguments
        InstMag                    = [];
        Args.MagErr                = 0.02;
        Args.Method                = 'lscov';
        Args.Algo                  = 'chol';  % 'chol' | 'orth'
        Args.Niter                 = 2;
        Args.MaxStarStd            = 0.1;
        Args.UseMagStdErr logical  = true;
        Args.CalibMag              = [];
        
        Args.Sparse logical        = true;
        Args.ZP_PrefixName         = 'Z';
        Args.MeanMag_PrefixName    = 'M';
        Args.StarProp              = {};  % one vector of properties per star - e.g., color
        Args.StarPropNames         = 'SP';
        Args.ImageProp             = {};  % one vector of properties per image - e.g., airmass
        Args.ImagePropNames        = 'IP';
        
        Args.ThresholdSigma        = 3;
        Args.resid_vs_magArgs cell = {};
    end
    
    if isempty(InstMag)
        % run in simulation mode
        
        MagErr = 0.03;
        Nimage = 50;
        Nstar  = 300;
        Mag    = rand(Nstar,1).*10;
        ZP     = rand(Nimage,1).*2;

        InstMag = ZP + Mag.';
        InstMag = InstMag + MagErr.*randn(size(InstMag));
        %InstMag(100) +InstMag + 0.5;
        % add outliers
        
        
        Args.MagErr = MagErr;
    end
    
    [Nimage, Nstar] = size(InstMag);
    
    if numel(Args.MagErr)==1
        ErrY = Args.MagErr.*ones(Nimage.*Nstar,1);
    else
        ErrY = Args.MagErr(:);
    end
    VarY = ErrY.^2;
    
    Y    = InstMag(:);

    if isempty(Args.CalibMag)
        AddCalibBlock = false;
    else
        AddCalibBlock = true;
        Y             = [Y; Args.CalibMag(:)];
    end
    
    Flag = true(size(Y));
    for Iiter=1:1:Args.Niter
        
                
        [H,CN] = imUtil.calib.calibDesignMatrix(Nimage, Nstar, 'Sparse',Args.Sparse,...
                                                               'ZP_PrefixName',Args.ZP_PrefixName,...
                                                               'MeanMag_PrefixName',Args.MeanMag_PrefixName,...
                                                               'StarProp',Args.StarProp,...
                                                               'StarPropNames',Args.StarPropNames,...
                                                               'ImageProp',Args.ImageProp,...
                                                               'ImagePropNames',Args.ImagePropNames,...
                                                               'AddCalibBlock',AddCalibBlock);
        % remove NaNs
        Flag = Flag & ~isnan(Y);
        
        switch lower(Args.Method)
            case 'lscov'
                [Par, ParErr] = lscov(H(Flag,:), Y(Flag), 1./VarY(Flag), Args.Algo);
                Resid = Y - H*Par;  % all residuals (including bad stars)
            otherwise
                error('Unknown Method option');
        end
        
        ParZP  = Par(1:Nimage);
        ParMag = Par(Nimage+1:end);
        % Std per star
        ResidSquare = reshape(Resid,[Nimage, Nstar]);
        FlagSquare  = reshape(Flag,[Nimage, Nstar]);
        ResidSquare(~FlagSquare) = NaN;  % set anused stars to NaN
        StdStar     = std(ResidSquare, [], 1, 'omitnan');
        
        if Iiter<Args.Niter
            % skip this step in the last iteration
            [FlagResid,Res] = imUtil.calib.resid_vs_mag(ParMag(:), StdStar(:), Args.ThresholdSigma, Args.resid_vs_magArgs{:});
            FlagResid = repmat(FlagResid(:),[1, Nimage]).';
            
            % calc VarY
            if Args.UseMagStdErr
                NewErr = repmat(Res.InterpStdResid.', Nimage, 1);
                VarY   = NewErr(:);
            end
            
            MatStdStar = repmat(StdStar, Nimage, 1);
            
            FlagResid = FlagResid(:);
            Flag = Flag & FlagResid & MatStdStar(:)<Args.MaxStarStd;
        end
        
        %std(ParZP - ZP)   % should be eq to MagErr/sqrt(Nimage)
        %std(ParMag  - Mag)  % should be eq to MagErr/sqrt(Nstar)
        
    end
    
    Result.Par       = Par;
    Result.ParErr    = ParErr;
    Result.ParZP     = ParZP;
    Result.ParMag    = ParMag;
    Result.Resid     = Resid;
    Result.Flag      = Flag;
    Result.NusedMeas = sum(Flag);
    Result.StdResid  = std(Resid(Flag), [], 1, 'omitnan');
    Result.RStdResid = tools.math.stat.rstd(Resid(Flag));
    Result.StdStar   = StdStar;
    Result.StdImage  = std(ResidSquare, [], 2, 'omitnan');
    
    if Args.Niter>1
        [Result.AssymStd, Imin] = min(Res.InterpStdResid);
        Result.MagAssymStd      = Res.Mag(Imin);
    else
        Result.AssymStd    = NaN;
        Result.MagAssymStd = NaN;
    end
    Resilt.ColNames         = CN;
end
