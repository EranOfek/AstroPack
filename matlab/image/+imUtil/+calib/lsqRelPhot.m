function Result=lsqRelPhot(InstMag, Args)
    % Perform relative photometry calibration using the linear least square method.
    % 
    % Example: imUtil.calib.lsqRelPhot;
    
    arguments
        InstMag                    = [];
        Args.MagErr                = 0.02;
        Args.Method                = 'lscov';
        Args.Algo                  = 'chol';  % 'chol' | 'orth'
        Args.Niter                 = 2;
        Args.CalibMag              = [];
        
        Args.Sparse logical        = true;
        Args.ZP_PrefixName         = 'Z';
        Args.MeanMag_PrefixName    = 'M';
        Args.StarProp              = {};  % one vector of properties per star - e.g., color
        Args.StarPropNames         = 'SP';
        Args.ImageProp             = {};  % one vector of properties per image - e.g., airmass
        Args.ImagePropNames        = 'IP';
        
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
        StdStar     = std(ResidSquare, [], 1, 'omitnan');
        
        if Iiter<Args.Niter
            % skip this step in the last iteration
            [FlagResid,Res] = imUtil.calib.resid_vs_mag(ParMag(:), StdStar(:), Args.resid_vs_magArgs{:});
            FlagResid = repmat(FlagResid(:),[1, Nimage]).';
            FlagResid = FlagResid(:);
            Flag = Flag & FlagResid;
        end
        
        %std(ParZP - ZP)   % should be eq to MagErr/sqrt(Nimage)
        %std(ParMag  - Mag)  % should be eq to MagErr/sqrt(Nstar)
        
    end
    
    Result.ParZP     = ParZP;
    Result.ParMag    = ParMag;
    Result.Resid     = Resid;
    Result.Flag      = Flag;
    Result.StdResid  = std(Resid(Flag));
    Result.RStdResid = tools.math.stat.rstd(Resid(Flag));
    
    
end
