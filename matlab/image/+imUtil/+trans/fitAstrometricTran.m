function Result = fitAstrometricTran(Xin,Yin, Xdep,Ydep, Tran, Args)
    %
    
    
    arguments
        Xind
        Yind
        Xdep
        Ydep
        Args.ExtraData   % Array with Ndata columns
        Args.Tran
        Args.ErrPos
        Args.Niter             = 2; 
        Args.FitMethod char    = 'lscov';
    end

    % calculate the design matrix
    [Hx, Hy] = Args.Tran.design_matrix([Xdep, Ydep, Args.ExtraData]);
        


    % fitting
    Iter = 0;
    % fit all sources in first iteration
    FlagSrc = ~isnan(sum(Hx,2)) & ~isnan(sum(Hy,2)) & ~isnan(Xind) & ~isnan(Yind);
    %Hx      = Hx(FlagSrc,:);
    %Hy      = Hy(FlagSrc,:);
    %CatX    = Xind(FlagSrc);
    %CatY    = Yind(FlagSrc);
    Nsrc    = numel(Xind);
    %FlagSrc = true(Nsrc,1);    % 
    Args.ErrPos = Args.ErrPos.*ones(Nsrc,1);

    % formal error only
    Var     = Args.ErrPos.^2;
    % error including additional contributions (e.g., scintilations)
    InvVar  = 1./Var;
    ResResid = [];
    while Iter<Args.MaxIter
        %
        Iter = Iter + 1;

        switch lower(Args.FitMethod)
            case 'lscov'

                %warning('off')
                [ParX,ParErrX] = lscov(Hx(FlagSrc,:), Xind(FlagSrc), InvVar(FlagSrc), InPar.Algo);

                [ParY,ParErrY] = lscov(Hy(FlagSrc,:), Yind(FlagSrc), InvVar(FlagSrc), InPar.Algo);
                %warning('on')

            case '\'
                ParX = Hx(FlagSrc,:)\Xind(FlagSrc);
                ParErrX = nan(size(ParX));

                ParY = Hy(FlagSrc,:)\Yind(FlagSrc);
                ParErrY = nan(size(ParY));

            otherwise
                error('Unknwon FitMethod option');
        end

        % calculate the residuals and rms of the fit (all sources)
        ResidX = Xind - Hx*ParX;
        ResidY = Yind - Hy*ParY;
        Resid  = sqrt(ResidX.^2 + ResidY.^2);
        % RMS is calculated only for selected sources
        RMS_X  = std(ResidX(FlagSrc));
        RMS_Y  = std(ResidY(FlagSrc));
        RMS    = sqrt(RMS_X.^2 + RMS_Y.^2);

        got here...
        
        % screening of sources
        if Iter<InPar.MaxIter
            % select good sources and re-estimate positional error

            % calculate RMS vs. mag.
            [FlagSrc,ResResid] = imUtil.calib.resid_vs_mag(RefMag(FlagSrc),Resid(FlagSrc),...
                                                                  'MagRange',InPar.MagRange,...
                                                                  'BinMethod',InPar.BinMethod,...
                                                                  'PolyDeg',InPar.PolyDeg,...
                                                                  'BinSize',InPar.BinSize,...
                                                                  'FunMean',InPar.FunMean,...
                                                                  'FunStd',InPar.FunStd,...
                                                                  'InterpMethod',InPar.InterpMethod,...
                                                                  'ThresholdSigma',InPar.ThresholdSigma);

            % Applay MagRnage
            if ~isempty(InPar.MagRange)
                FlagSrc = FlagSrc & RefMag>InPar.MagRange(1) & RefMag<InPar.MagRange(2);
            end
            % apply - removing sources with large residuals
            FlagSrc = FlagSrc & Resid<InPar.MaxResid;

            % add error as a function of mag to basic error
            InvVar = 1./(Var + ResResid.InterpMeanResid.^2);

        end

        ResLoop(Iter).Resid = Resid;
        ResLoop(Iter).RMS_X = RMS_X;
        ResLoop(Iter).RMS_Y = RMS_Y;
        ResLoop(Iter).RMS   = RMS;
        ResLoop(Iter).Flag  = FlagSrc;
    end

    [~,ResResid] = imUtil.calib.resid_vs_mag(RefMag(FlagSrc),Resid(FlagSrc),...
                                                                  'MagRange',InPar.MagRange,...
                                                                  'BinMethod',InPar.BinMethod,...
                                                                  'PolyDeg',InPar.PolyDeg,...
                                                                  'BinSize',InPar.BinSize,...
                                                                  'FunMean',InPar.FunMean,...
                                                                  'FunStd',InPar.FunStd,...
                                                                  'InterpMethod',InPar.InterpMethod,...
                                                                  'ThresholdSigma',InPar.ThresholdSigma);

    Param.FunX  = InPar.FunX;
    Param.FunY  = InPar.FunY;

    Param.ParX     = ParX;
    Param.ParY     = ParY;
    Param.ParErrX  = ParErrX;
    Param.ParErrY  = ParErrY;

    Res.ResResid = ResResid;
    Res.Resid    = Resid;
    Res.ResidX   = ResidX;
    Res.ResidY   = ResidY;
    Res.FlagSrc  = FlagSrc;
    Res.Ngood    = sum(FlagSrc);
    Res.Resid    = Resid;
    Res.RefMag   = RefMag;
    Res.RMS_X    = RMS_X;
    Res.RMS_Y    = RMS_Y;
    Res.RMS      = RMS;
    Res.CatX     = CatX;
    Res.CatY     = CatY;
    Res.RefX     = RefX;
    Res.RefY     = RefY;



    if isempty(ResResid)
        % no asymptotic rms
        Res.AssymRMS     = NaN;
        Res.AssymRMS_mag = NaN;
        Res.AssymRMS_RMS = NaN;
    else
        TmpMag = ResResid.InterpMeanResid; %(Res.FlagSrc);
        TmpStd = ResResid.InterpStdResid;  %(Res.FlagSrc);
        [MinMeanRMS, MinMeanInd] = min(TmpMag);
        Res.AssymRMS     = MinMeanRMS;
        Res.AssymRMS_mag = TmpMag(MinMeanInd);
        Res.AssymRMS_RMS = TmpStd(MinMeanInd);
    end
    Res.ResResid = ResResid;


    % populate a tran2dCl object
    if nargout>3
        if isempty(InPar.Tran)
            Tran = Tran2D;
            Tran.FunX  = Param.FunX;
            Tran.FunY  = Param.FunY;
            Tran.FunNX = Param.FunNX;
            Tran.FunNY = Param.FunNY;
            Tran.ParX  = Param.ParX;
            Tran.ParY  = Param.ParY;
            Tran.ParNX = Param.ParNX;
            Tran.ParNY = Param.ParNY;
        else
            Tran.ParX  = Param.ParX;
            Tran.ParY  = Param.ParY;
        end
    end



    

end