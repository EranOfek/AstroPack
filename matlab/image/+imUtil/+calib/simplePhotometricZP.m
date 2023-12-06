function [Result,Y,VarY]=simplePhotometricZP(InstrumentalMag, CalibMag , Args)
    % Simple photometric ZP solver
    %       Given instrumental magnitude and calibrated magnitude fit a ZP
    %       including color terms, airmass, position, width:
    %       InstMag-calibMag = ZP + a1*color + a2*color^2 + ... + a3*AM +
    %               a4*AM*color + a5*width + a6*X + a7*Y + ...
    %       Note that the median of each vector is subtracted befire the
    %       fit.
    % Input  : - A one or two column matrix of instrumental [Mag, Err]
    %          - A one or two column matrix of catalog calibrated [Mag, Err]
    %          * ...,key,val,...
    %            'Color' - Vector of colors per each source.
    %                   If empty, then do not fit color. Default is [].
    %            'AM' - Vector of AirMass per each source.
    %                   If empty, then do not fit airmass. Default is [].
    %            'Width' - Vector of width per each source.
    %                   If empty, then do not fit width. Default is [].
    %            'X' - Vector of X coordinate per each source.
    %                   If empty, and Y is empty, then do not fit width. Default is [].
    %            'Y' - Vector of Y coordinate per each source.
    %                   If empty, and X is empty, then do not fit width. Default is [].
    %            'ColorOrder' - Order of color terms to fit.
    %                   I.e., a*color + b*color^2 + ...
    %                   Default is 2.
    %            'ColorAM' - A logical indicating if to fit AM*color term.
    %                   Default is false.
    %            'PosOrder' - A two column matrix of [X,Y] orders to fit.
    %                   Default is [1 0;0 1; 1 1].
    %                   I.e., a*X + b*Y + c*X*Y
    %            'SubMed' - A logical indicating if to subtract the mean
    %                   from each fitted column before the fit (center
    %                   normalization). Default is true.
    %            'MedFun' - Mean function to use in subtraction.
    %                   Default is @median.
    %            'MedFunArgs' - A cell array of additional arguments to
    %                   pass to the 'MedFun'. Default is {1,'omitnan'}.
    %            'SigmaClip' - Sigmacliping [low, high].
    %                   Default is [-3 3].
    %            'Niter' - Number of fitting sigma-cliiping iterations.
    %                   Default is 2.
    % Output : - A structure of fitted results including the following
    %            fields:
    %            .ColNames - A cell array of fitted arguments,
    %                   corresponding to the vector of parameters.
    %            .MeanVec - A vector of subtracted mean values from each
    %                   column.
    %            .Par - Vector of best fitted parameters.
    %            .ParErr - Vector of fitted parameter errors.
    %            .AllResid - vector of all residuals (including non-used
    %                   values).
    %            .FlagGood - A vector of logical indicating which values
    %                   were used in the final fit.
    %            .Chi2 - Chi^2
    %            .Ndof - d.o.f
    %            .Nsrc - Number of sources.
    %          - Y vector. I.e., InstrumentalMag - CalibMag
    %          - VarY (variance in Y) vector.
    % Author : Eran Ofek (Dec 2022)
    % Example: InstrumentalMag = rand(100,1).*6;
    %          CalibMag = InstrumentalMag + 2 + randn(100,1).*0.03;
    %          Color    = rand(100,1).*0.2;
    %          Result=imUtil.calib.simplePhotometricZP(InstrumentalMag,CalibMag,'Color',Color);
    
    arguments
        InstrumentalMag
        CalibMag
        Args.Color           = [];
        Args.AM              = [];
        Args.Width           = [];
        Args.X               = [];
        Args.Y               = [];
        Args.MaxMagErr       = 0.015;
        Args.ColorOrder      = 2;
        Args.ColorAM logical = false;
        Args.PosOrder        = [1 0; 0 1; 1 1];
        Args.SubMed logical  = true;
        Args.MedFun          = @median
        Args.MedFunArgs cell = {1,'omitnan'};
        Args.SigmaClip       = [-1.5 1.5];
        Args.Niter           = 2;
    end
    
    Nsrc = size(InstrumentalMag,1);
    % design matrix and cols
    H = ones(Nsrc,1);
    ColNames = {'ZP'};
    MeanVec  = [0];
    for Ico=1:1:Args.ColorOrder
        if ~isempty(Args.Color)
            H = [H, Args.Color.^Ico];
            ColNames = [ColNames, sprintf('Color%d',Ico)];
            if Args.SubMed
                if Ico==1
                    Med = Args.MedFun(H(:,end), Args.MedFunArgs{:});
                end
                H(:,end) = H(:,end) - Med;
                MeanVec = [MeanVec, Med];
            else
                MeanVec = [MeanVec, 0];
            end
                
        end
    end
    if ~isempty(Args.AM)
        H = [H, Args.AM];
        ColNames = [ColNames, 'AM'];
        if Args.ColorAM
            % mixed AM x color term
            H = [H, Args.AM.*Args.Color];
            ColNames = [ColNames, 'ColorAM'];
            if Args.SubMed
                Med = Args.MedFun(H(:,end), Args.MedFunArgs{:});
                H(:,end) = H(:,end) - Med;
                MeanVec = [MeanVec, Med];
            else
                MeanVec = [MeanVec, 0];
            end
        end
    end
    
    if ~isempty(Args.Width)
        H = [H, Args.Width];
        ColNames = [ColNames, 'Width'];
        if Args.SubMed
            Med = Args.MedFun(H(:,end), Args.MedFunArgs{:});
            H(:,end) = H(:,end) - Med;
            MeanVec = [MeanVec, Med];
        else
            MeanVec = [MeanVec, 0];
        end
    end
    
    if ~isempty(Args.X) && ~isempty(Args.Y)
        Npo = size(Args.PosOrder,1);
        for Ipo=1:1:Npo
            H = [H, Args.X.^PosOrder(Ipo,1) .*Args.Y.^PosOrder(Ipo,2)];
            ColNames = [ColNames, sprintf('X%dY%d',PosOrder(Ipo,:))];
            if Args.SubMed
                Med = Args.MedFun(H(:,end), Args.MedFunArgs{:});
                H(:,end) = H(:,end) - Med;
                MeanVec = [MeanVec, Med];
            else
                MeanVec = [MeanVec, 0];
            end
        end
    end
    
    if size(InstrumentalMag,2)>1
        ErrInstMag = InstrumentalMag(:,2);
    else
        ErrInstMag = zeros(Nsrc,1);
    end
    if size(CalibMag,2)>1
        ErrCalibMag = CalibMag(:,2);
    else
        ErrCalibMag = zeros(Nsrc,1);
    end
    
    %H = H(:,1:2);

    Y    = InstrumentalMag(:,1) - CalibMag(:,1);
    VarY = ErrInstMag.^2 + ErrCalibMag.^2;  % variance
    
    if all(VarY==0)
        VarY = ones(Nsrc,1);
    end
    
    FlagNN = ~isnan(Y) & ~any(isnan(H),2) & VarY<(Args.MaxMagErr.^2);
    
    for Iiter=1:1:Args.Niter
        [Par, ParErr] = lscov(H(FlagNN,:), Y(FlagNN), 1./VarY(FlagNN));

        Resid = Y - H*Par;
        %Std   = nan(Nsrc,1);

        Std   = tools.math.stat.rstd(Resid(FlagNN));

        Z = Resid./Std;
        
        if Iiter~=Args.Niter
            FlagSC = Z>Args.SigmaClip(1) & Z<Args.SigmaClip(2);
            FlagNN = FlagNN & FlagSC;
        end
    end
    
    %Par = [Par; 0; 0];

    Result.ColNames = ColNames;
    Result.MeanVec  = MeanVec;
    Result.Par      = Par;
    Result.ParErr   = ParErr;
    Result.AllResid = Resid;
    Result.FlagGood = FlagNN;
    Result.Chi2     = sum(Resid(FlagNN).^2./VarY(FlagNN));
    Result.Ndof     = sum(FlagNN) - numel(Result.Par);
    Result.Nsrc     = Nsrc;
    
    
end
