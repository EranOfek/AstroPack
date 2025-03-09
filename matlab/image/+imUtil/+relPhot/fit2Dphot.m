function [Result] = fit2Dphot(InstMag, RefMag, X, Y, Args)
    % Fit a 2D X & Y dependent photometric zero point to instrumental mag
    %   Given a vector of instrumental mag and reference mag, and their X
    %   and Y positions (and optional color info), fit a function of the
    %   form: InstMag - RefMag = poly(X,Y) + F(colors) to the data and
    %   return the zero point per source.
    % Input  : - Vector of instrumntal mag.
    %          - Vector of ref. mag. (same length as inst. mag).
    %          - Vector of X position (same length as inst. mag).
    %          - Vector of Y position (same length as inst. mag).
    %          * ...,key,val,... 
    %            'MagErr' - A vector of mag errors. If [] then set to 1.
    %                   Default is [].
    %            'Poly' - A cell array of polynomals order.
    %                   Each cell represents a free parameter.
    %                   Each cell contains an array with two rows, for X
    %                   and Y. The numbres in the columns represents the
    %                   orders to sum.
    %                   For example, [1 2;0 1] may reult in fitting:
    %                   a.*(X.^1.*Y.^0 + X.^2.*Y), where a is the free
    %                   parameter to fit.
    %                   Default is {[0; 0], [1; 0], [0; 1], [1; 1]}
    %            'UseFlag' - A vector of logocals indicating which data
    %                   points will participate in the fit.
    %                   If empty, use all data points in first iteration.
    %                   Default is [].
    %            'NormCoo' - A logical indicating if to normalize the X/Y
    %                   coordinates prior to fitting. Default is true.
    %            'NormCooRange' - [min X, max X, min Y, max Y] for
    %                   coordinates normalization. If empty, then calculate
    %                   from data. Default is [].
    %            'Color' - An array with "color" information to add to the
    %                   fit. Default is [].
    %            'Camera' - A ector of "camera index" per source. Sources
    %                   with the same camera index will have a common zero
    %                   point. If empty, then do not add. Default is [].
    %            'SolveMethod' - Fitting method:
    %                   '\' - use baclash, no weights and errors.
    %                   'lscov' - use lscov.
    %            'SigmaClip' - Sigma clipping. Default is [-3 3].
    %            'Niter' - Number of iterations. Default is 2.
    % Output : - A structure with the following fields:
    %            .ModelMag - Fitted Model mag (H*Par)
    %            .Resid    - Residuals for all sources.
    %            .UseFlag  - Vector of UseFlag logicals.
    %            .RStdGood - Robust std for used sources.
    % Author : Eran Ofek (2025 Feb) 
    % Example: R = imUtil.relPhot.fit2Dphot

    arguments
        InstMag                = [];
        RefMag                 = [];
        X                      = [];
        Y                      = [];
        Args.MagErr            = [];
        Args.Poly              = {[0; 0], [1; 0], [0; 1], [1; 1]};
        Args.UseFlag           = [];
        Args.NormCoo           = true;
        Args.NormCooRange      = [];  % [1 1726 1 1726];
        Args.Color             = []; % columns of "colors"
        Args.Camera            = [];
        Args.SolveMethod       = 'lscov';  % '\' | 'lscov'
        Args.SigmaClip         = [-3 3];
        Args.Niter             = 2;
    end
    
    if isempty(InstMag)
        % simulation mode
        
        Nsrc   = 500;
        RefMag = rand(Nsrc,1).*5;
        Args.MagErr = 1.*( 10.^(interp1((0:1:5), -3+(0:1:5)./2.5, RefMag)));
        InstMag = 1 + RefMag + randn(Nsrc,1).*Args.MagErr;
        X       = rand(Nsrc,1).*1726;
        Y       = rand(Nsrc,1).*1726;
        
        InstMag = InstMag + 0.00001.*X - 0.000015.*Y;
        
    end
    
            
    InstMag  = InstMag(:);
    RefMag   = RefMag(:);
    DeltaMag = InstMag - RefMag;
    
    if isempty(Args.MagErr)
        Args.MagErr = ones(size(InstMag));
    end
    
    X        = X(:);
    Y        = Y(:);
    Nsrc = numel(InstMag);
    if isempty(Args.UseFlag)
        UseFlag = true(Nsrc,1);
    else
        UseFlag = Args.UseFlag;
    end
    
    if Args.NormCoo
        % Normalzie the X/Y coordinates to the -1 to 1 range
        if ~isempty(Args.NormCooRange)
            RangeX = Args.NormCooRange(1:2);
            RangeY = Args.NormCooRange(3:4);
        else
            RangeX = [min(X), max(X)];
            RangeY = [min(Y), max(Y)];
        end
        
        HalfX = 0.5.*(RangeX(2) - RangeX(1));
        HalfY = 0.5.*(RangeY(2) - RangeY(1));
        MidX  = 0.5.*(RangeX(2) + RangeX(1));
        MidY  = 0.5.*(RangeY(2) + RangeY(1));
            
        X = (X - MidX)./HalfX;
        Y = (Y - MidY)./HalfY;
    end
    
    % construct the design matrix
    Npoly = numel(Args.Poly);
    H     = zeros(Nsrc, Npoly);
    for Ipoly=1:1:Npoly
        H(:,Ipoly) = sum(X.^Args.Poly{Ipoly}(1,:), 2) .* sum(Y.^Args.Poly{Ipoly}(2,:), 2);
    end
    
    if ~isempty(Args.Color)
        % add color information to the design matrix
        Hcolor = Args.Color;
        H      = [H, Hcolor];
    end
    
    if ~isempty(Args.Camera)
        % camera index per source
        UnInd = unique(Args.Camera);
        Nind  = numel(UnInd);
        Hcam  = zeros(Nsrc, Nind);
        for Iind=1:1:Nind
            Hcam(:,Iind) = double(UnInd(Iind)==Args.Camera);
        end
        H = [H, Hcam];
    end
    
    for Iiter=1:1:Args.Niter
        switch Args.SolveMethod
            case '\'
                Par = H(UseFlag,:)\DeltaMag(UseFlag);
                ParErr = nan(size(Par));

            case 'lscov'
                [Par, ParErr] = lscov(H(UseFlag,:), DeltaMag(UseFlag), 1./(Args.MagErr(UseFlag).^2));

            otherwise
                error('Unknown SolveMethod option');
        end

        ModelMag   = H*Par;
        Resid      = DeltaMag - ModelMag;
        RStdGood   = tools.math.stat.rstd(Resid(UseFlag));
        ResidSigma = Resid./RStdGood;
        if Iiter<Args.Niter
            UseFlag    = UseFlag & (ResidSigma>Args.SigmaClip(1) & ResidSigma<Args.SigmaClip(2));
        end

    end
    
    
    Result.ModelMag = ModelMag;
    Result.Resid    = Resid;
    Result.UseFlag  = UseFlag;
    Result.RStdGood = RStdGood;
    Result.Par      = Par;
    Result.H        = H;
        
end
