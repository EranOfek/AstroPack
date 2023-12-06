function [Param, Res, Tran] = fitTransformation(ObjCat, ObjRef, Args)
    % Fit an exact transformation between two matched catalogs
    %   The returned transformation is from the reference to the catalog.
    % Input  : - Catalog in one of the following formats:
    %            1. AstroImage object containing an AstroCatalog.
    %            2. AstroCatalog object
    %            3. A 3-column matrix [X,Y,Mag].
    %            The fit is done one to many or many to many.
    %          - Like catalog, but for the reference.
    %          * ...,key,val,...
    %            'Tran' - A Tran2D object containing the required
    %                   transformation to fit. Defaut is Tran2D.
    %            'MaxIter' - Maximum number of fitting iterations.
    %                   In the second iteration, the observations are
    %                   weighted by their mean residual as a function of
    %                   magnitude. Defaut is 2.
    %            'ErrPos' - The positional errors to be used per source
    %                   in the first iteration [pixels]. In the second iteration,
    %                   these errors will be added in quadrature to the
    %                   mean residuals as a function of magnitude.
    %                   Default is 1e-5.
    %            'Norm'  - empty - do nothing, NaN - auto, or [centerX, RangeX, centerY, RangeY]
    %                   Default is [].
    %            'FitMethod' - Options are 'lscov' | '\'. Default is 'lscov'.
    %            'Algo' - Options are 'orth' | 'chol'. Default is 'chol'.
    %               resid_vs_mag:
    %            'MagRange' - Mag range. If empty no limit. Default is [].
    %            'MaxResid' - Maximum allowed residuals [pix]. Default is 1.
    %            'BinMethod' - Binning method: 'bin'|'poly'. Default is 'bin'
    %            'PolyDeg' - Poly order. Default is 3.
    %            'BinSize' - Bin size [mag]. Default is 1.
    %            'FunMean' - Function handle for mean in in bin.
    %                   Defaut is @tools.math.stat.nanmedian
    %            'FunStd' - Function handle for std in bin.
    %                   Default is @imUtil.background.rstd
    %            'InterpMethod' - Interpolation method. Default is 'linear'
    %            'ThresholdSigma' - Threshold in sigmas. Default is 3.
    %               column names:
    %            'ColCatX  - X column name in Catalog. Default is AstroCatalog.DefNamesX
    %            'ColCatY' - Y column name in Catalog. Default is AstroCatalog.DefNamesY
    %            'ColCatM' - Mag column name in Catalog. Default is {'MAG','mag','MAG_PSF','MAG_CONV'}
    %            'ColRefX' - X column name in Reference. Default is AstroCatalog.DefNamesX
    %            'ColRefY' - Y column name in Reference. Default is AstroCatalog.DefNamesY
    %            'ColRefM' - Mag column name in Reference. Default is {'MAG','mag','MAG_PSF','MAG_CONV'}
    %            'ColRefC' - Color column name in Reference. Default is {'Color'}
    %            'ColRefAM' - AirMass column name in Reference. Default is {'AIRMASS'}
    %            'ColRefPA' - Par Ang. column name in Reference. Default is {'ParAng'}
    %            'ColRefModX' - mod(X) column name in Reference. Default is {'ModX'}
    %            'ColRefModY' - mod(Y) column name in Reference. Default is {'ModY'};
    % Output : - A structure array of fitted parameters.
    %          - A structure array of results.
    %          - A Tran2D object for each image.
    % Example: Nsrc = 1000;
    %          Cat = rand(Nsrc,3).*[1024 1024 10];
    %          Ref = Cat + 0.1.*randn(Nsrc,3);
    %          Ref = [Ref, rand(Nsrc,1).*2];
    %          T   = Tran2D;
    %          [Param, Res] = imProc.trans.fitTransformation(Cat, Ref, 'Tran',T);

   
    arguments
        ObjCat
        ObjRef
        Args.Tran Tran2D    = Tran2D;      % optional Tran2d object - override Fun... 
        Args.MaxIter      = 2;
        Args.ErrPos       = 1e-5;  % total error in one axis - all contributions
        Args.Norm         = NaN;   % empty - do nothing, NaN - auto, or [centerX, RangeX, centerY, RangeY]
        Args.FitMethod    = 'lscov'; % 'lscov' | '\'
        Args.Algo         = 'chol'; % 'orth' | 'chol'
        
        % resid_vs_mag arguments
        Args.MagRange     = [];  % empty - no limit
        Args.MaxResid     = 1;   % pixels
        Args.BinMethod    = 'bin'; % 'bin' | 'poly'
        Args.PolyDeg      = 3;
        Args.BinSize      = 1;
        Args.FunMean      = @tools.math.stat.nanmedian;
        Args.FunStd       = @imUtil.background.rstd;
        Args.InterpMethod = 'linear';
        Args.ThresholdSigma = 3;
        
        % columns
        Args.ColCatX      = AstroCatalog.DefNamesX;
        Args.ColCatY      = AstroCatalog.DefNamesY;
        Args.ColCatM      = AstroCatalog.DefNamesMag; %{'MAG','mag','MAG_PSF','MAG_CONV'};
        Args.ColRefX      = AstroCatalog.DefNamesX;
        Args.ColRefY      = AstroCatalog.DefNamesY;
        Args.ColRefM      = AstroCatalog.DefNamesMag; %{'MAG','mag','MAG_PSF','MAG_CONV'};
        Args.ColRefC      = {'Color'};
        Args.ColRefAM     = {'AIRMASS'};
        Args.ColRefPA     = {'ParAng'};
        Args.ColRefModX   = {'ModX'};
        Args.ColRefModY   = {'ModY'};
        
    end
    DefColX = 'X';
    DefColY = 'Y';
    
    if isnumeric(ObjCat)
        Ncat = 1;
    else
        Ncat = numel(ObjCat);
    end
    if isnumeric (ObjRef)
        Nref = 1;
    else
        Nref = numel(ObjRef);
    end
    Nmax = max(Ncat, Nref);
    for Imax=1:1:Nmax
        %
        Icat = min(Imax, Ncat);
        Iref = min(Imax, Nref);
        
        if isa(ObjCat,'AstroImage')
            Cat = ObjCat(Icat).CatData;
        elseif isa(ObjCat,'AstroCatalog')
            Cat = ObjCat(Icat);
        elseif isnumeric(ObjCat)
            Cat = AstroCatalog({ObjCat});
            Cat.ColNames = {DefColX, DefColY, 'MAG'};
        else
            error('ObjCat must be of AstroImage or AstroCatalog or numeric type');
        end
        if isa(ObjRef,'AstroImage')
            Ref = ObjRef(Iref).CatData;
        elseif isa(ObjRef,'AstroCatalog')
            Ref = ObjRef(Iref);
        elseif isnumeric(ObjRef)
            Ref = AstroCatalog({ObjRef});
            Ref.ColNames = {DefColX, DefColY, 'MAG'};
        else
            error('ObjRef must be of AstroImage or AstroCatalog or numeric type');
        end
        
        % columns indices
        ColCatX = colnameDict2ind(Cat, Args.ColCatX);
        ColCatY = colnameDict2ind(Cat, Args.ColCatY);
        ColCatM = colnameDict2ind(Cat, Args.ColCatM);
        
        ColRefX = colnameDict2ind(Ref, Args.ColRefX);
        ColRefY = colnameDict2ind(Ref, Args.ColRefY);
        ColRefM = colnameDict2ind(Ref, Args.ColRefM);
        ColRefC = colnameDict2ind(Ref, Args.ColRefC);
        ColRefAM = colnameDict2ind(Ref, Args.ColRefAM);
        ColRefPA = colnameDict2ind(Ref, Args.ColRefPA);
        ColRefModX = colnameDict2ind(Ref, Args.ColRefModX);
        ColRefModY = colnameDict2ind(Ref, Args.ColRefModY);
        
        
        % fit the transformation
        [Param(Imax), Res(Imax), ResLoop, Tran(Imax)] = imUtil.patternMatch.fit_astrometric_tran(Cat.Catalog, Ref.Catalog, ...
                                                                         'Tran',Args.Tran,...
                                                                         'MaxIter',Args.MaxIter,...
                                                                         'ErrPos',Args.ErrPos,...
                                                                         'Norm',Args.Norm,...
                                                                         'FitMethod',Args.FitMethod,...
                                                                         'Algo',Args.Algo,...
                                                                         'ColCatX',ColCatX, 'ColCatY',ColCatY, 'ColCatM',ColCatM,...
                                                                         'ColRefX',ColRefX, 'ColRefY',ColRefY, 'ColRefM',ColRefM,...
                                                                         'ColRefC',ColRefC, 'ColRefAM',ColRefAM, 'ColRefPA', ColRefPA,...
                                                                         'ColRefModX',ColRefModX, 'ColRefModY',ColRefModY,...
                                                                         'MagRange',Args.MagRange,...
                                                                         'MaxResid',Args.MaxResid,...
                                                                         'BinMethod',Args.BinMethod,...
                                                                         'PolyDeg',Args.PolyDeg,...
                                                                         'BinSize',Args.BinSize,...
                                                                         'FunMean',Args.FunMean,...
                                                                         'FunStd',Args.FunStd,...
                                                                         'InterpMethod',Args.InterpMethod,...
                                                                         'ThresholdSigma',Args.ThresholdSigma);
                                                                         

        %
        
        
        
    end
    
end