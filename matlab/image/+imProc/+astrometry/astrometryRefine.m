function [Result, Obj, AstrometricCat] = astrometryRefine(ObjAC, Args)
    % Refine an astrometric solution of an AstroCatalog object
    %   This function may work on images which have either an approximate
    %   WCS (either in AstroHeader or AstroWCS), or a catalog with RA/Dec
    %   coordinates. The coordinates should be good to a few arcseconds.
    %   For no solutions use imProc.astrometry.astrometryCore.
    % Input  : - An AstroCatalog or AstroImage object (multiple elements supported).
    %          * ...,key,val,...
    %            'SearchRadius' - Sources search radius [arcsec].
    %                   Default is 3.
    %            'IncludeDistortions' - A logical indicating if to include
    %                   the fitted distortions in estimating the RA/Dec of
    %                   the sources in the catalog. Default is true.
    %            'Header' - An AstroHeader object corresponding to the
    %                   AstroCatalog. If exist, then will use the 
    %                   AstroWCS.header2wcs function to generate an
    %                   AstroWCS. If empty, will attempt to use the user
    %                   supplied AstroWCS in the 'WCS' function key.
    %                   Default is [].
    %            'WCS' - An AstroWCS object. If empty, will look for 'RA',
    %                   and 'Dec' columns in the AstroCatalog object.
    %                   Otherwise the RA and Dec will be calculated from
    %                   the AstroWCS object.
    %                   Default is [].
    %            'RA' - RA corrsponding to catalog/image center. This will be
    %                   used as the coordinate for the astrometric catalog
    %                   query and the CRVAL argument.
    %                   If empty, and WCS is given then will estimate from
    %                   WCS. If empty, then will estimate using
    %                   boundingCircle on the catalog. Default is [].
    %            'Dec' - Like Dec, but for the RA. Default is [].
    %            'CooUnits' - Units for the RA and Dec function keys.
    %                   Default is 'deg'.
    %            'Scale' - Catalog/image plate scale ["/pix].
    %                   If empty, will estimate from catalog.
    %                   Default is [].
    %            'ProjType' - Projection type. See imProc.trans.projection.
    %                   Default is 'TPV'. 
    %            'TranMethod' - imProc.astrometry.fitWCS
    %                   transformation method. Default is 'TPV'.
    %            'Tran' - A Tran2D object that defines the fitted
    %                   transformation. Default is Tran2D.
    %            'ErrPos' - Error in positions [pix]. Default is 0.01.
    %            'ExtraData' - Data for additional fitted parameters.
    %                   Matrix of e.g., [AM, PA, Color] with the same
    %                   number of rows as the catalog.
    %                   Default is [].
    %            'Niter' - Number of fitting iterations. Default is 2.
    %            'FitMethod' - Fitting method for Tran2D/fitAstrometricTran
    %                   Default is 'lscov'.
    %            'MaxResid' - Maximum residual to use in fit.
    %                   Default is 0.5.
    %            'MagRange' - [Min Max] max range. Default is [13 19].
    %            'BinMethod' - Method to use:
    %                   'poly' - polynomial fit.
    %                   'bin' - binning the data.
    %                   Default is 'bin'
    %            'PolyDeg' - Polynomial degree for the polynomial fit.
    %                   Default is 3.
    %            'BinSize' - Bin size for binning. Default is 1 (mag).
    %            'FunMean' - A function handle to use when calculating the mean
    %                   of the data in each bin.
    %                   Default is @nanmedian.
    %            'FunStd' - A function handle to use when calculating the std
    %                   of the data in each bin, or when calculating the global
    %                   std after the polynomial fit.
    %                   Default is @imUttil.background.rstd.
    %            'InterpMethod' - Interpolation method. Default is 'linear'.
    %            'ThresholdSigma' - Threshold in sigmas (std) for flagging good
    %                   data. Default is 3.
    %            'CatName' - Either an astrometric catalog name (char
    %                   array) to query around the requested coordinates,
    %                   or an AstroCatalog object containing such a
    %                   catalaog.
    %                   Default is 'GAIAEDR3'.
    %            'CatOrigin' - Catalog origin (relevant if CatName is a
    %                   char array).
    %                   Default is 'catsHTM'.
    %            'CatRadius' - Catalog query radius.
    %                   If empty will attempt to estimate automatically
    %                   from the diagonal of the image in pixels, and the
    %                   max(scale).
    %                   Default is 1400.
    %            'CatRadiusUnits' - CatRadius units.
    %                   Default is 'arcsec'.
    %            'Con' - Additional constraints for the catalog query.
    %                   See catsHTM.cone_search. Default is {}.
    %            'RefColNameMag' - Column name containing mag in the
    %                   astrometric reference catalog.
    %                   Default is {'Mag_BP','Mag'}.
    %            'RefRangeMag' - Magnitude range to retrieve.
    %                   Default is [12 19.5].
    %            'RefColNamePlx' - Parallax column name in the
    %                   astrometric reference catalog.
    %                   Default is {'Plx'}.
    %            'RefRangePlx' - Parllax range to retrieve.
    %                   Default is [-Inf 50].
    %            'EpochOut' - Output epoch. Default units is 'JD' (see
    %                   imProc.cat.applyProperMotion for more options).
    %                   If empty, will not apply proper motion and
    %                   parallax.
    %                   Default is [].
    %            'argsProperMotion' - A cell array of additional arguments
    %                   to pass to imProc.cat.applyProperMotion.
    %                   Default is {}.
    %            'flagSrcWithNeighborsArgs' - A cell array of additional
    %                   arguments to pass to the flagSrcWithNeighbors fun.
    %                   Default is {}.
    %            'ReuseAstrometricCat' - A logical indicating if to reuse
    %                   the astrometric catalog from the first query. This
    %                   is possible only when all the images/catalogs
    %                   corresponds to the same sky location.
    %                   Default is false.
    %            'RemoveNeighboors' - A logical indicating if to remove
    %                   sources with near neighboors from the astrometric
    %                   catalog. Default is true.
    %            'CreateNewObj' - A logical indicating if to create a new
    %                   copy of the input AstroCatalog object.
    %                   Default is true.
    %            'OutCatCooUnits' - Units of RA/Dec added to catalog.
    %                   Default is 'deg'.
    %            'OutCatColRA' - RA Column name added to catalog.
    %                   Default is 'RA'.
    %            'OutCatColDec' - Dec Column name added to catalog.
    %                   Default is 'Dec'.
    %            'OutCatColPos' - Position of RA/Dec columns added to catalog.
    %                   Default is Inf.
    %            'CatColNamesX' - A cell array dictionary of input catalog
    %                   X column name. Default is AstroCatalog.DefNamesX.
    %            'CatColNamesY' - A cell array dictionary of input catalog
    %                   Y column name. Default is AstroCatalog.DefNamesY.
    %            'CatColNamesMag' - A cell array dictionary of input catalog
    %                   magnitude column name. Default is AstroCatalog.DefNamesMag.
    %            'RefColNamesRA' - A cell array dictionary of reference astrometric catalog
    %                   RA column name. Default is AstroCatalog.DefNamesRA.
    %            'RefColNamesDec' - A cell array dictionary of reference astrometric catalog
    %                   Dec column name. Default is AstroCatalog.DefNamesDec.
    % Output : - A structure array with the following fields (each element
    %            corresponds to an AstroCatalog elelemt):
    %            'ParWCS' - The WCS parameters.
    %            'Tran' - The fitted Tran2D object.
    %            'ResFit' - The best fit results summary.
    %            'WCS' - An updated WCS object with the best fit solution.
    %          - The input AstroCatalog objct with new and updated  RA/Dec
    %            columns. The columns are added only if the second output 
    %            argument is requested.
    %          - An AstroCatalog containing the AstrometricCat catalog.
    % Author : Eran Ofek (Aug 2021)
    % Example: RR = imProc.astrometry.astrometryRefine(AI.CatData, 'WCS',Result.WCS, 'CatName',AstrometricCat, 'RA',149.1026601, 'Dec',69.4547688);
    
    arguments
        ObjAC 
        Args.Header                             = []; % If given convert to AstroWCS
        Args.WCS                                = []; % If given generate RA/Dec for sources
        
        Args.SearchRadius                       = 3;    
        Args.IncludeDistortions(1,1) logical    = true;
        
        Args.RA                                 = [];
        Args.Dec                                = [];
        Args.CooUnits                           = 'deg';
        Args.Scale                              = [];
        
        Args.ProjType                           = 'TPV';
        Args.TranMethod                         = 'TPV';
        Args.Tran                               = Tran2D;
        Args.ErrPos                             = 0.01;
        Args.ExtraData                          = [];
        Args.Niter                              = 2;
        Args.FitMethod                          = 'lscov';
        Args.MaxResid                           = 0.5;
        Args.MagRange                           = [13 19];
        Args.BinMethod                          = 'bin';
        Args.PolyDeg                            = 3;
        Args.BinSize                            = 1;
        Args.FunMean                            = @nanmedian;
        Args.FunStd                             = @imUtil.background.rstd;
        Args.InterpMethod                       = 'linear';
        Args.ThresholdSigma                     = 3;
        
        Args.CatName                            = 'GAIAEDR3';  % or AstroCatalog
        Args.CatOrigin                          = 'catsHTM';
        Args.CatRadius                          = 1400;
        Args.CatRadiusUnits                     = 'arcsec'
        Args.Con                                = {};
        
        Args.RefColNameMag                      = {'Mag_BP','Mag'};
        Args.RefRangeMag                        = [12 19.5];
        Args.RefColNamePlx                      = {'Plx'};
        Args.RefRangePlx                        = [-Inf 50];
        
        Args.EpochOut                           = [];
        Args.argsProperMotion cell              = {};
        
        Args.flagSrcWithNeighborsArgs cell      = {};
        Args.ReuseAstrometricCat(1,1) logical   = false;
                
        Args.RemoveNeighboors(1,1) logical      = true;
     
        Args.CreateNewObj(1,1) logical          = true;
        
        % add RA/Dec to input catalog (only if nargout>2)
        Args.OutCatCooUnits                     = 'deg';
        Args.OutCatColRA                        = 'RA';
        Args.OutCatColDec                       = 'Dec';
        Args.OutCatColPos                       = Inf;
        
        Args.CatColNamesX                       = AstroCatalog.DefNamesX;
        Args.CatColNamesY                       = AstroCatalog.DefNamesY;
        Args.CatColNamesMag                     = AstroCatalog.DefNamesMag;
        Args.RefColNamesRA                      = AstroCatalog.DefNamesRA;
        Args.RefColNamesDec                     = AstroCatalog.DefNamesDec;
    end
    RAD        = 180./pi;
    ARCSEC_DEG = 3600;
    % The name of the projected X/Y coordinates in the Reference astrometric catalog
    RefColNameX   = 'X';
    RefColNameY   = 'Y';
    CatColNameRA  = 'RA';
    CatColNameDec = 'Dec';
    
    if Args.CreateNewObj
        Obj = ObjAC.copyObject;
    else
        Obj = ObjAC;
    end
    
    
    % Case 1. AstroCatalog contains RA/Dec
    % Case 2. Use AstroWCS
    % Case 3. Use AstroHeader
    
    Nobj  = numel(Obj);
    Nhead = numel(Args.Header);
    Nwcs  = numel(Args.WCS);
    CooFromBoundingCircle = false;
    AstrometricCat        = AstroCatalog(size(Obj));  % []
    for Iobj=1:1:Nobj
        % for each element in AstroCatalog
        
        if isa(Obj, 'AstroImage')
            Cat = Obj(Iobj).CatData.copyObject;
            % Args.WCS order of priority:
            if isempty(Args.WCS)
                % attempt to get WCS from WCS or HeaderData fields
                if Obj(Iobj).WCS.Success
                    % good WCS found - use it
                    WCS = Obj(Iobj).WCS.copyObject;
                    Nwcs = 1;
                else
                    % get WCS from header
                    WCS = AstroWCS.header2wcs(Obj(Iobj).HeaderData);
                    Nwcs = 1;
                end
            else
                WCS = Args.WCS;
            end
        elseif isa(Obj, 'AstroCatalog')
            Cat = Obj(Iobj).copyObject;
            % Args.WCS order of priority:
            if isempty(Args.WCS)
                WCS = [];
            else
                WCS = Args.WCS;
            end
        else
            error('Unsupported input class. First input must be AstroCatalog or AstroImage');
        end
        
        % get X/Y columns from catalog
        [Xcat,~,IndCatX] = getColDic(Cat, Args.CatColNamesX);
        [Ycat,~,IndCatY] = getColDic(Cat, Args.CatColNamesY);

        % Args.WCS order of priority:
        
%         if ~isempty(Args.Header)
%             % convert AstroHeader to AstroWCS
%             % populate Args.WCS
%             Ihead = min(Iobj, Nhead);
%             Args.WCS = AstroWCS.header2wcs(Args.Header(Ihead));
%             Nwcs     = 1;
%         end
        
        if isempty(WCS)
            % assume RA/Dec are available in AstroCatalog
            [SrcRA, SrcDec] = getLonLat(Cat, 'rad');
        else
            % Convert X/Y to RA/Dec using AstroWCS
            Iwcs = min(Iobj, Nwcs);
            [SrcRA, SrcDec] = WCS(Iwcs).xy2sky(Xcat, Ycat, 'OutUnits','rad',...
                                                           'includeDistortion',Args.IncludeDistortions);
            % add approximate RA, Dec to new copy of catalog
            Cat = insertCol(Cat, [SrcRA, SrcDec], Inf, {CatColNameRA, CatColNameDec}, {'rad', 'rad'});
        end
    
        if CooFromBoundingCircle || isempty(Args.RA) || isempty(Args.Dec)
            if isempty(Args.WCS)
                % estimate RA/Dec of center of catalog from catalog itself
                CircleUnits         = 'deg';
                [Args.RA, Args.Dec, Args.CatRadius] = boundingCircle(Cat,'CooType','pix','OutUnits',CircleUnits); 

                Args.CooUnits       = CircleUnits;
                Args.CatRadiusUnits = CircleUnits;
            else
                % estimate from image center and WCS
                error('not yet available');
                
            end
            CooFromBoundingCircle = true;
        else
            CooFromBoundingCircle = false;
        end
            
        if Args.ReuseAstrometricCat && ~isempty(AstrometricCat) 
            Args.CatName = AstrometricCat(1);
        end
            
        % Get astrometric catalog / incluidng proper motion
        % RA and Dec output are in radians
        % If CatName is an AstroCatalog, then will retun as is, but RA and Dec
        % will be converted to OutUnits
        [AstrometricCat(Iobj), RA, Dec] = imProc.cat.getAstrometricCatalog(Args.RA, Args.Dec, 'CatName',Args.CatName,...
                                                                                        'CatOrigin',Args.CatOrigin,...
                                                                                        'Radius',Args.CatRadius,...
                                                                                        'RadiusUnits',Args.CatRadiusUnits,...
                                                                                        'CooUnits',Args.CooUnits,...
                                                                                        'OutUnits','rad',...
                                                                                        'Con',Args.Con,...
                                                                                        'EpochOut',Args.EpochOut,...
                                                                                        'argsProperMotion',Args.argsProperMotion,...
                                                                                        'ColNameMag',Args.RefColNameMag,...
                                                                                        'RangeMag',Args.RefRangeMag,...
                                                                                        'ColNamePlx',Args.RefColNamePlx,...
                                                                                        'RangePlx',Args.RefRangePlx);

        % RA/Dec in [deg]
        RAdeg  = RA.*RAD;
        Decdeg = Dec.*RAD;
    
        % estimate plate scale
        if isempty(Args.Scale)
            % estimate scale based on distances between sources
            SrcDistRad = celestial.coo.sphere_dist_fast(SrcRA, SrcDec, SrcRA(1), SrcDec(1));
            SrcDistPix = sqrt((Xcat - Xcat(1)).^2 + (Ycat - Ycat(1)).^2);
            Scale = median(SrcDistRad.*RAD.*ARCSEC_DEG./SrcDistPix,'all','omitnan');   % ["/pix]
        else
            Scale = Args.Scale; % ["/pix]
        end
        
        ProjectionScale = RAD .* ARCSEC_DEG ./ Scale;
    
        
        
        % filter Ref - remove sources with neighboors
        if Args.RemoveNeighboors
            % sort AstrometricCat
            AstrometricCat(Iobj).sortrows('Dec');
            
            UseFlag = ~imProc.match.flagSrcWithNeighbors(AstrometricCat(Iobj), Args.flagSrcWithNeighborsArgs{:}, 'CooType','sphere');
        else
            Nsrc    = sizeCatalog(AstrometricCat(Iobj));
            UseFlag = true(Nsrc,1);
        end
        
        
        % projection
        ProjAstCat = imProc.trans.projection(AstrometricCat(Iobj), RA, Dec, ProjectionScale, Args.ProjType, 'Coo0Units','rad',...
                                                                                       'AddNewCols',{RefColNameX,RefColNameY},...
                                                                                       'CreateNewObj',true);
     
        % match the RA/Dec against an external catalog
        % sources in MatchedCat corresponds to sources in ProjAstCat
        [MatchedCat,UM,TUM] = imProc.match.match(Cat, ProjAstCat,...
                                                     'Radius',Args.SearchRadius,...
                                                     'RadiusUnits','arcsec',...
                                                     'CooType','sphere',...
                                                     'AddIndInRef',false,...
                                                     'ColCatX',CatColNameRA,...
                                                     'ColCatY',CatColNameDec,...
                                                     'ColRefX',CatColNameRA,...
                                                     'ColRefY',CatColNameDec);
          
        % Debug: check that the matching is working
        % F=~isnan(MatchedCat.Catalog(:,1));
        % [ProjAstCat.Catalog(F,1:2), MatchedCat.Catalog(F,40:41)].*RAD
        
        
        % Count the number of matches
        Flag = ~isnan(MatchedCat.Catalog(:,1));
        Nmatches = sum(Flag);
                
        [Xcat,~,IndCatX] = getColDic(MatchedCat, Args.CatColNamesX);
        [Ycat,~,IndCatY] = getColDic(MatchedCat, Args.CatColNamesY);
        Xref = getColDic(ProjAstCat, RefColNameX);
        Yref = getColDic(ProjAstCat, RefColNameY);
        Mag  = getColDic(ProjAstCat, Args.RefColNameMag);
        
        % fit
                
        [Tran, ParWCS, ResFit, WCS] = imProc.astrometry.fitWCS(Xcat, Ycat, Xref, Yref, Mag, RAdeg, Decdeg,...
                                                       'ImageCenterXY',Args.WCS.CRPIX,...
                                                       'Scale',Scale,...
                                                       'ProjType',Args.ProjType,...
                                                       'TranMethod',Args.TranMethod,...
                                                       'Tran',Args.Tran,...
                                                       'UseFlag',UseFlag,...
                                                       'ExtraData',[],...
                                                       'ErrPos',Args.ErrPos,...
                                                       'Niter',Args.Niter,...
                                                       'FitMethod',Args.FitMethod,...
                                                       'MaxResid',Args.MaxResid,...
                                                       'MagRange',Args.MagRange,...
                                                       'BinMethod',Args.BinMethod,...
                                                       'PolyDeg',Args.PolyDeg,...
                                                       'BinSize',Args.BinSize,...
                                                       'FunMean',Args.FunMean,...
                                                       'FunStd',Args.FunStd,...
                                                       'InterpMethod',Args.InterpMethod,...
                                                       'ThresholdSigma',Args.ThresholdSigma);
        
        %
        Result(Iobj).ParWCS     = ParWCS;
        % store transformations
        Result(Iobj).Tran       = Tran;
        Result(Iobj).ResFit     = ResFit;
        
        % create an AstroWCS object
        %KeyValWCS = namedargs2cell(Result(Iobj).ParWCS);
        Result(Iobj).WCS = WCS;  %AstroWCS.tran2wcs(Result(Iobj).Tran, KeyValWCS{:});

        % add RA/Dec to the catalog
        if nargout>1
            % update RA/Dec in catalog
            [ObjSrcRA, ObjSrcDec] = Result(Iobj).WCS.xy2sky(Cat.getCol(IndCatX), Cat.getCol(IndCatY), 'OutUnits',Args.OutCatCooUnits);
            Cat = insertCol(Cat, [ObjSrcRA, ObjSrcDec], Args.OutCatColPos, {Args.OutCatColRA, Args.OutCatColDec}, {Args.OutCatCooUnits, Args.OutCatCooUnits});
        

            if isa(Obj, 'AstroImage')
                Obj(Iobj).CatData = Cat;
                % update WCS in AstroImage
                Obj(Iobj).WCS = Result(Iobj).WCS;
                % add WCS kesy to Header
                Obj(Iobj).HeaderData = wcs2head(Obj(Iobj).WCS, Obj(Iobj).HeaderData);
            elseif isa(Obj, 'AstroCatalog')
                Obj(Iobj)         = Cat;
            else
                error('Unsupported input class. First input must be AstroCatalog or AstroImage');
            end
        end
    end
                
end