function [Result, AstrometricCat, Obj] = astrometryCore(Obj, Args)
    % A core function for astrometry. Match pattern and fit transformation.
    % Input  : - An AstroCatalog object, with sources X, Y positions.
    %            This can be a multiple element object. In this case, the
    %            RA/Dec coordinate refers to all the images.
    %          * ...,key,val,...
    %            'RA' - A single J2000.0 RA coordinates
    %                   in rad, deg, or sexagesimal string.
    %                   This is a mandatory argument.
    %            'Dec' - A single J2000.0 Dec coordinates
    %                   in rad, deg, or sexagesimal string.
    %                   This is a mandatory argument.
    %            'CooUnits' - RA/Dec coordinates units ('deg','rad').
    %                   This is ignored if RA/Dec are sexagesimal.
    %                   Default is 'deg'.
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
    %            'argsFilterForAstrometry' - A cell array of additional
    %                   arguments to pass to imProc.cat.filterForAstrometry
    %                   Default is {}.
    %            'argsFitPattern' - A cell array of additional arguments to
    %                   pass to imProc.trans.fitPattern
    %                   Default is {}.
    %            'ProjType' - Projection type. See imProc.trans.projection.
    %                   Default is 'TPV'.
    %            'ImageCenterXY' - [X,Y] in pixels of image center
    %                   corresponding to the guess coordinates.
    %                   If empty, will take the 0.5.*max(XY), where XY are
    %                   the source pixel coordinates.
    %                   Note that this arguments is in the Result output.
    %                   Default is [].
    %            'Scale' - Value, or range of scale [arcse/pix].
    %                   Default is 1.0
    %            'RotationRange' - Range of rotations to test [deg].
    %                   Default is [-90, 90].
    %            'RotationStep' - Rotation step. Default is 0.2 [deg].
    %            'RangeX' - Range of X shift to test [pixels].
    %                   Default is [-1000 1000].
    %            'RangeY' - Range of Y shift to test [pixels].
    %                   Default is [-1000 1000].
    %            'StepX' - Step in X shift histogram. Default is 4.
    %            'StepY' - Step in Y shift histogram. Default is 4.
    %            'Flip' - A two column matrix of [X, Y] flips to test.
    %                   Default is [1 1; 1 -1;-1 1;-1 -1].
    %            'SearchRadius' - Matching search radius [pixels].
    %                   Default is 5.
    %            'Tran' - A Tran2D object describing the 2D transformation
    %                   to fit.
    %                   Default is Tran2D.
    %            'MaxSol2Check' - Maximum number of solution candidates to
    %                   test (solutions from fitPattern). Default is 3.
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
    % Output : - A structure array of results.
    %            Element per input catalog.
    %            Available fields are:
    %            'ImageCenterXY' - The [X, Y] image center [pix] used in the solution. 
    %            'ResPattern' - The structure output with candidate
    %                   solutions from fitPattern.
    %            'Nsolutions' - Number of solutions found by fitPattern.
    %               If Nsolutions>0 then the following fields are available
    %               'Tran' - A Tran2D object per solution, containing the
    %                       transformation (after projection).
    %               'Res' - A structure array (per solution) with the best
    %                       fit transformation information, including rms and
    %                       number of matches.
    %               'ErrorOnMean' - A vector of assymptoticRMS/sqrt(Ngood)
    %                       for each solution.
    %               'BestInd' - Index of solution with minimal ErrorOnMean.
    %          - An AstroCatalog object containing the astrometric catalog
    %            used, after applying proper motions, and queryRange.
    %          - An handle to the original input catalog, after adding the
    %            RA/Dec columns for all the sources.
    %            The input catalog is modified only if more than two output
    %            arguments are requested.
    % Author : Eran Ofek (Jul 2021)
    % Example: Result = imProc.astrometry.astrometryCore(AI.CatData, 'RA',149.1026601, 'Dec',69.4547688, 'CatColNamesMag','MAG_CONV_2');
   
    arguments
        Obj AstroCatalog
        Args.RA
        Args.Dec
        Args.CooUnits                     = 'deg';
        Args.CatName                      = 'GAIAEDR3';  % or AstroCatalog
        Args.CatOrigin                    = 'catsHTM';
        Args.CatRadius                    = 1400;
        Args.CatRadiusUnits               = 'arcsec'
        Args.Con                          = {};
        
        Args.RefColNameMag                = {'Mag_BP','Mag'};
        Args.RefRangeMag                  = [12 19.5];
        Args.RefColNamePlx                = {'Plx'};
        Args.RefRangePlx                  = [-Inf 50];
        
        Args.EpochOut                     = [];
        Args.argsProperMotion cell        = {};
        Args.argsFilterForAstrometry cell = {};
        Args.argsFitPattern cell          = {};
        
        Args.ProjType                     = 'TPV';
        Args.ImageCenterXY                = [];  % attempt to identify automatically
        
        Args.Scale                        = 1.0;      % range or value [arcsec/pix]
        Args.RotationRange(1,2)           = [-90, 90];
        Args.RotationStep(1,1)            = 0.2;
        
        Args.RangeX(1,2)                  = [-1000 1000]; 
        Args.RangeY(1,2)                  = [-1000 1000]; 
        Args.StepX(1,1)                   = 4;
        Args.StepY(1,1)                   = 4;
        Args.Flip(:,2)                    = [1 1; 1 -1;-1 1;-1 -1]; % [1 -1]
        Args.SearchRadius(1,1)            = 5;
        
        
        Args.MaxSol2Check                 = 3;      % maximum number of solutions to check
        Args.TranMethod char              = 'TPV';   % 'TPV' | 'tran2d'
        Args.Tran                         = Tran2D;                           
        Args.ErrPos                       = 0.1;
        Args.Niter                        = 2; 
        Args.FitMethod char               = 'lscov';
        Args.MaxResid                     = 1;
        Args.MagRange                     = [];
        Args.BinMethod                    = 'bin';   % 'bin' | 'poly'
        Args.PolyDeg                      = 3;
        Args.BinSize                      = 1;
        Args.FunMean                      = @median;
        Args.FunStd                       = @imUtil.background.rstd;
        Args.InterpMethod                 = 'linear';
        Args.ThresholdSigma               = 3;         
        
        % add RA/Dec to input catalog (only if nargout>2)
        Args.OutCatCooUnits               = 'deg';
        Args.OutCatColRA                  = 'RA';
        Args.OutCatColDec                 = 'Dec';
        Args.OutCatColPos                 = Inf;
        
        Args.CatColNamesX                 = AstroCatalog.DefNamesX;
        Args.CatColNamesY                 = AstroCatalog.DefNamesY;
        Args.CatColNamesMag               = AstroCatalog.DefNamesMag;
        Args.RefColNamesRA                = AstroCatalog.DefNamesRA;
        Args.RefColNamesDec               = AstroCatalog.DefNamesDec;
        
    end
    RAD        = 180./pi;
    ARCSEC_DEG = 3600;
    % The name of the projected X/Y coordinates in the Reference astrometric catalog
    RefColNameX = 'X';
    RefColNameY = 'Y';
    
    RotationEdges = (Args.RotationRange(1):Args.RotationStep:Args.RotationRange(2));
    % mean value of projection scale:
    ProjectionScale = (180./pi) .* 3600 ./ mean(Args.Scale);
        
    if isempty(Args.CatRadius) && iscahr(Args.CatName)
        % attempt to estimate CatRadius automatically
        % from 1st catalog only!
        XY = getXY(Obj(1), 'ColX',Args.CatColNamesX, 'ColY',Args.CatColNamesY);
        Args.CatRadius = sqrt(sum((range(XY).*max(Args.Scale)).^2,2));
        Args.CatRadius = convert.angular('arcsec', Args.CatRadiusUnits, Args.CatRadius);
    end
    
    % Get astrometric catalog / incluidng proper motion
    % RA and Dec output are in radians
    % If CatName is an AstroCatalog, then will retun as is, but RA and Dec
    % will be converted to OutUnits
    [AstrometricCat, RA, Dec] = imProc.cat.getAstrometricCatalog(Args.RA, Args.Dec, 'CatName',Args.CatName,...
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
                                                                                    'RangePlx',Args.RefRangePlx,...
                                                                                    'OutRADecUnits','rad');
          
    % RA/Dec in [deg]
    RAdeg  = RA.*RAD;
    Decdeg = Dec.*RAD;
   
    % Project astrometric catalog
    % set CreateNewObj=true, because AstrometricCat is an output argument.
    %   MUST ALWATS BE true, otherwise, this operation may overide the
    %   existing AstrometricCat in the matlab session
    
    % AstroWCS.alphadelta2phitheta
    % native2interm
    
    ProjAstCat = imProc.trans.projection(AstrometricCat, RA, Dec, ProjectionScale, Args.ProjType, 'Coo0Units','rad',...
                                                                                   'AddNewCols',{RefColNameX,RefColNameY},...
                                                                                   'CreateNewObj',true);
    % ProjAstCat.plot({'X','Y'},'.')   
    
    Nobj = numel(Obj);
    for Iobj=1:1:Nobj
        % filter astrometric catalog
        % set CreateNewObj=true, because otherwise the Catalog will be overwritten
        % FFU: need to add column names...
        [FilteredCat, FilteredProjAstCat, Summary] = imProc.cat.filterForAstrometry(Obj(Iobj), ProjAstCat,...
                                                                                    'ColCatX',Args.CatColNamesX,...
                                                                                    'ColCatY',Args.CatColNamesY,...
                                                                                    'ColCatMag',Args.CatColNamesMag,...
                                                                                    'ColRefX',RefColNameX,...
                                                                                    'ColRefY',RefColNameY,...
                                                                                    'ColRefMag',Args.RefColNameMag,...
                                                                                    'CreateNewObj',true,...
                                                                                    Args.argsFilterForAstrometry{:});


        % The Ref catalog is projected around some center that should coincide
        % with the center of Cat.
        % Therefore, we should shift Cat to its own center
        if isempty(Args.ImageCenterXY)
            % attempt to identify ImageCenterXY automatically
            XY = getXY(Obj(Iobj), 'ColX',Args.CatColNamesX, 'ColY',Args.CatColNamesY);
            Result(Iobj).ImageCenterXY = max(XY).*0.5;
        else
            Result(Iobj).ImageCenterXY = Args.ImageCenterXY;
        end
        imProc.trans.tranAffine(FilteredCat, -Result(Iobj).ImageCenterXY, true); %[-1024 -2048],true);

  
        % Match pattern catalog to projected astrometric catalog
        % FFU: CatColNamesX/Y are for both Cat and Ref!!
        [ResPattern] = imProc.trans.fitPattern(FilteredCat, FilteredProjAstCat, Args.argsFitPattern{:},...
                                                                          'Scale',Args.Scale,...
                                                                          'HistRotEdges',RotationEdges,...
                                                                          'RangeX',Args.RangeX,...
                                                                          'RangeY',Args.RangeY,...
                                                                          'StepX',Args.StepX,...
                                                                          'StepY',Args.StepY,...
                                                                          'Flip',Args.Flip,...
                                                                          'SearchRadius',Args.SearchRadius,...
                                                                          'ColNamesX',Args.CatColNamesX,...
                                                                          'ColNamesY',Args.CatColNamesY);

        % go over possible solutions:
        Result(Iobj).Nsolutions = numel(ResPattern.Sol.SN);   % number of candidate solutions
        Result(Iobj).ResPattern = ResPattern;

        if Result(Iobj).Nsolutions==0
            % no solution found

        else

            % assume solutions are ordered by S/N
            Nsol = min(Args.MaxSol2Check, Result(Iobj).Nsolutions);
            for Isol=1:1:Nsol
                % Apply affine transformation to Reference
                % CreateNewObj=true, because FilteredProjAstCat is needed later on
                TransformedProjAstCat = imProc.trans.tranAffine(FilteredProjAstCat, ResPattern.Sol.AffineTran{Isol}, true,...
                                                                'ColX',RefColNameX,...
                                                                'ColY',RefColNameY);

                % match sources based on X/Y positions:
                [MatchedCat,UM,TUM] = imProc.match.match(FilteredCat, TransformedProjAstCat,...
                                                 'Radius',Args.SearchRadius,...
                                                 'CooType','pix',...
                                                 'ColCatX',Args.CatColNamesX,...
                                                 'ColCatY',Args.CatColNamesY,...
                                                 'ColRefX',RefColNameX,...
                                                 'ColRefY',RefColNameY);
                % Count the number of matches
                Flag = ~isnan(MatchedCat.Catalog(:,1));
                Nmatches = sum(Flag);

                % DEBUGING
                % show table of [X, Y, RA, Dec] of matched sources    
                % Note that XPEAK/YPEAK and X/Y are different because of the shift
                % applied (shift coo to image center)
                %XY = getCol(MatchedCat,{'XPEAK','YPEAK','X','Y'});   % X/Y in image
                %XY = XY(Flag,:);
                %RF = celestial.coo.convertdms(TransformedProjAstCat.Catalog(Flag,1),'r','SH');
                %DF = celestial.coo.convertdms(TransformedProjAstCat.Catalog(Flag,2),'r','SD');
                %T  = table(XY(:,1),XY(:,2), XY(:,3), XY(:,4), RF, DF)

                % Fit transformation
                %[Param, Res] = imProc.trans.fitTransformation(TransformedCat, FilteredProjAstCat, 'Tran',Args.Tran);
                % MatchedRef has the same number of lines as in Ref,
                % but it is affine transformed to the coordinate system of Cat

                [Xcat,~,IndCatX] = getColDic(MatchedCat, Args.CatColNamesX);
                [Ycat,~,IndCatY] = getColDic(MatchedCat, Args.CatColNamesY);
                Xref = getColDic(FilteredProjAstCat, RefColNameX);
                Yref = getColDic(FilteredProjAstCat, RefColNameY);
                Mag  = getColDic(FilteredProjAstCat, Args.RefColNameMag);
                
                
                % fit the catalog to the reference and generate the Tran2D
                % object and all the information required for the WCS
                
                [Tran, ParWCS, ResFit] = imProc.astrometry.fitAstrometry(Xcat, Ycat, Xref, Yref, Mag, RAdeg, Decdeg,...
                                                       'ImageCenterXY',Result(Iobj).ImageCenterXY,...
                                                       'Scale',ResPattern.Sol.Scale(Isol),...
                                                       'ProjType',Args.ProjType,...
                                                       'TranMethod',Args.TranMethod,...
                                                       'Tran',Args.Tran,...
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
        
        
               Result(Iobj).ParWCS(Isol) = ParWCS;
               
        if 1==0        
                switch lower(Args.TranMethod)
                    case 'tpv'
                        % The following fitting attempt to mimic the order
                        % of the TAN-TPV transformation
                        

                        % fit affine transformation (only)
                        % Units are "pixels"
                        Tran1 = Tran2D('poly1');
                        % note that independent and dependent coordinates change
                        % order
                        [Tran1, ResFit] = fitAstrometricTran(Tran1,...
                                                Xref, Yref,...
                                                Xcat, Ycat,...
                                                'ExtraData',[],...
                                                'Mag',Mag,...
                                                'ErrPos',Args.ErrPos,...
                                                'Niter',1,...
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
                        CRPIX1     = Tran1.ParX(1);
                        CRPIX2     = Tran1.ParY(1);
                        % CD matrix
                        CD         = [Tran1.ParX(2:3).'; Tran1.ParY(2:3).'];  % [pix]
                        % measured scale in arcsec/pix
                        ScaleASpix = ResPattern.Sol.Scale(Isol);
                        CD         = CD.*ScaleASpix./ARCSEC_DEG;   % [deg]

                        % Apply affine transformation on Xcat, Ycat
                        Xsi  = CD(1,1).*(Xcat - CRPIX1) + CD(1,2).*(Ycat - CRPIX2);
                        Eta  = CD(2,1).*(Xcat - CRPIX1) + CD(2,2).*(Ycat - CRPIX2);

                        % convert Xref, Yref to deg
                        XrefT = Xref.*ScaleASpix./ARCSEC_DEG;   % [deg]
                        YrefT = Yref.*ScaleASpix./ARCSEC_DEG;   % [deg]

                        % parameters from which to construct WCS
                        Result(Iobj).ParWCS(Isol).CRPIX  = [CRPIX1, CRPIX2] + Result(Iobj).ImageCenterXY(:).';
                        Result(Iobj).ParWCS(Isol).CRVAL  = [RAdeg, Decdeg];
                        Result(Iobj).ParWCS(Isol).CD     = CD;
                        Result(Iobj).ParWCS(Isol).CUNIT  = {'deg', 'deg'};
                        Result(Iobj).ParWCS(Isol).CTYPE  = {sprintf('RA---%s',upper(Args.ProjType)), sprintf('DEC--%s',upper(Args.ProjType))};
                        Result(Iobj).ParWCS(Isol).NAXIS  = 2;
                        
                        [Tran, ResFit] = fitAstrometricTran(Args.Tran,...
                                                XrefT, YrefT,...
                                                Xsi, Eta,...
                                                'ExtraData',[],...
                                                'Mag',Mag,...
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

                    case 'tran2d'
                        % a full Tran2D solution, where the observations
                        % are the independent variable
                        % and the fit is done in pixel (observations) units
                        % i.e., the logical thing to do
                        [Tran, ResFit] = fitAstrometricTran(Args.Tran,...
                                                Xcat, Ycat,...
                                                Xref,Yref,...
                                                'ExtraData',[],...
                                                'Mag',Mag,...
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
                        
                                                                    
%                 [Param, Res, Tran] = imProc.trans.fitTransformation(FilteredProjAstCat, MatchedCat,...
%                                                               'Tran',Args.Tran,...
%                                                               'Norm',NaN,...
%                                                               'ColRefX',Args.CatColNamesX,...
%                                                               'ColRefY',Args.CatColNamesY,...
%                                                               'ColCatX',RefColNameX,...
%                                                               'ColCatY',RefColNameY);
                    otherwise
                        error('Unknown TranMethod option');
                end
                        
        end  % 1==0
        
        
                                                          
                % store transformations
                Result(Iobj).Tran(Isol)       = Tran;
                Result(Iobj).ResFit(Isol)     = ResFit;

                
                %
                Step = 10;
                CXY = Result(Iobj).ImageCenterXY;
                VecX = (-CXY(1):Step:CXY(1));
                VecY = (-CXY(2):Step:CXY(2));
                [MatX, MatY] = meshgrid(VecX, VecY);
                
                
                %[x,y]=backward(Result(Iobj).Tran(1),[MatX(:), MatY(:)]);
                
                            
                
            end
            
            % classify the quality of solutions
            %[Result(Iobj).Res.RMS]
            Result(Iobj).ErrorOnMean = [Result(Iobj).ResFit.AssymRMS_mag]./sqrt([Result(Iobj).ResFit.Ngood]);
            [~,Result(Iobj).BestInd] = min(Result(Iobj).ErrorOnMean);

            % Generate AstroWCS for best solution
            StructWCS = Result(Iobj).ParWCS(Result(Iobj).BestInd);
            KeyValWCS = namedargs2cell(StructWCS);
            Result(Iobj).WCS = AstroWCS.tran2wcs(Result(Iobj).Tran(Result(Iobj).BestInd), KeyValWCS{:});
            
            % add RA/Dec to the catalog
            if nargout>2
                [ObjSrcRA, ObjSrcDec] = Result(Iobj).WCS.xy2sky(Obj(Iobj).getCol(IndCatX), Obj(Iobj).getCol(IndCatY));
                Obj(Iobj).insertCol([ObjSrcRA, ObjSrcDec], Args.OutCatColPos, {Args.OutCatColRA, Args.OutCatColDec}, {Args.OutCatCooUnits, Args.OutCatCooUnits})
            end
            
        end
        

    end
    
    
end