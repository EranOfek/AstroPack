function [Result, Obj, AstrometricCat] = astrometryCore(Obj, Args)
    % A core function for astrometry. Match pattern and fit transformation.
    %       The function is designed to solve the astrometry of an image in
    %       a single shoot (no partitioning).
    %       A new copy of the catalog is always created.
    % Input  : - An AstroImage with populated CatData or an AstroCatalog object,
    %            with sources X, Y positions.
    %            This can be a multiple element object. In this case, the
    %            RA/Dec coordinate refers to all the images.
    %          * ...,key,val,...
    %            'RA' - A single J2000.0 RA coordinates
    %                   in rad, deg, or sexagesimal string.
    %                   This is a mandatory argument.
    %                   If first input is an AstroImage this can also be an
    %                   header keyword name. Default is 'RA'.
    %            'Dec' - A single J2000.0 Dec coordinates
    %                   in rad, deg, or sexagesimal string.
    %                   This is a mandatory argument.
    %                   If first input is an AstroImage this can also be an
    %                   header keyword name. Default is 'DEC'.
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
    %            'FilterSigma' - Width [sigma units] of Gaussian filter with
    %                   which to cross-correlate the H2 (hits for shifts) matrix.
    %                   If empty, no filtering is applied. Default is 3.
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
    %            'SortCat' - Column name by which to sort the output
    %                   catalog. If empty, do not sort. Default is 'Dec'.
    %            'UpdateHeader' - A logical indicating if to add to the
    %                   header the astrometric quality information.
    %                   The following columns will be added:
    %                   'AST_NSRC','AST_ARMS','AST_ERRM'
    %                   Default is true.
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
    %          - An handle to the original input catalog, after adding the
    %            RA/Dec columns for all the sources.
    %            Optionally sorted by Dec.
    %            The input catalog is modified only if two or more output
    %            arguments are requested.
    %          - An AstroImage or AstroCatalog object containing the astrometric catalog
    %            used, after applying proper motions, and queryRange.
    %            If the input is AstroImage, this is an AstroImage with the
    %            WCS and Headers updated with the new WCS.
    % Author : Eran Ofek (Jul 2021)
    % Example: Result = imProc.astrometry.astrometryCore(AI.CatData, 'RA',149.1026601, 'Dec',69.4547688, 'CatColNamesMag','MAG_CONV_2');
   
    arguments
        Obj                                   % AstroImage or AstroCatalaog
        Args.RA                           = 'RA';
        Args.Dec                          = 'DEC';
        Args.CooUnits                     = 'deg';
        Args.CatName                      = 'GAIADR3';  % or AstroCatalog
        Args.CatOrigin                    = 'catsHTM';
        Args.CatRadius                    = 1400.*2;
        Args.CatRadiusUnits               = 'arcsec'
        Args.Con                          = {};
                
        Args.RefColNameMag                = {'phot_bp_mean_mag','phot_g_mean_mag'}; %{'Mag_BP','Mag'};
        Args.RefRangeMag                  = [12 19]; %.5];
        Args.RefColNamePlx                = {'Plx'};
        Args.RefRangePlx                  = [-Inf 50];
        
        Args.EpochOut                     = [];
        Args.argsGetAstrometricCat cell   = {};
        Args.argsProperMotion cell        = {};
        Args.argsFilterForAstrometry cell = {};
        Args.argsFitPattern cell          = {};
        
        Args.ProjType                     = 'TPV';
        Args.ImageCenterXY                = [];  % attempt to identify automatically
        
        Args.Scale                        = 1.0;      % range or value [arcsec/pix]
        Args.RotationRange(1,2)           = [-90, 90];
        Args.RotationStep(1,1)            = 0.2;
        Args.DistEdges                    = (12:3:300).';   % 12:3:300
        Args.HistDistEdgesRotScale        = [10 600 300];
        
        Args.RangeX(1,2)                  = [-1000 1000].*2;
        Args.RangeY(1,2)                  = [-1000 1000].*2;
        Args.StepX(1,1)                   = 2;
        Args.StepY(1,1)                   = 2;
        Args.Flip(:,2)                    = [1 1; 1 -1;-1 1;-1 -1]; % [1 -1]
        Args.SearchRadius(1,1)            = 6;   
        Args.FilterSigma                  = 3;
        
        Args.MaxSol2Check                 = 3;      % maximum number of solutions to check
        Args.TranMethod char              = 'TPV';   % 'TPV' | 'tran2d'
        Args.Tran                         = Tran2D('poly3');                           
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
        Args.SortCat                      = 'Dec';  % if empty donit sort

        Args.TestNbin             = 3;
        Args.RegionalMaxMedianRMS = 1;     % arcsec OR pix?
        Args.RegionalMaxWithNoSrc = 0;
        Args.MaxErrorOnMean       = 0.05;  % arcsec OR pix?
        
        Args.UpdateHeader logical         = true;
        
        Args.CatColNamesX                 = AstroCatalog.DefNamesX;
        Args.CatColNamesY                 = AstroCatalog.DefNamesY;
        Args.CatColNamesMag               = AstroCatalog.DefNamesMag;
        Args.RefColNamesRA                = AstroCatalog.DefNamesRA;
        Args.RefColNamesDec               = AstroCatalog.DefNamesDec;
        
    end
    RAD         = 180./pi;
    ARCSEC_DEG  = 3600;
    % The name of the projected X/Y coordinates in the Reference astrometric catalog
    RefColNameX = 'X';
    RefColNameY = 'Y';
    
    % ### IF YOU CHANGE SOMETHING IN THIS BLOCK - MAKE THE SAME IN astrometryCore
    %
    
    if isa(Obj, 'AstroImage')
        % can read RA/Dec from Header if AstroImage
        [Args.RA, Args.Dec] = getCoo(Obj(1).HeaderData, 'RA',Args.RA, 'Dec',Args.Dec, 'Units',Args.CooUnits, 'OutUnits',Args.CooUnits);
    else
        [Args.RA, Args.Dec] = celestial.coo.parseCooInput(1, 1, 'InUnits',Args.CooUnits, 'OutUnits',Args.CooUnits);
    end
        
    
    % make sure Tran is a new copy, otherwise may overwrite other Tran
    Args.Tran = Args.Tran.copy;
    
    % get EpochOut
    if isempty(Args.EpochOut)
        if isa(Obj, 'AstroImage')
            Args.EpochOut = julday(Obj);
            if any(isnan(Args.EpochOut))
                Args.EpochOut = [];
            end
        end
    end
    
    % ### END OF COMMON BLOCK

    RotationEdges = (Args.RotationRange(1):Args.RotationStep:Args.RotationRange(2));
    % mean value of projection scale:
    ProjectionScale = (180./pi) .* 3600 ./ mean(Args.Scale);  % [pix/radian]
    NormScale = Args.Scale./mean(Args.Scale);
        
    if isempty(Args.CatRadius) && ischar(Args.CatName)
        % attempt to estimate CatRadius automatically
        % from 1st catalog only!
        if isa(Obj, 'AstroImage')
            XY = getXY(Obj(1).CatData, 'ColX',Args.CatColNamesX, 'ColY',Args.CatColNamesY);
        else
            % assuming Obj is AstroCatalog
            XY = getXY(Obj(1), 'ColX',Args.CatColNamesX, 'ColY',Args.CatColNamesY);
        end
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
                                                                                    'OutRADecUnits','rad',...
                                                                                    Args.argsGetAstrometricCat{:});
          
    % RA/Dec in [deg]
    RAdeg  = RA.*RAD;
    Decdeg = Dec.*RAD;
   
    % Project astrometric catalog
    % set CreateNewObj=true, because AstrometricCat is an output argument.
    %   MUST ALWATS BE true, otherwise, this operation may overide the
    %   existing AstrometricCat in the matlab session
    % Units of X/Y positions in the ProjAstCat are pixels!
    
    % Must copy AstrometricCat, 
    % shallow copy is enough otherwise, this operation may overide the
    %   existing AstrometricCat in the matlab session
    % Shallow copy is enough
    ProjAstCat = AstrometricCat.copy;  
    ProjAstCat = imProc.trans.projection(ProjAstCat, RA, Dec, ProjectionScale, Args.ProjType, 'Coo0Units','rad',...
                                                                                   'AddNewCols',{RefColNameX,RefColNameY},...
                                                                                   'CreateNewObj',false);
    % ProjAstCat.plot({'X','Y'},'.')   
    
    Nobj = numel(Obj);
    
    % allocate Result
    Result = struct('ImageCenterXY',cell(Nobj,1),...
                    'Nsolutions',cell(Nobj,1),...
                    'ResPattern',cell(Nobj,1),...
                    'ErrorOnMean',cell(Nobj,1),...
                    'BestInd',cell(Nobj,1),...
                    'WCS',cell(Nobj,1),...
                    'ParWCS',cell(Nobj,1),...
                    'Tran',cell(Nobj,1),...
                    'ResFit',cell(Nobj,1));
                
    for Iobj=1:1:Nobj
        % filter astrometric catalog
        % set CreateNewObj=true, because otherwise the Catalog will be overwritten
        % FFU: need to add column names...
        
        if isa(Obj, 'AstroImage')
            Cat = Obj(Iobj).CatData;
        elseif isa(Obj, 'AstroCatalog')
            Cat = Obj(Iobj);
        else
            error('Unknown first input argument type - must be AstroImage or AstroCatalog');
        end
        
        % FFU: get X/Y column indices once
        
        % can we add here CreateNewObj=false ? Answer: no - this is messing
        % up the catalog in a bad way - not fully understood
        % ProjAstCat is not used anymore 
        
        % ProjAstCat is not used anymore so no need to copy it
        % make sure you are no overriding the previous catalog
        FilteredCat = Cat.copy;  % shallow copy is enough      
        
%         SN = Cat.getCol('SN_3');
%         FlagSN = SN>100;
%         Cat.Catalog = Cat.Catalog(FlagSN,:);
        
        [FilteredCat, FilteredProjAstCat] = imProc.cat.filterForAstrometry(FilteredCat, ProjAstCat,...
                                                                                    'ColCatX',Args.CatColNamesX,...
                                                                                    'ColCatY',Args.CatColNamesY,...
                                                                                    'ColCatMag',Args.CatColNamesMag,...
                                                                                    'ColRefX',RefColNameX,...
                                                                                    'ColRefY',RefColNameY,...
                                                                                    'ColRefMag',Args.RefColNameMag,...
                                                                                    'CreateNewObj',false,...
                                                                                    Args.argsFilterForAstrometry{:});


        % The Ref catalog is projected around some center that should coincide
        % with the center of Cat.
        % Therefore, we should shift Cat to its own center
        if isempty(Args.ImageCenterXY)
            % attempt to identify ImageCenterXY automatically
            XY = getXY(Cat, 'ColX',Args.CatColNamesX, 'ColY',Args.CatColNamesY);
            Result(Iobj).ImageCenterXY = max(XY).*0.5;
        else
            Result(Iobj).ImageCenterXY = Args.ImageCenterXY;
        end
        FilteredCat = imProc.trans.tranAffine(FilteredCat, -Result(Iobj).ImageCenterXY, true, 'CreateNewObj',false); %[-1024 -2048],true);
  
        % debuging
        %figure(1); FilteredCat.plotSources; axis([-200 50 -400 100]);
        %figure(2); FilteredProjAstCat.plotSources; axis([-200 50 -400 100])
        % figure(1); [Dist1, Theta1, X, Y] = plot.distBetweenPoints
        % figure(2); [Dist2, Theta2, X, Y] = plot.distBetweenPoints
        
        % Match pattern catalog to projected astrometric catalog
        % FFU: CatColNamesX/Y are for both Cat and Ref!!
        % NormScale - is the scale normalized to 1 (as the new ref was
        % sacled)
        [ResPattern] = imProc.trans.fitPattern(FilteredCat, FilteredProjAstCat, Args.argsFitPattern{:},...
                                                                          'Scale',NormScale,...
                                                                          'HistRotEdges',RotationEdges,...
                                                                          'HistDistEdgesRot',Args.DistEdges,...
                                                                          'HistDistEdgesRotScale',Args.HistDistEdgesRotScale,...
                                                                          'RangeX',Args.RangeX,...
                                                                          'RangeY',Args.RangeY,...
                                                                          'StepX',Args.StepX,...
                                                                          'StepY',Args.StepY,...
                                                                          'Flip',Args.Flip,...
                                                                          'SearchRadius',Args.SearchRadius,...
                                                                          'FilterSigma',Args.FilterSigma,...
                                                                          'ColNamesX',Args.CatColNamesX,...
                                                                          'ColNamesY',Args.CatColNamesY);

        % go over possible solutions:
        Result(Iobj).Nsolutions = numel(ResPattern.Sol.SN);   % number of candidate solutions
        Result(Iobj).ResPattern = ResPattern;

        if Result(Iobj).Nsolutions==0
            % no solution found
            Result(Iobj).WCS.Success = false;
            
        else

            % assume solutions are ordered by S/N
            Nsol = min(Args.MaxSol2Check, Result(Iobj).Nsolutions);
            for Isol=1:1:Nsol
                % Apply affine transformation to Reference
                % CreateNewObj=true, because FilteredProjAstCat is needed later on
                TransformedProjAstCat = FilteredProjAstCat.copy;
                % The TransformedProjAstCat is only used for matching
                TransformedProjAstCat = imProc.trans.tranAffine(TransformedProjAstCat, ResPattern.Sol.AffineTran{Isol}, true,...
                                                                'ColX',RefColNameX,...
                                                                'ColY',RefColNameY,...
                                                                'CreateNewObj',false);

                MatchInd = imProc.match.matchReturnIndices(FilteredCat, TransformedProjAstCat,...
                                                 'Radius',Args.SearchRadius.*mean(Args.Scale),...
                                                 'CooType','pix',...
                                                 'ColCatX',Args.CatColNamesX,...
                                                 'ColCatY',Args.CatColNamesY,...
                                                 'ColRefX',RefColNameX,...
                                                 'ColRefY',RefColNameY);
                                             
                MatchedCat = FilteredCat.copy;
                MatchedCat = selectRows(MatchedCat, MatchInd.Obj2_IndInObj1, 'CreateNewObj',false);
                                             
                % DEBUGING:
                % FilteredCat.plot({'X','Y'},'o')          
                % hold on
                % TransformedProjAstCat.plot({'X','Y'},'.')
                % MatchedCat.plot({'X','Y'},'o','MarkerSize',15)
                % axis([-600 600 -600 600])

                
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
                [Xcat,~,IndCatX] = getColDic(MatchedCat, Args.CatColNamesX);
                [Ycat,~,IndCatY] = getColDic(MatchedCat, Args.CatColNamesY);
                Xref = getColDic(FilteredProjAstCat, RefColNameX);
                Yref = getColDic(FilteredProjAstCat, RefColNameY);
                Mag  = getColDic(FilteredProjAstCat, Args.RefColNameMag);
                % fit the catalog to the reference and generate the Tran2D
                % object and all the information required for the WCS
                
                %'Flip',ResPattern.Sol.Flip,...
                [Tran, ParWCS, ResFit] = imProc.astrometry.fitWCS(Xcat, Ycat, Xref, Yref, Mag, RAdeg, Decdeg,...
                                                       'ImageCenterXY',Result(Iobj).ImageCenterXY,...
                                                       'Scale',ResPattern.Sol.Scale(Isol).*Args.Scale,...
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
               

                % DEBUG:
                %plot(Xorig, ResFit.Resid.*3600,'.')
                %plot(Yorig, ResFit.Resid.*3600,'.')
                %  scatter(Xorig, Yorig, 10, ResFit.Resid.*3600,'filled')
                %  colorbar

                % store transformations
                Result(Iobj).Tran(Isol)       = Tran;
                Result(Iobj).ResFit(Isol)     = ResFit;

            end
            
            % classify the quality of solutions
            %[Result(Iobj).Res.RMS]
            Result(Iobj).ErrorOnMean = [Result(Iobj).ResFit.AssymRMS_mag]./sqrt([Result(Iobj).ResFit.Ngood]);
            [~,Result(Iobj).BestInd] = min(Result(Iobj).ErrorOnMean);
            Ibest = Result(Iobj).BestInd;
            
            % Generate AstroWCS for best solution
            StructWCS = Result(Iobj).ParWCS(Ibest);
            KeyValWCS = namedargs2cell(StructWCS);
            Result(Iobj).WCS = AstroWCS.tran2wcs(Result(Iobj).Tran(Ibest), KeyValWCS{:});
            
            
            Result(Iobj).WCS.ResFit   = Result(Iobj).ResFit(Ibest);
            Result(Iobj).WCS          = populateSucess(Result(Iobj).WCS, 'TestNbin',Args.TestNbin,...
                                                                         'RegionalMaxMedianRMS',Args.RegionalMaxMedianRMS,...
                                                                         'RegionalMaxWithNoSrc',Args.RegionalMaxWithNoSrc,...
                                                                         'MaxErrorOnMean',Args.MaxErrorOnMean);

                        
            
                                                              
            % add RA/Dec to the catalog and update Header
            if nargout>1
                
                % update header with astrometric quality information
                if Args.UpdateHeader
                    Keys = {'AST_NSRC','AST_ARMS','AST_ERRM'};
                    Obj(Iobj).HeaderData.replaceVal(Keys,...
                                                    {Result(Iobj).ResFit(Ibest).Ngood,...
                                                     Result(Iobj).ResFit(Ibest).AssymRMS.*ARCSEC_DEG,...
                                                     Result(Iobj).ResFit(Ibest).ErrorOnMean.*ARCSEC_DEG},...
                                                    'Comment',{'Number of astrometric sources',...
                                                               'Astrometric assymptotic RMS [arcsec]',...
                                                               'Astrometric error on the mean [arcsec]'});
                end
                   
                
                % update RA/Dec in catalog
                [ObjSrcRA, ObjSrcDec] = Result(Iobj).WCS.xy2sky(Cat.getCol(IndCatX), Cat.getCol(IndCatY), 'OutUnits',Args.OutCatCooUnits);
                % insert or replace
                Cat = insertCol(Cat, [ObjSrcRA, ObjSrcDec], Args.OutCatColPos, {Args.OutCatColRA, Args.OutCatColDec}, {Args.OutCatCooUnits, Args.OutCatCooUnits});
                if ~isempty(Args.SortCat)
                    Cat = sortrows(Cat, Args.SortCat);
                end


                % update the Obj with the new CatData and new header:
                if isa(Obj, 'AstroImage')
                    Obj(Iobj).CatData = Cat;
                    
                    % update WCS in AstroImage
                    Obj(Iobj).WCS = Result(Iobj).WCS;
                    
                    % add WCS kesy to Header
                    %Obj(Iobj).HeaderData.Data = [];
                    %warning('header deleted!!')
                    Obj(Iobj).HeaderData = wcs2header(Obj(Iobj).WCS, Obj(Iobj).HeaderData);
                    % add RA/Dec corners to header
                    Obj(Iobj).HeaderData = addCornersCoo2header(Obj(Iobj).WCS, Obj(Iobj).HeaderData);
                        
                else
                    % assume Obj is AstroCatalog
                    Obj(Iobj) = Cat;
                end
            end
        end

    end
    
end