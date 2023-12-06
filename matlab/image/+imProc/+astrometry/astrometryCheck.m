function [Result, AstrometricCat]=astrometryCheck(Obj, Args)
    % Compare the astrometry of a catalog with a reference astrometric catalog.
    %       Return statistics regarding the matched sources, rms, rms as a
    %       function of position and mag.
    % Input  : - An AstroCatalog or AstroImage object (with AstroCatalog).
    %          * ...,key,val,...
    %            'WCS' - An AstroWCS object containing the WCS of the
    %                   catalog. If given, will override the AstroWCS on
    %                   the AstroImage. Default is [].
    %            'CatName' - Catalog name. Default is 'GAIAEDR3'.
    %                   If AstroCatalog, then will return the catalog as
    %                   is.
    %            'getAstrometricCatalogArgs' - A cell array of additional
    %                   arguments to pass to imProc.cat.getAstrometricCatalog
    %            'Radius' - Matching radius between the input catalog and
    %                   the astrometric catalog. Default is 5.
    %            'RadiusUnits' - Units for matching radius.
    %                   Default is 'arcsec'.
    %            'IncludeDistortions' - A logical indicating if to use the
    %                   WCS including the distortions (true), or not (false).
    %                   Default is true.
    %            'MaxNmtach' - When calculating the rms and median of the residuals,
    %                   do not use sources with number of matches larger than
    %                   this value. Default is 1.
    %            'Nbin' - When calculating the rms vs. X/Y position, this
    %                   is the number of bins in X and Y. Default is 3.
    %            'ColNamesX' - A cell array of dictionary names for X coordinates
    %                   in the input catalog. Default is AstroCatalog.DefNamesX
    %            'ColNamesY' - Like 'ColNamesX', but for the Y axis.
    %                   Default is AstroCatalog.DefNamesY
    %            'ColNamesMag' - Like 'ColNamesX', but for the magnitude
    %                   Default is AstroCatalog.DefNamesMag
    % Output : - A structure array with an element per input catalog, with
    %            the statistical information regarding the matching.
    %            The following fields are available:
    %            'Nsrc' - Numbre of sources.
    %            'NmatchedSrc' - Number of sources with Nmatch>0
    %            'VecDist' - Vector of matched distances [arcsec].
    %            'VecDeltaRA' - RA diff [arcsec].
    %            'VecDeltaDec' - Dec diff [arcsec].
    %            'VecNmatch' - Number of matched per source
    %            'RMS_RA' - rms RA [arcsec]
    %            'RMS_Dec' - rms Dec [arcsec]
    %            'RRMS_RA' - robust rms RA [arcsec]
    %            'RRMS_Dec' - robust rms Dec [arcsec]
    %            'MedDelta_RA' - median of RA diff [arcsec]
    %            'MedDelta_Dec' - median of Dec diff [arcsec]
    %            'BinN' - Matrix of number of matches in positional bin.
    %            'BinMean' - mean dist in positional bin.
    %            'BinMedian' - median dist in positional bin.
    %            'MagResid' - Output of imUtil.calib.resid_vs_mag
    % Author : Eran Ofek (Jul 2021)
    % Example: AstrometricCat = catsHTM.cone_search('GAIAEDR3',1,1,1000);
    %          % or load AstrometricCat_PTF_Cropped.mat
    %          R=imProc.astrometry.astrometryCheck(AstrometricCat,'CatName',AstrometricCat)
    
    arguments
        Obj 
        Args.WCS                              = [];
        Args.CatName                          = 'GAIAEDR3';  % or AstroCatalog
        Args.getAstrometricCatalogArgs cell   = {};
        
        Args.Radius                           = 5;
        Args.RadiusUnits                      = 'arcsec';
        
        Args.IncludeDistortions(1,1) logical  = true;
        
        Args.MaxNmtach                        = 1;  % if Inf use allmatched sources
        Args.Nbin                             = 3;  % if empty - skip this stage
        
        Args.ColNamesX                        = AstroCatalog.DefNamesX;
        Args.ColNamesY                        = AstroCatalog.DefNamesY;
        Args.ColNamesMag                      = AstroCatalog.DefNamesMag;
        
    end
    RAD        = 180./pi;
    ARCSEC_DEG = 3600;
    % column names added to Cat/MatchedCat
    ColNameRA     = 'RA';
    ColNameDec    = 'Dec';
    ColNameDist   = 'Dist';   % column name of added ang. dist [arcsec]
    ColNameNmatch = 'Nmatch';
    
    
    
    Nobj = numel(Obj);
    for Iobj=1:1:Nobj
        % for each image/catalog
        if isa(Obj, 'AstroImage')
            if ~isempty(Args.WCS)
                WCS = Args.WCS.copy();
            else
                WCS = Obj(Iobj).WCS.copy();
            end
            % create a modified version of Obj
            Cat = Obj(Iobj).CatData.copy();
        elseif isa(Obj, 'AstroCatalog')
            WCS = Args.WCS.copy();
            % create a modified version of Obj
            Cat = Obj(Iobj).copy();
        else
            error('First input argument must be AstroImage or AstroCatalog');
        end
        if isemptyCatalog(Cat)
            error('Catalog must contain sources - catalog %d is empry',Iobj);
        end
        
        if isempty(WCS)
            % attempt to read RA/Dec from catalog
            [SrcRA, SrcDec] = getLonLat(Cat, 'rad');
        else
            % Calculate RA/Dec for sources in catalog
            [Xcat, Ycat] = getXY(Cat);
            [SrcRA, SrcDec] = WCS.xy2sky(Xcat, Ycat, 'rad', Args.IncludeDistortions);   
            
            % add SrcRA, SrcDec to Cat
            insertCol(Cat, [SrcRA, SrcDec], Inf, {ColNameRA, ColNameDec}, {'rad', 'rad'});
        end
        
        % retrieve astrometric catalog
        if ~isa(Args.CatName,'AstroCatalog')
            [CenterCoo, BestRadius] = boundingCircle([SrcRA, SrcDec]); % [in/out: radians]
        else
            CenterCoo  = [NaN NaN];
            BestRadius = [NaN];
        end
        [AstrometricCat] = imProc.cat.getAstrometricCatalog(CenterCoo(1), CenterCoo(2), 'CatName',Args.CatName,...
                                                                     'Radius',BestRadius,...
                                                                     'RadiusUnits','rad',...
                                                                     'CooUnits','rad',...
                                                                     'OutUnits','rad',...
                                                                     Args.getAstrometricCatalogArgs{:});
        
        % Match AstrometricCat with SrcRA, SrcDec
        [MatchedAstCat] = imProc.match.matchOld(AstrometricCat, Cat, 'Radius',Args.Radius,...
                                                  'RadiusUnits',Args.RadiusUnits,...
                                                  'AddDistCol',true,...
                                                  'DistUnits','arcsec',...
                                                  'DistColName',ColNameDist,...
                                                  'AddNmatchCol',true,...
                                                  'NmatchColName',ColNameNmatch,...
                                                  'CooType','sphere',...
                                                  'AddIndInRef',false,...
                                                  'ColCatX',ColNameRA,...
                                                  'ColCatY',ColNameDec,...
                                                  'ColRefX',ColNameRA,...
                                                  'ColRefY',ColNameDec);
        % calc matches statistics
        DistVec     = abs(getCol(MatchedAstCat, ColNameDist));  % [arcsec]
        NmatchVec   = getCol(MatchedAstCat, ColNameNmatch);
        % assume all RA/Dec are in radians
        [AstRA, AstDec] = getLonLat(MatchedAstCat, 'rad');
        [CatRA, CatDec] = getLonLat(Cat, 'rad');
        
        DeltaDec    = CatDec - AstDec;
        DeltaRA     = (CatRA  - AstRA).*cos(AstDec);
        % convert rad to arcsec
        DeltaRA     = DeltaRA .*RAD.*ARCSEC_DEG;
        DeltaDec    = DeltaDec.*RAD.*ARCSEC_DEG;
        
        
        Result(Iobj).Nsrc        = MatchedAstCat.sizeCatalog;                   % number of sources in input catalog
        Result(Iobj).NmatchedSrc = sum(~isnan(MatchedAstCat.Catalog(:,1)));  % number of sucessfuly matched sources within search radius
        
        % [arcsec]
        Result(Iobj).VecDist     = getCol(MatchedAstCat, ColNameDist);
        Result(Iobj).VecDeltaRA  = DeltaRA;
        Result(Iobj).VecDeltaDec = DeltaDec;
        Result(Iobj).VecNmatch   = NmatchVec;
        
        
        % [arcsec]
        Flag = NmatchVec<=Args.MaxNmtach & NmatchVec>0;
        Result(Iobj).RMS_RA       = std(DeltaRA(Flag));
        Result(Iobj).RMS_Dec      = std(DeltaDec(Flag));
        Result(Iobj).RRMS_RA      = imUtil.background.rstd(DeltaRA(Flag));
        Result(Iobj).RRMS_Dec     = imUtil.background.rstd(DeltaDec(Flag));
        Result(Iobj).MedDelta_RA  = median(DeltaRA(Flag));
        Result(Iobj).MedDelta_Dec = median(DeltaDec(Flag));
        
        % calculate statistics as a function of position
        % we ant to do this in X/Y space!!
        if ~isempty(Args.Nbin)
            [X,~,IndX]     = getColDic(Cat, Args.ColNamesX);
            [Y,~,IndY]     = getColDic(Cat, Args.ColNamesY);
            if isempty(IndX) || isempty(IndY)
                error('X/Y coordinates are not available in input catalog object');
            end
            [Result(Iobj).BinN, Result(Iobj).BinMean, Result(Iobj).BinMedian] = tools.math.stat.bin2dFun(X, Y, DistVec, 'Nbin',Args.Nbin);
        end
        
        % calculate statistics as a function of magnitude
        Mag = getColDic(MatchedAstCat, Args.ColNamesMag);
        if all(isnan(Mag)) || isempty(Mag)
            Result(Iobj).MagResid = [];
        else
            [Flag, Result(Iobj).MagResid] = imUtil.calib.resid_vs_mag(Mag, DistVec);
        end
        
    end
    
end