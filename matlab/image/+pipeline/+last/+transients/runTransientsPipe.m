function [AD, ADc, MergedTranCat, Status] = runTransientsPipe(VisitData, Args)
    %{
    Performs the subtraction and transient search algorithms using 
    AstroDiff on images within a visit directory.
    Input   : - Path to visit directory holding sub-image coadds, or array 
                of AstroImage objects in memory.
              * ...,key,val,...
                'SaveProducts' - Bool on whether to save subtraction and 
                       transients products. Default is false.
                'SavePath' - Path to directory in which to save products in
                       case SaveProducts is true. If SavePath is not 
                       specified and VisitData is a path, then SavePath is 
                       set to VisitData. Default is ''.
                'RefPath' - Path to directory with reference images. If empty, 
                       constructs assuming reference directory is 
                       "/'machine_name'/data/references'. Default is ''.
                'Product' - Products to be saved per subtraction in case 
                       SaveProducts is true. Default is ''.
                'WriteHeader' - Array of bools indicating on whether to 
                       write a head for the products. Required by 
                       imProc.io.writeProduct and has to be the same length 
                       as Product. Default is ''.
                'SaveMergedCat' - Bool on whether to save all produced
                       transients catalogs as a single merged catalog.
                       Default is true.
                'AddMeta' - Bool on whether to add some meta data to the
                       transients catalog, for e.g. mount, camera, croID data. 
                       Default is true.
                'SameTelOnly' - Bool on whether to force to use the
                       exact same telescope (same mount) for reference
                       images. Default is true.
                'killDuplicates' - Bool on whether to remove duplicate
                       candidates in overlap regions between sub-images.
                       Only the candidates closest to the sub-image center
                       will be kept. Default is true.
                'MinimumNCoadd' - The minimum number of single images used
                       for the coadded New image. Default is 18.
                'MaximumCenterOffset' - The maximum offset between the New 
                       and Ref center coordinates. Default is 2.0.
                'MinimumOverlapFraction' - The minimum overlap between the 
                       New and Ref images as a fraction. Default is 0.5.
                'AsteroidSearchRad' - Radius around each transient 
                       candidate with which to search for asteroids in New
                       and Ref images. Given in arcsec. Default is 20.
                'AsteroidLimMag' - Limiting magnitude higher than which
                       asteroids are ignored. Default is 21.5.
                'CometSearchRad' - Radius around each transient 
                       candidate in which to search for comets in New
                       and Ref images. Given in arcsec. Default is 90.
                'GeoPos' - Geodetic position of the observer (on
                       Earth). [Lon (deg), Lat (deg), Height (m)].
                       If empty, then calculate geocentric
                       positions. Default is [35.05 30.04 415].
    Output  : - AstroDiff objects holding all products and results derived 
                by the algorithm.
              - AstroDiff cutouts around each single transients candidate 
                which passes the flagging criteria.
              - AstroCatalog of all found transients candidates.
              - Result message
    Author  : Ruslan Konno (Jun 2024)
    Example : VisitPath = '/path/to/visit/dir'
              [AD, ADc, MergedTranCat, Status] = pipeline.last.transients.runTransientsPipe(VisitPath)
    %}

    arguments
        VisitData

        Args.SaveProducts logical = false;
        Args.SavePath = '';
        Args.RefPath = '';
        Args.Product = '';
        Args.WriteHeader = '';
        Args.SaveMergedCat logical = true;
        Args.AddMeta logical = true;
        Args.SameTelOnly logical = false;
        Args.killDuplicates logical = true;

        Args.MinimumNCoadd = 18;
        Args.MaximumCenterOffset = 0.86;
        Args.MinimumOverlapFraction = 0.5;

        Args.AsteroidSearchRad = 10;
        Args.AsteroidLimMag = 21.5;
        Args.CometSearchRad = 90;
        Args.GeoPos = [35.05 30.04 415];

        Args.CropIDs = [];

        Args.FilterConfigFile = '';

        Args.PixScale = 1.25;
    end

    % 1: ----- Set default arguments -----

    % Set default return status.
    % The function should never be able to return the default status.     
    % If it does, there is an uncontrolled return.
    Status = 'Uncontrolled exit.';

    % Initialize empty output arguments
    AD = AstroZOGY();
    ADc = AstroZOGY();
    MergedTranCat = AstroCatalog();

    % Some unit conversion parameters
    Rad2Arcsec = 3600.*180./pi; %206265;
    Arcsec2Rad = 1./Rad2Arcsec; %4.84814e-6;

    % 2: ----- Set and verify paths -----
    
    % If Args.SaveProducts is true, check if Args.SavePath is given.
    % If Args.SavePath is not given and VisitData is a char/string, set
    % Args.SavePath to VisitData. Return if Args.SavePath is not a directory.
    if Args.SaveProducts && isempty(Args.SavePath)
        if isa(VisitData, 'char') || isa(VisitData, 'string')
            Args.SavePath = VisitData;
        else
            Status = 'SaveProducts is true but SavePath is not set, exiting.';
            return
        end

        if ~isfolder(Args.SavePath)
            Status = 'SavePath directory not found, exiting.';
            return
        end
    end

    % Get path of reference images and check if it exists, return if not.
    if isempty(Args.RefPath)
        Computer = tools.os.get_computer;
        RefPath = strcat('/',Computer,'/data/references');
    else
        RefPath = Args.RefPath;
    end

    if ~isfolder(RefPath)
        Status = 'RefPath directory not found, exiting.';
        return
    end
   
    % Find New image coadds and load
    if isa(VisitData, 'char') || isa(VisitData, 'string')

        if ~isfolder(VisitData)
            Status = 'VisitData directory not found, exiting.';
            return
        end

    % 3: ----- Load and verify New images -----

        Coadds = strcat(VisitData,'/LAST*coadd_Image_1.fits');
        New = AstroImage.readFileNamesObj(Coadds, 'Path', VisitData);
    elseif isa(VisitData, 'AstroImage')
        New = VisitData;
    end
   
    % Check for empty images, return if all images are empty.
    NonEmptyNew = ~New.isemptyImage;

    if ~any(NonEmptyNew)
      Status = 'All New images are empty.';
      return
    end

    % Only use non-empty images.
    New = New(NonEmptyNew);
    Nobj = numel(New);
   
    % 4: ----- Load and verify Ref images -----
    
    % Find reference image for each New image

    % Preload refs

    % Get name of New image and search for Ref image via wildcards
    FN = FileNames.generateFromFileName(New(1).ImageData.FileName);
    FNref = FN.copy();

    % Convert telescope designation to wildcard if Refs from other
    % telescopes are allowed.
    if ~Args.SameTelOnly
        FNref.ProjName={replaceBetween(FNref.ProjName{1},"LAST.01.",".0","*")};
    end

    % Wildcard time and crop ID.
    FNref.Time = {'*.*.*'};
    FNref.CropID = '*';

    % Use only the LAST field ID for Ref search. If New image
    % observation was of an Object with a dot extsion, the dot
    % extension is removed for Ref search.
    FieldID = split(FNref.FieldID{1},'.');
    FieldID = FieldID{1};
    
    % Construct Ref filename
    FieldRefPath = strcat(RefPath, '/', FieldID);
    FNref.FieldID{1} = FieldID;
    RefFile = fullfile(FieldRefPath,FNref.genFile);
    RefFile{1} = replace(RefFile{1},'_coadd_','_*_');

    % Load Ref image as AstroImage and Ref image FileName object
    Refs = AstroImage.readFileNamesObj(RefFile{1}, 'Path', FieldRefPath);
    NumRefs = numel(Refs);

    if NumRefs < 1
        Status = 'No reference images found.';
        return;        
    end

    % Track number of matched reference images
    NRefsMatched = 0;

    % Track number of images failing Args.MinimumNCoadd criterium.
    NBelowMinNCoadd = 0;

    % Tack number of images with no overlap to any reference image
    NoOverlap = 0;

    for Iobj=Nobj:-1:1

        % Check if New image meets NCoadd criterium. If it does not,
        % remember and continue.
        NCOADD = New(Iobj).HeaderData.getVal('NCOADD');

        if NCOADD < Args.MinimumNCoadd
            NBelowMinNCoadd = NBelowMinNCoadd + 1;
            continue
        end

        if ~isempty(Args.CropIDs)
            if ~ismember(New(Iobj).HeaderData.getVal('CROPID'), Args.CropIDs)
                continue
            end
        end

        % Match Ref by finding the closest one
        NewRADec = New(Iobj).WCS.CRVAL;
        CRValDist = nan(NumRefs,1);
        for IRef = 1:NumRefs
            RefRADec = Refs(IRef).WCS.CRVAL;
            CRValDist(IRef) = rad2deg(celestial.coo.sphere_dist(...
                RefRADec(1), RefRADec(2), NewRADec(1), NewRADec(2), 'deg'));
        end

        MinCRValDist = min(CRValDist);

        % Verify there is some overlap between New and Ref
        if MinCRValDist > Args.MaximumCenterOffset
            NoOverlap = NoOverlap +1;
            continue
        end

        Ref = Refs(CRValDist == MinCRValDist);

        FNrref = FileNames.generateFromFileName(Ref.ImageData.FileName);

        % Make sure Ref products are complete, continue if not.
        if isempty(Ref.PSF) || isempty(Ref.Mask)
            warning('Missing reference products.');
            continue
        end

        % Generate New and Ref filenames properly
        FN = FileNames.generateFromFileName(New(Iobj).ImageData.FileName);
        NewName = FN.genFile;
        RefName = FNrref.genFile;
        
        % Check if the New image is the Ref image, continue if they are.
        if convertCharsToStrings(NewName{1}) == convertCharsToStrings(RefName{1})
            warning('New image is reference image.');
            continue
        end

        % Reference image found, remember this.
        NRefsMatched = NRefsMatched + 1;

        % Create AstroDiff (AstroZOGY)
        AD(Iobj) = AstroZOGY(New(Iobj), Ref);
        % Remember in AD if Ref image is already background subtracted.
        if FNrref.Level{1} == "ref"
            AD(Iobj).RefIsBackgroundSubtracted = true;
        else
            AD(Iobj).RefIsBackgroundSubtracted = false;
        end
    end

    % clear for memory
    clear Refs;

    % 5: ----- Verify subtraction requirements -----

    % If no New images pass the NCoadd criterium, return.
    if NBelowMinNCoadd == Nobj
        Status = 'All new images below required amount of NCOADD.';
        return;
    end
    
    % If no Ref images found, return
    if NRefsMatched < 1
        Status = 'No reference images matched.';
        return;
    end
   
    % If all New and Ref images have no overlap, return.
    if NoOverlap == Nobj
        Status = 'All New and Ref images have no overlap.';
        return;
    end
    
    % Remove empty AstroDiff objects and remember number of AstroDiffs
    % Return if all are empty.
    NonEmptyCell = any(~cellfun('isempty',{AD(:).New}), 1);
    if ~any(NonEmptyCell)
        Status = 'All AstroDiffs are empty.';
        return;
    end
    
    % Use only non-empty AstroDiff objects
    AD = AD(:, NonEmptyCell);
    Nobj = numel(AD);
    
    % Register New and Ref
    AD.register;

    % Check if the New and Ref images are overlapping up to the 
    % requried amount after registration
    % This is done assuming AD.register marks no overlap regions as NaN in
    % the bit mask of Ref
    NotEnoughOverlap = 0;
    for Iobj = Nobj:-1:1
        % Get fraction of NaN pixels in Ref Image and compare to total
        % number of pixels
        NaNs = sum(AD(Iobj).Ref.MaskData.findBit('NaN'), 'all');
        [ImageSizeX, ImageSizeY] = AD(Iobj).Ref.ImageData.sizeImage;
        TotalNumPixels = ImageSizeX*ImageSizeY;
        FractionNotNaNs = (1-NaNs / TotalNumPixels);
        % If fraction of not-NaNs is below minumum overlap fraction,
        % remember and remove AstroDiff object.
        if FractionNotNaNs < Args.MinimumOverlapFraction
            NotEnoughOverlap = NotEnoughOverlap +1;
            AD(Iobj) = [];
        end
    end

    % If all overlaps are less than half, return.
    if NotEnoughOverlap == Nobj
        Status = 'All New and Ref images overlap for less than half of the field.';
        return;
    end

    % Remember new number of AstroDiffs
    Nobj = numel(AD);

    % 6: ----- Fill New and Ref estimates -----

    % Estimate backround and variance of New and Ref
    AD.estimateBackVar;
    % Estimate zero points
    AD.estimateFnFr;

    % 7: ----- Produce subtraction images -----
    
    % Create proper subtraction image D
    AD.subtractionD;
    % Derive Gabor stat image
    AD.matchfilterGabor;
    % Derive S stat image
    AD.subtractionS;
    % Derive Scorr stat image
    AD.subtractionScorr;
    % Derive Z2 stat image
    AD.translient;

    % 8: ----- Find and process transients -----
    
    % Find transients
    AD.findTransients;
    % Catalog match

    % Merged cat
    % TODO: Make some decision on merged cat matching. Right now it is
    % commented out as it takes ~20s per visit.
    
    %imProc.match.match_catsHTMmerged(AD);
    %imProc.match.match_catsHTM(AD,'MergedCat',...
    %    'ColDistName','MergedDist','ColNmatchName','MergedMatches');
   
    % Galaxy match
    imProc.match.match2Galaxies(AD);

    % Star match
    % Star matching is not that trivial since it is done using the GAIADR3 
    % catalog, which is a large catalog and bright stars can be ~100s of 
    % arcsec large in LAST images. We have to add some steps to make this 
    % process fast enough. Some of the steps are in
    % imProc.match.match2Stars.
    
    % We will cut down the GAIA catalog to the full visit image

    % Get the center coordinates of the visit image
    for Iobj=Nobj:-1:1
        C_RA(Iobj) = convert.angular('deg','rad',AD(Iobj).HeaderData.getVal('RA'));
        C_Dec(Iobj) = convert.angular('deg','rad',AD(Iobj).HeaderData.getVal('Dec'));        
    end

    C_RA_med = median(C_RA);
    C_Dec_med = median(C_Dec);

    % Get the distance from the visit center to the farthest sub-image and 
    % add the width of a sub-image to cover all sub-images

    SubImageWidth = AD(1).sizeImage*Args.PixScale*Arcsec2Rad;
    MaxDistRad = max(celestial.coo.sphere_dist(...
        C_RA, C_Dec, C_RA_med, C_Dec_med, 'rad'), [], 'all');
    MaxDistRad = MaxDistRad + SubImageWidth;

    % Use the visit center coordinates and the distance to the furthest
    % sub-image to cone search the GAIA catalog and keep only the matched
    % sources
    StarCat = catsHTM.cone_search('GAIADR3', C_RA_med, C_Dec_med, ...
        MaxDistRad, 'RadiusUnits', 'rad', 'OutType','AstroCatalog');
    StarCat.sortrows('Dec');

    % Search for star matches on cutdown catalog
    imProc.match.match2Stars(AD, StarCat);
    % Clear catalog for memory
    clear StarCat;

    % MP match

    if numel(Args.GeoPos) == 3
        Args.GeoPos(1:2) = Args.GeoPos(1:2)*pi/180;
    end

    % Get asteroid catalogs for New and Ref
    INPOP = celestial.INPOP;
    INPOP.populateAll;
    OrbElMerge= celestial.OrbitalEl.loadSolarSystem('merge');

    % Propogate catalog to New image epoch
    NewJulDay = median(arrayfun(@(x) x.New.julday,AD));

    [AstCatNew] = OrbElMerge.searchMinorPlanetsNearPosition(...
        NewJulDay, C_RA_med, C_Dec_med, MaxDistRad,...
        'INPOP', INPOP, 'CooUnits','rad', 'SearchRadiusUnits','rad',...
        'QuickSearchBuffer', 500,'MagLimit', Args.AsteroidLimMag,...
        'RefEllipsoid','WGS84', 'GeoPos', Args.GeoPos,...
        'OutUnitsDeg',true,'Integration', true);

    % Propogate catalog to Ref image epoch
    RefJulDay = median(arrayfun(@(x) x.Ref.julday,AD));

    [AstCatRef] = OrbElMerge.searchMinorPlanetsNearPosition(...
        RefJulDay, C_RA_med, C_Dec_med, MaxDistRad,...
        'INPOP', INPOP, 'CooUnits','rad', 'SearchRadiusUnits','rad',...
        'QuickSearchBuffer', 500,'MagLimit', Args.AsteroidLimMag,...
        'RefEllipsoid','WGS84', 'GeoPos', Args.GeoPos,...
        'OutUnitsDeg',true,'Integration', true);
    
    % Split the catalogs in New and Ref sources (positive and 
    % negative transients) so asteroids at New julday will not be
    % assocaited to negative sources, and asteroids at Ref julday will not
    % be associated to positive sources.    
    for Iobj=Nobj:-1:1

        Scores = AD(Iobj).CatData.getCol('SCORE');
        NumRows = numel(Scores);
        NewSrcsIndx = Scores>0;
        RefSrcsIndx = Scores<0;
        NewSrcs = AD(Iobj).CatData.selectRows(NewSrcsIndx);
        RefSrcs = AD(Iobj).CatData.selectRows(RefSrcsIndx);

        % Match MP in New
        [~,~,NewSrcs] = imProc.match.match2solarSystem(NewSrcs, 'InCooUnits', 'deg', ...
                        'SourcesColDistName', 'N_DistMP', 'AstCat', AstCatNew,...
                        'JD', NewJulDay, 'AddMag2Obj', true, ...
                        'ColMag', 'Mag', 'ObjColMag', 'N_MagMP',...
                        'SearchRadius', Args.AsteroidSearchRad, ...
                        'GeoPos', Args.GeoPos);

        % Match MP in Ref
        [~,~,RefSrcs] = imProc.match.match2solarSystem(RefSrcs, 'InCooUnits', 'deg', ...
                        'SourcesColDistName', 'R_DistMP', 'AstCat', AstCatRef,...
                        'JD', RefJulDay, 'AddMag2Obj', true, ...
                        'ColMag', 'Mag', 'ObjColMag', 'R_MagMP',...
                        'SearchRadius', Args.AsteroidSearchRad, ...
                        'GeoPos',Args.GeoPos);

        N_DistMP = nan(NumRows,1);
        if NewSrcs.isColumn('N_DistMP')
            N_DistMP(NewSrcsIndx) = NewSrcs.getCol('N_DistMP');
        end

        N_MagMP = nan(NumRows,1);
        if NewSrcs.isColumn('N_MagMP')
            N_MagMP(NewSrcsIndx) = NewSrcs.getCol('N_MagMP');
        end 

        R_DistMP = nan(NumRows,1);
        if RefSrcs.isColumn('R_DistMP')
            R_DistMP(RefSrcsIndx) = RefSrcs.getCol('R_DistMP');
        end
        
        R_MagMP = nan(NumRows,1);
        if RefSrcs.isColumn('R_MagMP')
            R_MagMP(RefSrcsIndx) = RefSrcs.getCol('R_MagMP');
        end

        AD(Iobj).CatData.insertCol(...
                cell2mat({cast(N_DistMP,'double'), cast(N_MagMP,'double'),...
                cast(R_DistMP,'double'), cast(R_MagMP,'double')}),...
                inf,...
                {'N_DistMP','N_MagMP','R_DistMP','R_MagMP'}, ...
                {'arcsec','mag','arcsec','mag'}...
            );

    end

    % Clear for memory
    clear AstCatNew;
    clear AstCatRef;
    clear INPOP;
    clear OrbElMerge;

    % Comet matching
    
    OrbElComet= celestial.OrbitalEl.loadSolarSystem('comet');

    % Match Comet in New

    [ComCatNew] = OrbElComet.searchMinorPlanetsNearPosition(...
        NewJulDay, C_RA_med, C_Dec_med, MaxDistRad,...
        'CooUnits','rad', 'SearchRadiusUnits','rad',...
        'OutUnitsDeg', true, 'Integration', false, ...
        'GeoPos', Args.GeoPos);

    % If comets within FoV, match to candidates
    if size(ComCatNew.Catalog,1) > 0

        ComCatNew.sortrows('Dec');
        [CometLon, CometLat] = ComCatNew.getLonLat('rad');

        % Loop over AstroDiffs
        for Iobj=1:1:Nobj

            Scores = AD(Iobj).CatData.getCol('SCORE');
            NewSrcsIndx = Scores>0;
            NewSrcs = AD(Iobj).CatData.selectRows(NewSrcsIndx);
            
            % Match all transients candidates to comets at New image epoch
            [RA, Dec] = NewSrcs.getLonLat('rad');
            ComMatches = VO.search.search_sortedlat_multi( ...
                [CometLon, CometLat], RA, Dec, ...
                -Args.CometSearchRad*Arcsec2Rad);
            ComMatchsInd = find(vertcat(ComMatches.Nmatch) > 0);
            NComMatches = numel(ComMatchsInd);

            % If no matches, continue.
            if NComMatches < 1
                continue
            end

            % If matched, get distance and magnitude.
            MPDist_new = AD(Iobj).CatData.getCol('N_DistMP');
            MPMag_new = AD(Iobj).CatData.getCol('N_MagMP');

            MPDist_newOnly = MPDist_new(NewSrcsIndx);
            MPMag_newOnly = MPMag_new(NewSrcsIndx);

            % For each candidate, get closest matching asteroid/comet and
            % save distance and magnitude
            for IComMatches = 1:1:NComMatches
                IComMatchInd = ComMatchsInd(IComMatches);
                OldDist = MPDist_newOnly(IComMatchInd);
                NewDist = min(ComMatches(IComMatchInd).Dist)*Rad2Arcsec;
                if isnan(OldDist) || (NewDist < OldDist)
                    MPDist_newOnly(IComMatchInd) = NewDist;
                    Ind1 = ComMatches(IComMatchInd).Ind1;
                    ComMags = ComCatNew.getCol('Mag');
                    MPMag_newOnly(IComMatchInd) = ComMags(Ind1);
                end
            end

            MPDist_new(NewSrcsIndx) = MPDist_newOnly;
            MPMag_new(NewSrcsIndx) = MPMag_newOnly;

            % Update minor planet columns
            AD(Iobj).CatData.replaceCol(MPDist_new,'N_DistMP');
            AD(Iobj).CatData.replaceCol(MPMag_new,'N_MagMP');
        end

    end

    %Clear for memory
    clear ComCatNew;

    % Match Comet in Ref

    [ComCatRef] = OrbElComet.searchMinorPlanetsNearPosition(...
        RefJulDay, C_RA_med, C_Dec_med, MaxDistRad,...
        'CooUnits','rad', 'SearchRadiusUnits','rad',...
        'OutUnitsDeg',true,'Integration', false, ...
        'GeoPos', Args.GeoPos);

    % If comets within FoV, match to candidates
    if size(ComCatRef.Catalog,1) > 0

        ComCatRef.sortrows('Dec');
        [CometLon, CometLat] = ComCatRef.getLonLat('rad');

        % Loop over AstroDiffs
        for Iobj=1:1:Nobj

            Scores = AD(Iobj).CatData.getCol('SCORE');
            RefSrcsIndx = Scores<0;
            RefSrcs = AD(Iobj).CatData.selectRows(RefSrcsIndx);

            % Match all transients candidates to comets at Ref image epoch
            [RA, Dec] = RefSrcs.getLonLat('rad');
            ComMatches = VO.search.search_sortedlat_multi( ...
                [CometLon, CometLat], RA, Dec, ...
                -Args.CometSearchRad*Arcsec2Rad);
            ComMatchsInd = find(vertcat(ComMatches.Nmatch) > 0);
            NComMatches = numel(ComMatchsInd);

            % If no matches, continue.
            if NComMatches < 1
                continue
            end

            % If matched, get distance and magnitude.
            MPDist_ref = AD(Iobj).CatData.getCol('R_DistMP');
            MPMag_ref = AD(Iobj).CatData.getCol('R_MagMP');

            MPDist_refOnly = MPDist_ref(RefSrcsIndx);
            MPMag_refOnly = MPMag_ref(RefSrcsIndx);
            % For each candidate, get closest matching asteroid/comet and
            % save distance and magnitude
            for IComMatches = 1:1:NComMatches
                IComMatchInd = ComMatchsInd(IComMatches);
                OldDist = MPDist_refOnly(IComMatchInd);
                NewDist = min(ComMatches(IComMatchInd).Dist)*Rad2Arcsec;
                if isnan(OldDist) || (NewDist < OldDist)
                    MPDist_refOnly(IComMatchInd) = NewDist;
                    Ind1 = ComMatches(IComMatchInd).Ind1;
                    ComMags = ComCatRef.getCol('Mag');
                    MPMag_refOnly(IComMatchInd) = ComMags(Ind1);
                end
            end

            MPDist_ref(RefSrcsIndx) = MPDist_refOnly;
            MPMag_ref(RefSrcsIndx) = MPMag_refOnly;

            % Update minor planet columns
            AD(Iobj).CatData.replaceCol(MPDist_ref,'R_DistMP');
            AD(Iobj).CatData.replaceCol(MPMag_ref,'R_MagMP');
        end

    end    

    %Clear for memory
    clear ComCatRef;
    
    % Measure transients
    AD.measureTransients;

    % Flag non transients
    AD.flagNonTransients('ConfigFile', Args.FilterConfigFile);

    % If AddMeta true, add meta information to catalog
    if Args.AddMeta
        for Iobj=1:1:Nobj
            
            % Get header
            Header = AD(Iobj).HeaderData;
            % Number of candidates for array length
            NumTran = size(AD(Iobj).CatData.Catalog,1);

            OnesArray = ones(NumTran,1);

            % Mount, Camera, CropID
            Mount = Header.getVal('MOUNTNUM')*OnesArray;
            Cam = Header.getVal('CAMNUM')*OnesArray;
            CropID = Header.getVal('CROPID')*OnesArray;

            % Object (i.e. target)
            % This will usually be a LAST field ID but it can have a dot
            % extension e.g. 1234.ToOTarget. Because only doubles are
            % allowed in the catalog, we're saving the LAST field ID only,
            % i.e. we're removing the dot extension '.ToOTarget' if it
            % exists.

            Object = Header.getVal('OBJECT');
            if ~isnumeric(Object)
                Object = split(Header.getVal('OBJECT'),'.');
                Object = str2double(Object{1});
            end
            Object = Object*OnesArray;

            % FWHM, LIMMAG, PH_COL1, EXPTIME, ZP_new, ZP_ref, ZP_d
            FWHM_new = AD(Iobj).New.PSFData.fwhm*OnesArray;
            FWHM_ref = AD(Iobj).Ref.PSFData.fwhm*OnesArray;
            LIMMAG_new = AD(Iobj).New.HeaderData.getVal('LIMMAG')*OnesArray;
            LIMMAG_ref = AD(Iobj).Ref.HeaderData.getVal('LIMMAG')*OnesArray;
            LIMMAG_D = AD(Iobj).HeaderData.getVal('LIMMAG')*OnesArray;
            PH_COL1_new = AD(Iobj).New.HeaderData.getVal('PH_COL1')*OnesArray;
            PH_COL1_ref = AD(Iobj).Ref.HeaderData.getVal('PH_COL1')*OnesArray;            
            Exposure_new = AD(Iobj).New.HeaderData.getVal('EXPTIME')*OnesArray;
            Exposure_ref = AD(Iobj).Ref.HeaderData.getVal('EXPTIME')*OnesArray;
            ZP_new = AD(Iobj).ZpN*OnesArray;
            ZP_ref = AD(Iobj).ZpR*OnesArray;
            ZP_D = AD(Iobj).ZpD*OnesArray;
    
            AD(Iobj).CatData.insertCol(...
                cell2mat({cast(Mount,'double'), cast(Cam,'double'), cast(CropID,'double'), ...
                cast(Object,'double'),...
                cast(FWHM_new,'double'), cast(FWHM_ref,'double'), cast(LIMMAG_new,'double'),...
                cast(LIMMAG_ref,'double'),cast(LIMMAG_D,'double'),cast(ZP_D,'double'),cast(ZP_new,'double'),...
                cast(ZP_ref,'double'),cast(PH_COL1_new,'double'),cast(PH_COL1_ref,'double'), ...
                cast(Exposure_new,'double'),cast(Exposure_ref,'double')}), ...
                'SCORE',...
                {'MOUNT','CAM','CROPID','OBJECT','N_FWHM','R_FWHM','N_LIMMAG',...
                'R_LIMMAG','LIMMAG','ZP','N_ZP','R_ZP', 'N_PH_COL1', 'R_PH_COL1', ...
                'N_EXPTIME','R_EXPTIME'}, ...
                {'','','','','','','mag','mag','mag','','','','','','s','s'});
        end
    end
    
    % 9: ----- Create output products -----

    % Create a merged catalog, holding all candidates in the individual AD
    % catalogs. Generally this will be a visit catalog when used in the
    % pipeline.
    for Iobj=Nobj:-1:1
        TranCat(Iobj) = AD(Iobj).CatData;
    end
    MergedTranCat = merge(TranCat);
    MergedTranCat.sortrows('Dec');
    
    % Get cutouts only for transients
    ADn = removeNonTransients(AD);
    % Make cutouts
    ADc = ADn.cutoutTransients;

    % Clear for memory
    clear ADn;
    
    % Get number of cutouts, i.e. positive candidates
    NADc = numel(ADc);

    % Make sure there are actually positive candidates
    if NADc == 1 && isempty(ADc(1).Table)
        NADc = 0;
    end
    
    % Kill duplicates
    % Candidates (real and not) in overlap areas between sub-images will 
    % appear multiple times, i.e. we will have duplicates. Here we clean
    % them. We find the duplicates by matching candidates within 1.5 arcsec
    % and keep only those closest to the center of its sub-image
    if Args.killDuplicates

        % Remember the number of positive candidates before removing
        % duplicates
        NADcWithDups = sum(MergedTranCat.getCol('FLAGS_TRANSIENT') == 0);
        
        % Clean merged catalog
        % Match all candidates within 1.5 arcsec
        [MRA, MDec] = MergedTranCat.getLonLat('rad');
        HalfSize = size(AD(1).Image)./2;
        SelfMatches = VO.search.search_sortedlat_multi( ...
                [MRA, MDec], MRA, MDec, -1.5*Arcsec2Rad);
        SelfMachthesN = vertcat(SelfMatches.Nmatch);
        % Count all candidates with more than one match as duplicates
        Duplicates = SelfMachthesN > 1;
        DuplicatesMatches = SelfMatches(Duplicates);
        DuplicatesNMatches = vertcat(DuplicatesMatches.Nmatch);
        % Get number of duplicates
        NDup = numel(DuplicatesNMatches);

        % Loop over duplicates
        for IDup=1:NDup

            % Get duplicate entry
            IDuplicates = DuplicatesMatches(IDup);
            IDuplicatesInd = IDuplicates.Ind;
            % Remember which duplicate entry is the current candidate
            SelfIdx = IDuplicatesInd == IDuplicates.Ind1;

            % Get catalog values for current duplicates            
            DuplicatesCat = MergedTranCat.selectRows(IDuplicatesInd);
            
            % Remove false duplicates, i.e. near candidates
            % in the same sub-image
            CropIDs = DuplicatesCat.Table.CROPID;
            SelfCrop = CropIDs(SelfIdx);
            % Get flag of all duplicates that are not in the same crop-id
            % as the current candidate plus the current candidate, i.e. the
            % current candidate is the only duplicate with its crop-id
            NonSelfImgDup = ((CropIDs ~= SelfCrop) | SelfIdx);

            % Update duplicates catalog
            DuplicatesCat = DuplicatesCat.selectRows(NonSelfImgDup);
            IDuplicatesInd = IDuplicatesInd(NonSelfImgDup);
            
            % If the candidate is the only one left, then it is not a
            % duplicate. Mark it as not a duplicate and continue.
            if DuplicatesCat.sizeCatalog == 1
               Duplicates(DuplicatesMatches(IDup).Ind1) = 0;
               continue
            end
            
            % Choose the duplicate that is closest to the center as the
            % survivor. Mark it as not a duplicate, leave all others
            % marked as duplicates.
            [DupX, DupY] = DuplicatesCat.getXY('ColX','XPEAK','ColY','YPEAK');
            CenterDistance = sqrt((DupX-HalfSize(1)).^2+(DupY-HalfSize(2)).^2);
            Survivor = CenterDistance == min(CenterDistance);
            Duplicates(IDuplicatesInd(Survivor)) = 0;
        end
        % Update the merged catalog by keeping only the candidates not
        % marked as duplicates.
        MergedTranCat = MergedTranCat.selectRows(~Duplicates);
        MergedTranCat.sortrows('Dec');

        % Clean ADc if necessary
        % Check the number of positive candidates after duplicate removal
        NADcWithoutDups = sum(MergedTranCat.getCol('FLAGS_TRANSIENT') == 0);
        
        % If the new number of positive candidates is lower than before
        % duplicate removal, we need to kill some cutout objects.
        if NADcWithDups > NADcWithoutDups
            % Get a catalog holding on the positive candidates
            PassingTranCat = MergedTranCat.selectRows(...
                MergedTranCat.getCol('FLAGS_TRANSIENT') == 0);
            % Get the XY and RADec values of positive candidates
            [MergedX, MergedY] = PassingTranCat.getXY('ColX','XPEAK','ColY','YPEAK');
            [MergedRA, MergedDec] = PassingTranCat.getLonLat('rad');
            % Keep memory of cutouts that survive
            NotKilled = ones(NADcWithDups,1);
            % Loop over cutouts
            for IADc = 1:NADcWithDups
                TC = ADc(IADc).CatData;

                % Get the XY and RADec of cutout candidate
                [ADcX, ADcY] = TC.getXY('ColX','XPEAK','ColY','YPEAK');
                [ADcRA, ADcDec] = TC.getLonLat('rad');

                % If the XY and RADec of the cutout matches exactly any of
                % the candidates in the catalog, then the cutout candidate
                % survived. Otherwise it was killed.
                NotKilled(IADc) = any(...
                    ismember(MergedX, ADcX) & ismember(MergedY, ADcY) &...
                    ismember(MergedRA, ADcRA) & ismember(MergedDec, ADcDec));
            end
            
            % Remove all killed candidates from the cutout array.
            ADc(~NotKilled) = [];
            % Update number of cutouts.
            NADc = numel(ADc);
        end

    end
    
    % 10: ----- Save to-disk products -----
    
    % If SaveProducts true, save desired products in desired path
    if Args.SaveProducts
        % Save individual image products if any specified
        if ~isempty(Args.Product)
            for Iobj=Nobj:-1:1
                FN = FileNames.generateFromFileName(AD(Iobj).New.ImageData.FileName);
                % Set AD name
                FNad = FN.copy();
                FNad.Level = {'coadd.zogyD'};
                FNad.FullPath = Args.SavePath;
                AD(Iobj).ImageData.FileName = FNad.genFull{1};
                
                [~,~,~]=imProc.io.writeProduct(AD(Iobj), FNad, ...
                'Level', 'coadd.zogyD', 'Product', Args.Product,...
                'WriteHeader',Args.WriteHeader,'Overwrite', true);
            end                
        end
        
        % Save merged catalog
        if Args.SaveMergedCat
            FN = FileNames.generateFromFileName(AD(1).New.ImageData.FileName);
            FN_merged = FN.copy();
            FN_merged.Level = {'coadd.zogyD'};
            FN_merged.CropID = 0;
            FN_merged.Product = {'Cat'};
            FN_merged.FullPath = Args.SavePath;
            
            [~,~,~]=imProc.io.writeProduct(MergedTranCat, FN_merged, ...
            'Level', 'coadd.zogyD', 'Product', {'Cat'},...
            'WriteHeader',false,'Overwrite', true, 'GetHeaderJD', false, ...
            'CropID_FromIndex',false);
        end
    end

    % TODO: cutouts can be very large (~GB), don't save them yet, we can 
    % try again when we can make them smaller, 
    % likely this will need a new inherited slimmed-down class
    %{
    if Args.SaveProducts
        FN = FileNames.generateFromFileName(AD(1).New.ImageData.FileName);
        % Set AD name
        FNtran = FN.copy();
        FNtran.Level = {'coadd.zogyD'};
        FNtran.Product = {'TransientsCat'};
        FNtran.CropID = 0;
        FNtran.FileType = {'mat'};
        FNtran.FullPath = Args.SavePath;
        TranCatFileName = FNtran.genFull{1};
        
        save(TranCatFileName,"ADc","-v7.3");
    end  
    %}

    % Update Status and finish
    StatusCell = strcat('Succesful exit,',{' '}, ...
        num2str(NADc),{' '},'transient(s) found.');
    
    Status = StatusCell{1};
end
