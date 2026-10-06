function [AD, ADc, TCL1, TCL2, Status] = pipelineII(VisitData, Args)
    %{
    Performs the subtraction and transient search algorithms using 
    AstroDiff on images within a visit directory.
    Input   : - Path to visit directory holding sub-image coadds, or array 
                of AstroImage objects in memory.
              * ...,key,val,...
                'RefPath' - Path to directory with reference images. If empty, 
                       constructs assuming reference directory is 
                       "/'machine_name'/data/references". Default is ''.
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
                'SubselectionFalse' - Cell of filter names. All candidates
                       with TranCatLevel1 that failed these filters will be
                       removed before multi-epoch matching.
                       Default is {'BadPixelHard', 'StarMatch', 'LIMMAG', 
                       'MPMatch', 'Negative'}.
                'RefCatName' - catsHTM Gaia catalog for all Gaia queries
                       (photometric ZP, smear template, star match), used
                       only when the New images carry no usable AST_CAT
                       keyword; otherwise the catalog named there (the one
                       pipelineI's astrometry used) is taken. Default is
                       'GAIADR3'.
                'GaiaCone' - The raw Gaia cones kept by pipelineI's
                       astrometry (its GaiaCone output). The photometric
                       ZP and the smear-template star cut take their Gaia
                       sources from the cone covering their search circle
                       instead of searching again; where none covers it,
                       or if empty, they search as before (issue #1348).
                       The star match keeps its own visit-wide search (its
                       bright-star halo margin reaches beyond the cones).
                       Default is [].
                'DumpComplexPath' - Directory in which AstroZOGY.subtractionD
                       saves the inputs of a sub image whose D or Pd came out
                       complex (issue #1360). If empty, no check. Default is ''.
                'GaiaProperMotion' - Apply the Gaia proper motion to the
                       epoch of each image for the photometric ZP (New and
                       Ref, each at its own JD), the smear-template star
                       cut and the star match (issue #1348).
                       Default is true.
    Output  : - Result message
              - AstroDiff objects holding all products and results derived 
                by the algorithm.
              - AstroDiff cutouts around each single transients candidate 
                which passes the flagging criteria.
              - AstroCatalog of all found transients candidates.
              - AstroCatalog like above but sub-selected for DB injection.
    Author  : Ruslan Konno (May 2026)
    Example : VisitPath = '/path/to/visit/dir'
              [AD, ADc, TCL1, TCL2, Status] = pipeline.last.pipes.pipelineII(VisitPath)
    %}

    arguments
        VisitData

        Args.RefPath = '';
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

        % Fallback Gaia catalog of the visit; AST_CAT wins (issue #1348)
        Args.RefCatName = 'GAIADR3';
        % Raw Gaia cones of the visit from pipelineI, and proper motion for
        % all the Gaia consumers below (issue #1348)
        Args.GaiaCone = [];
        Args.GaiaProperMotion logical = true;
        % Where to save a sub image whose ZOGY D/Pd is complex (issue #1360); '' - off
        Args.DumpComplexPath char = '';

        Args.CropIDs = [];

        % PSF-fit method used when re-fitting the Ref/New catalogues below.
        % psfFitPhot's own default is still 'legacy', which is much slower and
        % is the only remaining user of shift_fft (issue #1259).
        Args.PsfPhotMethod = '2DGN';   % 'legacy'/'old'|'1D'|'2D'|'2DGN'

        % Sub-pixel shift kernel of the PSF fit. Measured against known truth
        % (issue #1258): shifting the model PSF with lanczos3 costs ~1 mmag and,
        % worse, the bias grows with the sub-pixel offset, so no zero point
        % absorbs it; the FFT recovers the flux exactly. Set explicitly here so
        % the fit does not depend on psfFitPhot's default (issue #1257, item 3).
        Args.ShiftMethod = 'fft';   % 'lanczos3'|'fft' (issue #1258)

        Args.FilterConfigFile = '';

        Args.PixScale = 1.25;

        Args.PrecompKxKySize = [1716, 1716];
        Args.getKxKySizeFromImage logical = true;

        Args.InjectedSrcs = [];
        Args.RePopRefPSF = true;
        Args.RePopNewPSF = true;

        Args.SubselectionFalse = {'BadPixelHard', 'LIMMAG', 'Negative', ...
            'Overdensity', 'PVDist', 'Streak', 'PSFShape'};

        Args.applyCalibration logical = true;

        % Header keyword holding the photometric zero point of each image,
        % read by AstroDiff/estimateFnFr to set the New/Ref flux scaling
        % (Fr = 10^(0.4*(RefZP - NewZP))). estimateFnFr does not fit
        % anything: it divides the two header values, so both images must
        % carry a zero point on the SAME absolute scale or the difference
        % image is mis-scaled by exactly the offset between the two
        % calibrations (issue #1267). Default 'PH_ZP' preserves the existing
        % behaviour; use 'PT_ZP' on both sides once the reference images have
        % been rebuilt with pipeline.last.reference.rebuildRefProducts.
        % Changing only one side is worse than changing neither.
        Args.NewZP = 'PH_ZP';
        Args.RefZP = 'PH_ZP';
    end

    % 1: ----- Set default arguments -----

    % Set default return status.
    % The function should never be able to return the default status.     
    % If it does, there is an uncontrolled return.
    Status.Msg = 'Uncontrolled exit.';
    Status.Success = false;

    % Initialize empty output arguments
    AD = AstroZOGY();
    ADc = AstroZOGY();
    TCL1 = AstroCatalog();
    TCL2 = AstroCatalog();

    % Some unit conversion parameters
    Rad2Arcsec = 3600.*180./pi; %206265;
    Arcsec2Rad = 1./Rad2Arcsec; %4.84814e-6;

    % 2: ----- Set and verify paths -----
    
    % Get path of reference images and check if it exists, return if not.
    if isempty(Args.RefPath)
        Computer = tools.os.get_computer;
        RefPath = strcat('/',Computer,'/data/references');
    else
        RefPath = Args.RefPath;
    end

    if ~isfolder(RefPath)
        Status.Msg = 'RefPath directory not found, exiting.';
        return
    end
   
    % Find New image coadds and load
    if isa(VisitData, 'char') || isa(VisitData, 'string')

        if ~isfolder(VisitData)
            Status.Msg = 'VisitData directory not found, exiting.';
            return
        end

    % 3: ----- Load and verify New images -----

        Coadds = strcat(VisitData,'/LAST*coadd_Image_1.fits');
        % listed with AstroFileName, not FileNames (issue #1315)
        FNnew = AstroFileName.dir(Coadds);
        if FNnew.nFiles==0
            New = AstroImage;
        else
            New = AstroImage.readFileNamesObj(FNnew, 'Path', VisitData);
        end
    elseif isa(VisitData, 'AstroImage')
        New = VisitData;
    end
   
    % Check for empty images, return if all images are empty.
    NonEmptyNew = ~New.isemptyImage;

    if ~any(NonEmptyNew)
      Status.Msg = 'All New images are empty.';
      return
    end
   
    % Only use non-empty images.
    New = New(NonEmptyNew);
    Nobj = numel(New);

    % Gaia catalog of the visit: the one the astrometry of the New images
    % used, as recorded in AST_CAT, so that pipelineI's RefCatName governs
    % every Gaia query below (issue #1348). Args.RefCatName if no header
    % names one.
    AstCat = strings(1, Nobj);
    for Iobj=1:1:Nobj
        Val = New(Iobj).HeaderData.getVal('AST_CAT');
        if ischar(Val) || isstring(Val)
            AstCat(Iobj) = strtrim(string(Val));
        end
    end
    AstCat = unique(AstCat(AstCat~="" & AstCat~="USER"));
    if isempty(AstCat)
        GaiaCatName = Args.RefCatName;
    elseif isscalar(AstCat)
        GaiaCatName = char(AstCat);
    else
        error('pipelineII:MixedRefCat', ...
              'The New images name %d astrometric catalogs in AST_CAT (%s) - one Gaia catalog per visit is expected', ...
              numel(AstCat), strjoin(AstCat, ', '));
    end
   
    % 4: ----- Load and verify Ref images -----
    
    % Find reference image for each New image

    % Preload refs

    % Get name of New image and search for Ref image via wildcards.
    % The FileType is reduced to its first extension ("fits.fz" -> "fits"),
    % as FileNames parsed it, so that the search is unchanged (issue #1315)
    FNref = AstroFileName.parseString2AstroFileName(New(1).ImageData.FileName);
    FNref.FileType = extractBefore(FNref.FileType + ".", ".");

    % Convert telescope designation to wildcard if Refs from other
    % telescopes are allowed.
    if ~Args.SameTelOnly
        FNref.ProjName = replaceBetween(FNref.ProjName(1),"LAST.01.",".0","*");
    end

    % Wildcard time and crop ID.
    FNref.Time   = "*.*.*";
    FNref.CropID = "*";

    % Use only the LAST field ID for Ref search. If New image
    % observation was of an Object with a dot extsion, the dot
    % extension is removed for Ref search.
    FieldID = split(FNref.FieldID(1),'.');
    FieldID = char(FieldID(1));
    
    % Construct Ref filename
    FieldRefPath = strcat(RefPath, '/', FieldID);
    FNref.FieldID = FieldID;
    RefFile = fullfile(FieldRefPath, char(FNref.genFile));
    RefFile = replace(RefFile,'_coadd_','_*_');

    % Load Ref image as AstroImage and Ref image FileName object
    % (listed with AstroFileName, not FileNames - issue #1315)
    FNrefs = AstroFileName.dir(RefFile);
    if FNrefs.nFiles==0
        Refs = AstroImage;
    else
        Refs = AstroImage.readFileNamesObj(FNrefs, 'Path', FieldRefPath);
    end
    NumRefs = numel(Refs);

    if (NumRefs < 1) || ((NumRefs == 1) && isempty(Refs(1).Image))
        Status.Msg = 'No reference images found.';
        return;
    end

    % Track number of matched reference images
    NRefsMatched = 0;

    % Track number of images failing Args.MinimumNCoadd criterium.
    NBelowMinNCoadd = 0;

    % Tack number of images with no overlap to any reference image
    NoOverlap = 0;

    % Track number of New images without a source catalog
    Status.NnoCatalog = 0;

    for Iobj=Nobj:-1:1

        % Check if New image meets NCoadd criterium. If it does not,
        % remember and continue.
        NCOADD = New(Iobj).HeaderData.getVal('NCOADD');

        if NCOADD < Args.MinimumNCoadd
            NBelowMinNCoadd = NBelowMinNCoadd + 1;
            continue
        end

        % A New coadd saved without a catalog (no PSF, or failed coadd
        % astrometry - issue #1364) cannot be calibrated or searched.
        if New(Iobj).isemptyCatalog || ~New(Iobj).CatData.isColumn('RA')
            Status.NnoCatalog = Status.NnoCatalog + 1;
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

        FNrref = AstroFileName.parseString2AstroFileName(Ref.ImageData.FileName);
        FNrref.FileType = extractBefore(FNrref.FileType + ".", ".");

        % Make sure Ref products are complete, continue if not.
        if isempty(Ref.PSF) || isempty(Ref.Mask)
            warning('Missing reference products.');
            continue
        end

        % Generate New and Ref filenames properly
        FN = AstroFileName.parseString2AstroFileName(New(Iobj).ImageData.FileName);
        FN.FileType = extractBefore(FN.FileType + ".", ".");
        NewName = FN.genFile;
        RefName = FNrref.genFile;
        
        % Check if the New image is the Ref image, continue if they are.
        if NewName(1) == RefName(1)
            warning('New image is reference image.');
            continue
        end

        % Reference image found, remember this.
        NRefsMatched = NRefsMatched + 1;

        % Create AstroDiff (AstroZOGY)
        AD(Iobj) = AstroZOGY(New(Iobj), Ref);
        % Remember in AD if Ref image is already background subtracted.
        if FNrref.Level(1) == "ref"
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
        Status.Msg = 'All new images below required amount of NCOADD.';
        return
    end
    
    % If no Ref images found, return
    if NRefsMatched < 1
        Status.Msg = 'No reference images matched.';
        return
    end
   
    % If all New and Ref images have no overlap, return.
    if NoOverlap == Nobj
        Status.Msg = 'All New and Ref images have no overlap.';
        return
    end
    
    % Remove empty AstroDiff objects and remember number of AstroDiffs
    % Return if all are empty.
    NonEmptyCell = any(~cellfun('isempty',{AD(:).New}), 1);
    if ~any(NonEmptyCell)
        Status.Msg = 'All AstroDiffs are empty.';
        return
    end
    
    % Use only non-empty AstroDiff objects
    AD = AD(:, NonEmptyCell);
    Nobj = numel(AD);
        
    % 6: ----- Fill New and Ref estimates -----

    % Estimate backround and variance of New and Ref
    AD.estimateBackVar;

    if Args.RePopRefPSF
        for Iobj = Nobj:-1:1
            % keep the reference PSF; it is restored below, with its
            % photometry and zero point, if the re-populated one is not
            % sane (#1355)
            OrigPSF = AD(Iobj).Ref.PSFData.copy();
            AD(Iobj).Ref = imProc.psf.populatePSF(AD(Iobj).Ref, 'RePopulatePSF', true, 'Method', 'new');
                % uniPSF repop: every PSF-shape argument (RadiusPSF 12, Annulus
                % [16 20], analytic 3.7 wings @ 1e-2, elliptical, no ellipticity
                % fallback, CropByQuantile false, single detection PSF) comes
                % from the populatePSF/buildPSF uniPSF defaults.
            if ~AD(Iobj).Ref.isemptyPSF && ~isSanePSF(AD(Iobj).Ref.PSFData.getPSF)
                warning('Re-populated Ref PSF of CROPID %d is not sane (non-positive or off-centre peak), keeping the reference PSF.', ...
                        AD(Iobj).New.HeaderData.getVal('CROPID'));
                AD(Iobj).Ref.PSFData = OrigPSF;
            else
                AD(Iobj).Ref = imProc.sources.psfFitPhot(AD(Iobj).Ref, 'PsfPhotMethod',Args.PsfPhotMethod, ...
                                                                        'ShiftMethod',Args.ShiftMethod);
                AD(Iobj).Ref = imProc.calib.photometricZP(AD(Iobj).Ref, 'CatColNameMag', 'MAG_PSF', 'CatName',GaiaCatName, ...
                                                          'GaiaCone',Args.GaiaCone, 'EpochOut',gaiaEpoch(AD(Iobj).Ref, Args.GaiaProperMotion));
            end
        end
    end

    if Args.RePopNewPSF
        for Iobj = Nobj:-1:1
            % keep the PipelineI PSF; it is restored below, with its
            % photometry and zero point, if the re-populated one is not
            % sane (#1355)
            OrigPSF = AD(Iobj).New.PSFData.copy();
            AD(Iobj).New = imProc.psf.populatePSF(AD(Iobj).New, 'RePopulatePSF', true,...
                'SmoothWings', false, 'SuppressWidth', 3, 'RadiusPSF', 8,...
                'CropByQuantile', true, 'Quantile', 0.99999, 'Method', 'new', ...
                'WingsMethod', 'empirical', ...
                'Annulus', [10 12], 'WingsPowerLaw', 2, ...           % pinned pre-uniPSF values: the repop
                'EllipticalWings', false, 'SkipEllipticityFallback', false); % recipe is frozen until the subtraction
                                                                             % flow is validated on uniPSF defaults
            if ~AD(Iobj).New.isemptyPSF && ~isSanePSF(AD(Iobj).New.PSFData.getPSF)
                warning('Re-populated New PSF of CROPID %d is not sane (non-positive or off-centre peak), keeping the PipelineI PSF.', ...
                        AD(Iobj).New.HeaderData.getVal('CROPID'));
                AD(Iobj).New.PSFData = OrigPSF;
            else
                AD(Iobj).New = imProc.sources.psfFitPhot(AD(Iobj).New, 'PsfPhotMethod',Args.PsfPhotMethod, ...
                                                                        'ShiftMethod',Args.ShiftMethod);
                AD(Iobj).New = imProc.calib.photometricZP(AD(Iobj).New, 'CatColNameMag', 'MAG_PSF', 'CatName',GaiaCatName, ...
                                                          'GaiaCone',Args.GaiaCone, 'EpochOut',gaiaEpoch(AD(Iobj).New, Args.GaiaProperMotion));
            end
        end
    end    

    % Estimate zero points
    AD.estimateFnFr('NewZP',Args.NewZP, 'RefZP',Args.RefZP);

    if Args.applyCalibration
        for Iobj = Nobj:-1:1

            if ~AD(Iobj).Ref.HeaderData.isKeyExist('PT_SPEC')
                if AD(Iobj).RefIsBackgroundSubtracted
                    [AD(Iobj).Ref, AD(Iobj).PC_Ref] = imProc.calib.fitPhotCalibTrans(AD(Iobj).Ref, 'IsMeanImages', true);
                else
                    [AD(Iobj).Ref, AD(Iobj).PC_Ref] = imProc.calib.fitPhotCalibTrans(AD(Iobj).Ref, 'IsMeanImages', false);
                end
            end

            if ~AD(Iobj).New.HeaderData.isKeyExist('PT_SPEC')
                [AD(Iobj).New, AD(Iobj).PC_New] = imProc.calib.fitPhotCalibTrans(AD(Iobj).New, 'IsMeanImages', false);
            end
        end
    end

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
        Status.Msg = 'All New and Ref images overlap for less than half of the field.';
        return
    end

    % Remember new number of AstroDiffs
    Nobj = numel(AD);

    % Drop AstroDiffs whose New or Ref PSF is empty: subtractionD raises
    % 'New PSF is not populated' and aborts the whole visit otherwise (#1363).
    NoPSF = false(1, Nobj);
    for Iobj = 1:Nobj
        NoPSF(Iobj) = AD(Iobj).New.isemptyPSF || AD(Iobj).Ref.isemptyPSF;
    end
    if any(NoPSF)
        warning('Missing New or Ref PSF for CROPID %s, skipping these crops.', ...
                mat2str(arrayfun(@(A) A.New.HeaderData.getVal('CROPID'), AD(NoPSF))));
        AD   = AD(~NoPSF);
        Nobj = numel(AD);
    end
    if Nobj == 0
        Status.Msg = 'All New or Ref images have no PSF.';
        return
    end


    % 7: ----- Produce subtraction images -----
    
    % Create proper subtraction image D
    AD.subtractionD('DumpComplexPath',Args.DumpComplexPath);
    % Derive Gabor stat image
    AD.matchfilterGabor;
    % Derive S stat image
    AD.subtractionS('smearTemplateArgs', {'StarCatName',GaiaCatName, 'StarCone',Args.GaiaCone, ...
                                          'StarProperMotion',Args.GaiaProperMotion});
    % Derive Scorr stat image
    AD.subtractionScorr;
    % Derive Z2 stat image

    PrecompKxKySize = [];

    if Args.getKxKySizeFromImage
        [PrecompKxKySize(1), PrecompKxKySize(2)] = AD(1).ImageData.sizeImage;
    elseif ~isempty(Args.PrecompKxKySize)
        PrecompKxKySize = Args.PrecompKxKySize;
    end

    AD.translient('PrecompKxKySize', PrecompKxKySize);

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
    StarCat = catsHTM.cone_search(GaiaCatName, C_RA_med, C_Dec_med, ...
        MaxDistRad, 'RadiusUnits', 'rad', 'OutType','AstroCatalog');
    % Move the stars to the epoch of the visit (issue #1348)
    if Args.GaiaProperMotion && ~isemptyCatalog(StarCat) && any(strcmp(StarCat.ColNames, 'Epoch'))
        VisitJD = nan(Nobj, 1);
        for Iobj=1:1:Nobj
            ValJD = gaiaEpoch(AD(Iobj).New, true);
            if ~isempty(ValJD)
                VisitJD(Iobj) = ValJD;
            end
        end
        VisitJD = median(VisitJD, 'omitnan');
        if isfinite(VisitJD)
            StarCat = imProc.cat.applyProperMotion(StarCat, StarCat.getCol('Epoch'), VisitJD, ...
                                                   'EpochInUnits','j', 'CreateNewObj',false);
        end
    end
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
    clear OrbElMerge;

    % Comet matching
    
    OrbElComet= celestial.OrbitalEl.loadSolarSystem('comet');

    % Match Comet in New

    [ComCatNew] = OrbElComet.searchMinorPlanetsNearPosition(...
        NewJulDay, C_RA_med, C_Dec_med, MaxDistRad,...
        'INPOP', INPOP, ...      % reuse the ephemeris built for the asteroids (#1257)
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
        'INPOP', INPOP, ...      % reuse the ephemeris built for the asteroids (#1257)
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
    clear INPOP;
    
    % Measure transients
    AD.measureTransients;

    % Flag non transients
    AD.flagNonTransients('ConfigFile', Args.FilterConfigFile,...
        'injectedSrcs', Args.InjectedSrcs);


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

    if Args.applyCalibration
        AD.calibrateTransients;
    end

    % 9: ----- Create output products -----

    % Create a merged catalog, holding all candidates in the individual AD
    % catalogs. Generally this will be a visit catalog when used in the
    % pipeline.
    for Iobj=Nobj:-1:1
        TranCat(Iobj) = AD(Iobj).CatData;
    end

    % Create Level 1 Transients Catalog
    TCL1 = merge(TranCat);
    TCL1.sortrows('Dec');
    
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
    % them. Candidates within 1.5 arcsec in different sub-images are
    % linked, and each connected group is one set of duplicates. In each
    % group, the candidate closest to the center of its sub-image is the
    % survivor, regardless of FLAGS_TRANSIENT (issue #1374), and all
    % group members in the survivor's sub-image are kept (near candidates
    % in the same sub-image are distinct detections). Ties are broken by
    % the lowest CROPID.
    if Args.killDuplicates

        % Remember the number of positive candidates before removing
        % duplicates
        NADcWithDups = sum(TCL1.getCol('FLAGS_TRANSIENT') == 0);
        
        % Clean merged catalog
        % Match all candidates within 1.5 arcsec
        [MRA, MDec] = TCL1.getLonLat('rad');
        NCand = numel(MRA);
        Duplicates = false(NCand,1);
        if NCand > 1
            HalfSize = size(AD(1).Image)./2;
            SelfMatches = VO.search.search_sortedlat_multi( ...
                    [MRA, MDec], MRA, MDec, 1.5*Arcsec2Rad);
            IMatch = repelem((1:NCand)', vertcat(SelfMatches.Nmatch));
            JMatch = vertcat(SelfMatches.Ind);
            % Link only matches in different sub-images
            CropIDs = TCL1.getCol('CROPID');
            Link = IMatch ~= JMatch & CropIDs(IMatch) ~= CropIDs(JMatch);
            LinkMat = sparse(IMatch(Link), JMatch(Link), 1, NCand, NCand);
            % Duplicate groups (single candidates are groups of one)
            Group = conncomp(graph(LinkMat | LinkMat'))';

            % Choose the candidate closest to the center as the survivor of
            % each group, and keep the group members in its sub-image.
            [DupX, DupY] = TCL1.getXY('ColX','XPEAK','ColY','YPEAK');
            CenterDistance = sqrt((DupX-HalfSize(2)).^2+(DupY-HalfSize(1)).^2);
            MinDistance = accumarray(Group, CenterDistance, [], @min);
            Survivor = CenterDistance == MinDistance(Group);
            SurvivorCrop = accumarray(Group(Survivor), CropIDs(Survivor), ...
                    [max(Group) 1], @min);
            Duplicates = CropIDs ~= SurvivorCrop(Group);
        end
        % Update the merged catalog by keeping only the candidates not
        % marked as duplicates.
        TCL1 = TCL1.selectRows(~Duplicates);
        TCL1.sortrows('Dec');

        % Clean ADc if necessary
        % Check the number of positive candidates after duplicate removal
        NADcWithoutDups = sum(TCL1.getCol('FLAGS_TRANSIENT') == 0);
        
        % If the new number of positive candidates is lower than before
        % duplicate removal, we need to kill some cutout objects.
        if NADcWithDups > NADcWithoutDups
            % Get a catalog holding on the positive candidates
            PassingTranCat = TCL1.selectRows(...
                TCL1.getCol('FLAGS_TRANSIENT') == 0);
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
            % If all cutouts were killed, keep one empty object, as when
            % there are no candidates at all (issue #1374)
            if NADc == 0
                ADc = AstroZOGY();
            end
        end

    end

    % Create Level 2 Transients Catalog

    TCL2 = TCL1;

    % Load filter flags
    BD_TF = BitDictionary('BitMask.TransientsFilter.Default');
    Flags = TCL2.getCol('FLAGS_TRANSIENT');

    % Filter out candidates that fail selected filters
    if ~isempty(Args.SubselectionFalse)

        Subselect = true(numel(Flags),1);
        
        NFlags = numel(Args.SubselectionFalse);

        for IFlags = 1:NFlags
            Subselect = Subselect & ~BD_TF.findBit(Flags,Args.SubselectionFalse{IFlags});
        end

        TCL2 = TCL2.selectRows(Subselect);
    end

    % Update Status and finish
    StatusCell = strcat('Succesful exit,',{' '}, ...
        num2str(NADc),{' '},'transient(s) found.');
    if Status.NnoCatalog>0
        StatusCell{1} = sprintf('%s %d New image(s) without a catalog skipped (issue #1364).', StatusCell{1}, Status.NnoCatalog);
    end

    Status.Msg = StatusCell{1};
    Status.Success = true;
end


function JD = gaiaEpoch(Image, ApplyPM)
    % The epoch [JD] to move the Gaia sources to for an image (issue #1348).
    % Input  : - An AstroImage.
    %          - Apply the proper motion (true) or not (false).
    % Output : - The image JD from its header; [] if ApplyPM is false or
    %            the header has no usable JD (no proper motion is then
    %            applied).
    % Author : Alexander Gioffe (Sep 2026)

    JD = [];
    if ApplyPM
        try
            Val = Image.julday;
            if ~isempty(Val) && isfinite(Val(1))
                JD = Val(1);
            end
        catch
            JD = [];   % no readable JD - no proper motion
        end
    end
end

function Flag = isSanePSF(P)
    % A PSF stamp is sane if it is finite and its maximum is positive and
    % within 1 pix of the stamp centre. An inverted PSF (e.g. built from
    % stamps with a negative sum, #1355) has its maximum in the noise floor.
    Flag = false;
    if isempty(P) || any(~isfinite(P(:)))
        return
    end
    [MaxVal, Imax] = max(P(:));
    [Iy, Ix] = ind2sub(size(P), Imax);
    Ctr = (size(P) + 1)./2;
    Flag = MaxVal > 0 && abs(Iy - Ctr(1)) <= 1 && abs(Ix - Ctr(2)) <= 1;
end
