function [ResultFit, AI, CatName] = astrometryAllSubImage(Obj, Args)
    % Solve astrometry to all sub images belonging to one image.
    %     Given a vector of AstroImage object containing sub images
    %     belonging to a single image, perform astrometry to all sub
    %     images. The astrometry starts by running astrometryCore on one
    %     sub image. Next, the WCS is extrapolated to nearby subimages and
    %     use astrometryRefine to solve the astrometry of these sub images.
    % Input  : - A vector of AstroImage objects that contains images and
    %            catalogs belonging to a single image.
    %          * ...,key,val,... 
    %            'StartSubImage' - Index of sub image in the input
    %                   AstroImage vector from which to start.
    %                   I.e., will run astrometryCore on this subimage.
    %            'CCDSEC' - Either a 4 columns matrix of CCDSEC (in the
    %                   original full image) of the subimages contained in
    %                   the vector of sub images.
    %                   Or, this is an header keyword from which to get the
    %                   CCDSEC. Default is 'ORIGSEC'.
    %
    %            'Scale' - Value, or range of scale [arcse/pix].
    %                   Default is 1.0
    %            'Tran' - A Tran2D object describing the 2D transformation
    %                   to fit. Default is Tran2D('poly3')
    %            'CatName' - Either an astrometric catalog name (char
    %                   array) to query around the requested coordinates,
    %                   or an AstroCatalog object containing such a
    %                   catalaog.
    %                   Default is 'GAIADR3'.
    %            'CreateNewObject' - A logical indicating if to create a
    %                   new copy of the AstroImage handle object.
    %                   Default is false.
    %            'JD' - A vector of JD of the AstroImage images.
    %                   If empty, then get JD from header.
    %                   Default is [].
    %            'InitWCS' - An initial AstroWCS object. If provided, then
    %                   will be used as the InitWCS guess for the first image
    %                   I.e., if given astrometryRefine will be attempted
    %                   on the first image.
    %                   Default is [].
    %            'RA' - Optional J2000 R.A. [deg]. If given and InitWCS is
    %                   empty, then will use it.
    %                   Will be used only for the first sub image.
    %                   Default is [].
    %            'Dec' - Optional J2000 Dec. [deg]. If given and InitWCS is
    %                   empty, then will use it. Default is [].
    %            'SkipSolved' - If true, then skip images with WCS in which
    %                   Success is true. Default is true.
    %           
    %            'FunRefineSearchRadiusNsrc' - A function handle that get
    %                   the number of sources in the image and return a
    %                   search radius (arcsec) for astrometryRefine.
    %                   Default is @(Nsrc) min(max(5,2e5./(Nsrc.^2)),30)
    %            'CooOffset' - Optional offset to add to the initial guess
    %                   RA,Dec [deg]. Default is [0 0].
    %
    %            --- Arguments of astrometryRefine, astrometryCore ---
    %            'CooUnits' - The RA, Dec units in the AstroCatalog in the
    %                   AstroImage input. Default is 'deg'.
    %            'CatRadius' - Search radius for the Catalog.
    %                   Default is 3600 arcsec.
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
    %            'KeyExpTime' - Header keyword for the exposure time.
    %                   Default is 'EXPTIME'.
    %            'RefRangeMagExpTimeFun' - Function handle for the upper value of the RefMagRange
    %                   as a function of ExpTime.
    %                   Default is @(ET) 1.8.*log10(ET);
    %            'astrometryCoreArgs' - A cell array of additional arguments to pass
    %                   to imProc.astrometry.astrometryCore.
    %                   Default is {}.
    %            'astrometryRefineArgs' - A cell array of additional arguments to pass
    %                   to imProc.astrometry.astrometryRefine.
    %                   Default is {}.
    %
    % Output : - A structure array of astrometry fit results.
    %            This structure array is common to astrometryCore,
    %            astrometryREfine and all the other astrometric functions.
    %            It contains, among others, the following fields:
    %            'ParWCS' - The WCS parameters.
    %            'Tran' - The fitted Tran2D object.
    %            'ResFit' - The best fit results summary.
    %            'WCS' - An updated WCS object with the best fit solution.
    %          - The input AstroImage in which the WCS is updated.
    %          - An array of AstroCatalog contains the astrometric (e.g., GAIA)
    %            catalog for each one of the elements in the input
    %            AstroImage.
    % Author : Eran Ofek (2025 Nov) 
    % Example: [ResFit,AI,CatName]=imProc.astrometry.astrometryAllSubImage(AI);
    %          [ResFit,AI,CatName]=imProc.astrometry.astrometryAllSubImage(AllSI(1,:),'SkipSolved',true,'CatName',CatName);

    arguments
        Obj

        Args.StartSubImage          = [10 16 9 15];
        Args.CCDSEC                 = 'ORIGSEC';

        Args.Scale                  = 1.25;  % [arcsec/pix]
        Args.Tran                   = Tran2D('poly3');
        Args.CatName                = 'GAIADR3';
        %Args.SortByTime             = false;
        Args.CreateNewObj           = false;
        Args.JD                     = [];  % JD of images - read from header if not given
        Args.KeyJD                  = 'MIDJD';
        Args.InitWCS                = [];   % If scalar refer to StartSubImage. Otherwise for all.
        Args.RA                     = [];
        Args.Dec                    = [];
        Args.SkipSolved             = true;


        Args.FunRefineSearchRadiusNsrc = @(Nsrc) min(max(5,2e5./(Nsrc.^2)),30); % the astrometryRefine search radius as a function of Nsrc
        Args.CooOffset              = [0 0]; % [RA Dec] deg

        Args.CooUnits               = 'deg';
        Args.CatRadius              = 3600;
        Args.RangeX                 = [-1000 1000].*2.5;
        Args.RangeY                 = [-1000 1000].*2.5;
        Args.StepX                  = 2;
        Args.StepY                  = 2;
        Args.Flip                   = [1 1;-1 -1];
        Args.RefRangeMag            = [10 17.0];  % [12 18]
        Args.SearchRadius           = 6;
        Args.FilterSigma            = 3;
        
        % Dynamic definition of RefRangeMag:
        Args.KeyExpTime             = 'EXPTIME';
        Args.RefRangeMagExpTimeFun  = @(ET) 1.8.*log10(ET);

        Args.astrometryCoreArgs     = {};
        Args.astrometryRefineArgs   = {};
    end

    if Args.CreateNewObj
        AI = Obj.copy;
    else
        AI = Obj;
    end
    Nai = numel(AI);

    if isempty(Args.JD)
        JD = AI.julday('KeyJD',Args.KeyJD);
    else
        JD = Args.JD;
    end

    ExpTime = AI(1).HeaderData.getVal(Args.KeyExpTime);
    if ~isnan(ExpTime) && ~isempty(Args.RefRangeMagExpTimeFun)
        Diff = diff(Args.RefRangeMagExpTimeFun([20 ExpTime]));
        
        Args.RefRangeMag = Args.RefRangeMag + Diff;
    end

    if isnumeric(Args.CCDSEC)
        % CCDSEC is a matrix of CCDSEC per image
        CCDSEC = Args.CCDSEC;
    else
        % attempt to read CCDSEC from header
        StCCDSEC = AI.getStructKey(Args.CCDSEC);
        Nst = numel(StCCDSEC);
        CCDSEC = nan(Nst,4);
        for Ist=1:1:Nst
            CCDSEC(Ist,:) = imUtil.ccdsec.ccdsecStr2num(StCCDSEC(Ist).(Args.CCDSEC));
        end
    end

    

    Nwcs = numel(Args.InitWCS);
    
    Nstart = numel(Args.StartSubImage);

 
    ResultFit = imProc.astrometry.defResultFit(Nai);

    Istart = 0;
    NotFound = true;
    while NotFound
        Istart   = Istart + 1;

        Iwcs    = min(Nwcs, Args.StartSubImage(Istart));
        if Iwcs==0
            InitWCS = [];
            Iwcs = Args.StartSubImage(Istart);
        else
            InitWCS = Args.InitWCS(Iwcs);
        end
        
        if isa(Args.CatName,'AstroCatalog')
            Ncat = numel(Args.CatName);
            Icat = min(Ncat, Iwcs);
            if Ncat==1
                % allocate CatName
                CatName = AstroCatalog([Nai,1]);
                CatName(Args.StartSubImage) = Args.CatName;
            else
                CatName = Args.CatName;
            end
            InCatName = CatName(Args.StartSubImage);
        else
            InCatName = Args.CatName;
            CatName   = AstroCatalog([Nai,1]);
        end

        [ResultFit(Args.StartSubImage(Istart)), AI(Args.StartSubImage(Istart)), CatName(Args.StartSubImage(Istart))] = imProc.astrometry.astrometrySingleImage(AI(Args.StartSubImage(Istart)),...
                                                                                                'InitWCS',InitWCS,...
                                                                                                'RA',Args.RA,...
                                                                                                'Dec',Args.Dec,...
                                                                                                'CatName',InCatName,...
                                                                                                'Scale',Args.Scale,...
                                                                                                'Tran',Args.Tran,...
                                                                                                'CreateNewObj',Args.CreateNewObj,...
                                                                                                'JD',Args.JD,...
                                                                                                'SkipSolved',Args.SkipSolved,...
                                                                                                'FunRefineSearchRadiusNsrc',Args.FunRefineSearchRadiusNsrc,...
                                                                                                'CooOffset',Args.CooOffset,...
                                                                                                'CooUnits',Args.CooUnits,...
                                                                                                'CatRadius',Args.CatRadius,...
                                                                                                'RangeX',Args.RangeX,...
                                                                                                'RangeY',Args.RangeY,...
                                                                                                'StepX',Args.StepX,...
                                                                                                'StepY',Args.StepY,...
                                                                                                'Flip',Args.Flip,...
                                                                                                'RefRangeMag',Args.RefRangeMag,...
                                                                                                'SearchRadius',Args.SearchRadius,...
                                                                                                'FilterSigma',Args.FilterSigma,...
                                                                                                'KeyExpTime',Args.KeyExpTime,...
                                                                                                'RefRangeMagExpTimeFun',Args.RefRangeMagExpTimeFun,...
                                                                                                'astrometryCoreArgs',Args.astrometryCoreArgs,...
                                                                                                'astrometryRefineArgs',Args.astrometryRefineArgs);

        if ResultFit(Args.StartSubImage(Istart)).Success || Istart>=Nstart
            % found or want over all options
            NotFound = false;
        end
    end

    SolvedWCS = AstroWCS([Nai,1]);
    SolvedWCS(Args.StartSubImage(Istart)) = ResultFit(Args.StartSubImage(Istart)).WCS;

    FlagNotDone = true(Nai,1);
    if ~isempty(ResultFit(Args.StartSubImage)) && ~isempty(ResultFit(Args.StartSubImage).WCS) && ResultFit(Args.StartSubImage).WCS.Success
        FlagNotDone(Args.StartSubImage) = false;
    end
    IndNotDone = find(FlagNotDone);
    

    ArgsIter = Args;
    for Iai=1:1:Nai-1        
        
        %[InitWCS, Iccdsec]=imProc.astrometry.remapWCS(CCDSEC(IndNotDone,:), AI, CCDSEC, 'JD',[]);
        [InitWCS, Iccdsec, Iref]=imProc.astrometry.remapWCS(CCDSEC(IndNotDone,:), SolvedWCS, CCDSEC, 'JD',[]);

        Iwcs = IndNotDone(Iccdsec);

        if isempty(CatName(Iwcs).Catalog)
            CatNameI = Args.CatName;
        else
            CatNameI = CatName(Iwcs);
        end

        %[Iai, Iccdsec, Iwcs, Iref]
        [ResultFit(Iwcs), AI(Iwcs), CatTmp] = imProc.astrometry.astrometrySingleImage(AI(Iwcs),...
                                                                                                'InitWCS',InitWCS,...
                                                                                                'CatName',CatNameI,...
                                                                                                'Scale',Args.Scale,...
                                                                                                'Tran',Args.Tran,...
                                                                                                'CreateNewObj',Args.CreateNewObj,...
                                                                                                'JD',Args.JD,...
                                                                                                'SkipSolved',Args.SkipSolved,...
                                                                                                'FunRefineSearchRadiusNsrc',Args.FunRefineSearchRadiusNsrc,...
                                                                                                'CooOffset',Args.CooOffset,...
                                                                                                'CooUnits',Args.CooUnits,...
                                                                                                'CatRadius',Args.CatRadius,...
                                                                                                'RangeX',Args.RangeX,...
                                                                                                'RangeY',Args.RangeY,...
                                                                                                'StepX',Args.StepX,...
                                                                                                'StepY',Args.StepY,...
                                                                                                'Flip',Args.Flip,...
                                                                                                'RefRangeMag',Args.RefRangeMag,...
                                                                                                'SearchRadius',Args.SearchRadius,...
                                                                                                'FilterSigma',Args.FilterSigma,...
                                                                                                'KeyExpTime',Args.KeyExpTime,...
                                                                                                'RefRangeMagExpTimeFun',Args.RefRangeMagExpTimeFun,...
                                                                                                'astrometryCoreArgs',Args.astrometryCoreArgs,...
                                                                                                'astrometryRefineArgs',Args.astrometryRefineArgs);


        %ResultFit(Iwcs)

        if isa(CatTmp, 'AstroCatalog')
            CatName(Iwcs) = CatTmp;
        end
        SolvedWCS(Iwcs) = ResultFit(Iwcs).WCS;


        if ~isempty(ResultFit(Iwcs)) && ~isempty(ResultFit(Iwcs).WCS) && ResultFit(Iwcs).WCS.Success
            FlagNotDone(Iwcs) = false;
        end
        IndNotDone = find(FlagNotDone);
    end

end
