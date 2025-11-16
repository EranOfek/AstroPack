function [ResFit, AI, CatName] = astrometryVisitSubImage(Obj, Args)
    % Perform astrometry for all sub images in all the images in a visit.
    %   The function input is a an AstroImage matrix in which the 1st
    %   dimension is epoch, and 2nd dimension is sub image index.
    %   The function attempt to solve the astrometry of all sub images in
    %   the 1st epoch using: imProc.astrometry.astrometryAllSubImage.
    %   Next, for each sub images, it solves all epochs using:
    %   imProc.astrometry.astrometrySameImage.
    %
    % Input  : - An AstroImage object array containing images for all sub
    %            images and all epochs. First dim is for epoch, and 2nd dim
    %            for sub image index.
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
    % Output : - 
    % Author : Eran Ofek (2025 Nov) 
    % Example: 
    % [ResFit, AI, CatName]=imProc.astrometry.astrometryVisitSubImage(AllSI);
    % [ResFit, AI, CatName]=imProc.astrometry.astrometryVisitSubImage(AllSI,'InitWCS',AllSI(1,10).WCS,'SkipSolved',false); % test with known init WCS
    % [ResFit, AI, CatName]=imProc.astrometry.astrometryVisitSubImage(AllSI,'InitWCS',[],'SkipSolved',false)

    arguments
        Obj
        Args.StartSubImage          = 10;
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
    
    [Nep, Nsub] = size(AI);
    ResFit = imProc.astrometry.defResultFit([Nep, Nsub]);

    if isa(Args.CatName, 'AstroCatalog')
        % Catalogs are provided for all elements
        CatNameEpoch1 = Args.CatName(1,:);
        CatName       = Args.CatName;
        if numel(CatName)~=Nsub
            error('Illegal number of AstroCatalog elements in CatName');
        end
    else
        CatNameEpoch1 = Args.CatName;
        CatName = AstroCatalog([1, Nsub]);
    end


    % Solve all sub images in the first epoch
    [ResFit(1,:),AI(1,:),CatName] = imProc.astrometry.astrometryAllSubImage(AI(1,:),...
                                                                            'CatName',CatNameEpoch1,...
                                                                            'StartSubImage',Args.StartSubImage,...
                                                                            'CCDSEC',Args.CCDSEC,...
                                                                            'Scale',Args.Scale,...
                                                                            'Tran',Args.Tran,...
                                                                            'CreateNewObj',false,...
                                                                            'JD',Args.JD,...
                                                                            'KeyJD',Args.KeyJD,...
                                                                            'InitWCS',Args.InitWCS,...
                                                                            'RA',Args.RA,...
                                                                            'Dec',Args.Dec,...
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


    if Nep>1
        % solve all epochs for each sub image
        for Isub=1:1:Nsub
            %Isub
          

            [ResFit(2:end,Isub), AI(2:end,Isub)] = imProc.astrometry.astrometrySameImage(AI(2:end,Isub),...
                                                                                         'Scale',Args.Scale,...
                                                                                         'Tran',Args.Tran,...
                                                                                         'CatName',CatName(Isub),...
                                                                                         'SortByTime',false,...
                                                                                         'CreateNewObj',false,...
                                                                                         'JD',Args.JD,...
                                                                                         'InitWCS',AI(1,Isub).WCS,...
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




        end
    end


end
