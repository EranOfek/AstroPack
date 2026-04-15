function [ResultFit, AI, CatName] = astrometrySameImage(Obj, Args)
    % Run astrometryCore/astrometryRefine on a set of images with similar pointing (few arcsec).
    %   Given a vector of AstroImage object with images at the same sky position
    %   (to accuracy of a few arcsec), solve the astrometry for all images.
    %   The function uses the advantage that all the images are of similar position.
    %   Therefore, the first image will be solved using astrometryCore, 
    %   and if the previous images astrometry is good, then the next image
    %   will be solved using astrometryRefine.
    %   Alternatively, the use can provide an initial WCS ('InitWCS'). In
    %   this case astrometryRefine will be attempted also on the first
    %   image. If astrometryRefine is failed, then execute astrometryCore.
    %   If astrometryCore is failed, then attempt again with somewhat
    %   different parameters.
    %
    % Input  : - A vector of AstroImage object.
    %          * ...,key,val,... 
    %            'Scale' - Value, or range of scale [arcse/pix].
    %                   Default is 1.0
    %            'Tran' - A Tran2D object describing the 2D transformation
    %                   to fit. Default is Tran2D('poly3')
    %            'CatName' - Either an astrometric catalog name (char
    %                   array) to query around the requested coordinates,
    %                   or an AstroCatalog object containing such a
    %                   catalaog.
    %                   Default is 'GAIADR3'.
    %            'SortByTime' - A logical indicating if to sort the vector
    %                   of images bt time. Default is false.
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
    %                   empty, then will use it. Default is [].
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
    % Author : Eran Ofek (2025 Nov) 
    % Example: [ResFit, AI, Cat]=imProc.astrometry.astrometrySameImage(AI);
    %          [ResFit, AI, Cat]=imProc.astrometry.astrometrySameImage(AI(:,10), 'InitWCS',AI(1,10));

    arguments
        Obj
        
        Args.Scale                  = 1.25;  % [arcsec/pix]
        Args.Tran                   = Tran2D('poly3');
        Args.CatName                = 'GAIADR3';
        Args.SortByTime             = false;
        Args.CreateNewObj           = false;
        Args.JD                     = [];  % JD of images - read from header if not given
        Args.InitWCS                = [];
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

        Args.MatchMethod            = 'old'; % 'old'|'mex'
    end

    if Args.CreateNewObj
        AI = Obj.copy;
    else
        AI = Obj;
    end
    Nai = numel(AI);

    if isempty(Args.JD)
        JD = AI.julday();
    else
        JD = Args.JD;
    end


    if Args.SortByTime
        [~,SI] = sort(JD);
        AI     = AI(SI);
        JD     = JD(SI);
    end

    ExpTime = AI(1).HeaderData.getVal(Args.KeyExpTime);
    if ~isnan(ExpTime) && ~isempty(Args.RefRangeMagExpTimeFun)
        Diff = diff(Args.RefRangeMagExpTimeFun([20 ExpTime]));
        
        Args.RefRangeMag = Args.RefRangeMag + Diff;
    end
    
            


    if ~isempty(Args.RA) && ~isempty(Args.Dec)
        RA  = Args.RA  + Args.CooOffset(1);
        Dec = Args.Dec + Args.CooOffset(2);
    else
        if isempty(Args.InitWCS)
            InitWCS = AstroWCS([1,1]);
    
            % get approximate coordinates for field center
            [RA, Dec] = getCoo(AI(1).HeaderData);
            RA        = RA  + Args.CooOffset(1);
            Dec       = Dec + Args.CooOffset(2);
            
        else
            InitWCS = Args.InitWCS;
    
            RA  = InitWCS.CRVAL(1);
            Dec = InitWCS.CRVAL(2);
    
        end
    end

    CatName = Args.CatName;
    ResultFit = imProc.astrometry.defResultFit(Nai);
    % ResultFit = struct('ImageCenterXY',cell(Nai,1),...
    %                 'Nsolutions',cell(Nai,1),...
    %                 'ResPattern',cell(Nai,1),...
    %                 'ErrorOnMean',cell(Nai,1),...
    %                 'BestInd',cell(Nai,1),...
    %                 'WCS',cell(Nai,1),...
    %                 'ParWCS',cell(Nai,1),...
    %                 'Tran',cell(Nai,1),...
    %                 'ResFit',cell(Nai,1),...
    %                 'Origin',cell(Nai,1),...
    %                 'Success',cell(Nai,1));

    

    Success  = false(Nai,1);
    for Iai=1:1:Nai
        
        %CellArgs = namedargs2cell(Args); % must be inside the loop because of INitWCS updates...

        [ResultFit(Iai), AI(Iai), CatName] = imProc.astrometry.astrometrySingleImage(AI(Iai), 'Scale',Args.Scale,...
                                                                                                'Tran',Args.Tran,...
                                                                                                'CatName',Args.CatName,...
                                                                                                'CreateNewObj',false,...
                                                                                                'JD',JD,...
                                                                                                'InitWCS',InitWCS,...
                                                                                                'RA',RA,...
                                                                                                'Dec',Dec,...
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
                                                                                                'MatchMethod',Args.MatchMethod,...
                                                                                                'astrometryCoreArgs',Args.astrometryCoreArgs,...
                                                                                                'astrometryRefineArgs',Args.astrometryRefineArgs);


        %
    
        if Iai==1
            % Update CatName
            Args.CatName = CatName;
        end

        if ~isempty(ResultFit(Iai)) && ~isempty(ResultFit(Iai).WCS) && ResultFit(Iai).WCS.Success
            % copy WCS to InitWCS (for next image)
            Args.InitWCS = ResultFit(Iai).WCS;
        end
        

        if ResultFit(Iai).Success
            % set InitWCS to the latest good WCS
            InitWCS = ResultFit(Iai).WCS;
            Success(Iai) = ResultFit(Iai).WCS.Success;
        end
        

    end % for Iai=1:1:Nai

end
