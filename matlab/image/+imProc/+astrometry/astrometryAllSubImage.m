function [ResultFit, AI, CatName] = astrometryAllSubImage(Obj, Args)
    % One line description
    %     Optional detailed description
    % Input  : - 
    %          - 
    %          * ...,key,val,... 
    % Output : - 
    % Author : Eran Ofek (2025 Nov) 
    % Example: [ResFit,AI,CatName]=imProc.astrometry.astrometryAllSubImage(AI);

    arguments
        Obj

        Args.StartSubImage          = 10;
        Args.CCDSEC                 = 'ORIGSEC';

        Args.Scale                  = 1.25;  % [arcsec/pix]
        Args.Tran                   = Tran2D('poly3');
        Args.CatName                = 'GAIADR3';
        Args.SortByTime             = false;
        Args.CreateNewObj           = false;
        Args.JD                     = [];  % JD of images - read from header if not given
        Args.InitWCS                = [];   % If scalar refer to StartSubImage. Otherwise for all.
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
        JD = AI.julday();
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
    
    Iwcs    = min(Nwcs, Args.StartSubImage);
    if Iwcs==0
        InitWCS = [];
        Iwcs = Args.StartSubImage;
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
   
    ResultFit = imProc.astrometry.defResultFit(Nai);

    [ResultFit(Iwcs), AI(Args.StartSubImage), CatName(Iwcs)] = imProc.astrometry.astrometrySingleImage(AI(Iwcs),...
                                                                                                'InitWCS',InitWCS,...
                                                                                                'CatName',InCatName,...
                                                                                                'Scale',Args.Scale,...
                                                                                                'Tran',Args.Tran,...
                                                                                                'SortByTime',Args.SortByTime,...
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


    SolvedWCS = AstroWCS([Nai,1]);
    SolvedWCS(Iwcs) = ResultFit(Iwcs).WCS;

    FlagNotDone = true(Nai,1);
    if ~isempty(ResultFit(Iwcs)) && ~isempty(ResultFit(Iwcs).WCS) && ResultFit(Iwcs).WCS.Success
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

        [Iai, Iwcs, Iccdsec, Iref]
        [ResultFit(Iwcs), AI(Iwcs), CatTmp] = imProc.astrometry.astrometrySingleImage(AI(Iwcs),...
                                                                                                'InitWCS',InitWCS,...
                                                                                                'CatName',CatNameI,...
                                                                                                'Scale',Args.Scale,...
                                                                                                'Tran',Args.Tran,...
                                                                                                'SortByTime',Args.SortByTime,...
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


        ResultFit(Iwcs)

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
