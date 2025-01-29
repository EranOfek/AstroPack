function [ADc, TranCatLevel2, Status] = matchTransientsToMultiEpochs(ADc, TranCatLevel1, Args)
    %{
    Match transients to previous observations.
    Input   : - AstroDiff cutouts on transients.
              - AstroCat holding transient catalogs.
              * ...,key,val,...
                'SubselectionFalse' - Cell of filter names. All candidates
                       with TranCatLevel1 that failed these filters will be
                       removed before multi-epoch matching.
                       Default is {'BadPixelHard', 'StarMatch', 'LIMMAG', 
                       'MPMatch', 'Negative'}.
    Output  : - AstroDiff cutouts on transients updated with multi-epoch information.
    Author  : Ruslan Konno (Oct 2024)
    Example : VisitPath = '/path/to/visit/dir'
              [AD, ADc, TCL1, Status] = pipeline.last.transients.runTransientsPipe(VisitPath)
              [ADc, TCL2, Status] = pipeline.last.transients.matchTransientsToMultiEpochs(ADc, TCL1)
    %}

    arguments
        ADc
        TranCatLevel1

        Args.SubselectionFalse = {'BadPixelHard', 'LIMMAG', 'Negative', ...
            'Overdensity', 'PVDist', 'Streak', 'PeakDist', 'Variable'};

        Args.DB = [];
        Args.DbHost = 'last0';
    end
    
    Status = 'Uncontrolled exit.';

    TranCatLevel2 = [];

    % Return if catalog is empty
    if TranCatLevel1.sizeCatalog < 1
        Status = 'Transients catalog empty.';
        return
    end

    % TODO: No longer has to be separate, merge to main function
    % Match to DB
    [ADc, TranCatLevel2, Status] = matchTransientsToDB(ADc, TranCatLevel1, ...
        'SubselectionFalse', Args.SubselectionFalse,...
        'DbHost', Args.DbHost, 'DB', Args.DB);

end

function [ADc, TranCatLevel2, Status] = matchTransientsToDB(ADc, TranCatLevel1, Args)

    arguments
        ADc
        TranCatLevel1
        
        Args.SubselectionFalse = {'BadPixelHard', 'StarMatch', ...
            'LIMMAG', 'MPMatch', 'Negative'};
        
        Args.Template = '~/matlab/data/db/Design-Database-Pipeline-ClickHouse.xlsx';
        
        Args.DB     = [];
        
        Args.DbHost = 'socsrv';
        Args.DbName = 'last';   
        Args.DbUser = 'default';
        Args.DbPass = ''; 
        
        Args.Level  = 'coadd';
        Args.DbTable= 'diff_src';     %  
        Args.KeyID     = 'id_new_im'; % 'id_visit_im' ???  
        Args.ColNameID = 'id_diff_src';                        
    end    

    Status = 'Uncontrolled exit.';
    
    % create a DB object and connect or use a preloaded object with connection
    if isempty(Args.DB)        
        DB          = db.Db;
        DB.Host     = Args.DbHost;
        DB.DbName   = Args.DbName;
        DB.User     = Args.DbUser;
        DB.Password = Args.DbPass;
        DB.Conn;    
    else
        DB = Args.DB;
    end
   
    TranCatLevel2 = TranCatLevel1;
    Rad2Arcsec = 206265;

    % Load filter flags
    BD_TF = BitDictionary('BitMask.TransientsFilter.Default');
    Flags = TranCatLevel2.getCol('FLAGS_TRANSIENT');
    
    % Filter out candidates that fail selected filters
    if ~isempty(Args.SubselectionFalse)

        Subselect = true(numel(Flags),1);
        
        NFlags = numel(Args.SubselectionFalse);

        for IFlags = 1:NFlags
            Subselect = Subselect & ~BD_TF.findBit(Flags,Args.SubselectionFalse{IFlags});
        end

        TranCatLevel2 = TranCatLevel2.selectRows(Subselect);
        Flags = TranCatLevel2.getCol('FLAGS_TRANSIENT');

    end

    NTran = TranCatLevel2.sizeCatalog;
    Reported = nan(NTran,1);
    TranCatLevel2 = TranCatLevel2.insertCol(Reported,inf,'Reported','');

    DB.useDB(Args.DbName);

    Npos = sum(Flags == 0);

    % If no candidates pass all filters, return.
    if Npos < 1
        Status = 'No passing candidates, returned.';
        return
    end

    % Sub-select passing candidates
    RealTranCands = TranCatLevel2.selectRows(Flags ==0);
    OrigRows = find(Flags == 0);

    % Match candidates to DB via cone search
    RA = RealTranCands.getCol('RA');
    Dec = RealTranCands.getCol('Dec');

    % Construct multi-epoch catalog for each passing candidate
    for Ipos = 1:1:Npos
        TC = ADc(Ipos).CatData;
        Object0 = TC.getCol('OBJECT');
        Mount0 = TC.getCol('MOUNT');
        Camera0 = TC.getCol('CAM');
        CropID0 = TC.getCol('CROPID');
        JD = TC.getCol('JD');

        ObjectStr = '';

        if isnumeric(Object0)
            ObjectStr = sprintf('%i',Object0);
        end

        ObjectParts = split(ObjectStr, '.');
        if numel(ObjectParts) > 1
            ObjectStr = ObjectParts{1};
        end

        MountStr = sprintf('%i',Mount0);
        CameraStr = sprintf('%i',Camera0);
        CropIDStr = sprintf('%i',CropID0);
        JDBack = JD - 60;
        JDBackStr = sprintf('%d',JDBack);

        SearchCMD = strcat("SELECT * FROM diff_src WHERE mountnum=",MountStr,...
            " AND camnum=",CameraStr," AND object=",ObjectStr,...
            " AND cropid=",CropIDStr," AND jd >",JDBackStr);

        TranDB = DB.query(SearchCMD);

        RA_DB = TranDB.ra;
        Dec_DB = TranDB.dec;

        Dists = celestial.coo.sphere_dist(RA_DB, Dec_DB,...
            RA(Ipos), Dec(Ipos), 'deg');

        Dists = Dists*Rad2Arcsec;

        MatchDB = TranDB(Dists < 3,:);

        ReportedMatch = MatchDB.report_jd;

        AlreadyReported = any((ReportedMatch>0) & ~isnan(ReportedMatch));
        if AlreadyReported
            ADc(Ipos).AlreadyReported = 1;
        end

        Score = ADc(Ipos).CatData.getCol('SCORE');
        PassingMatches = sum(MatchDB.flags_transient == 0) + 1;

        % This should be elsewhere probably
        if (PassingMatches > 1) || (Score >= 8.0)
            UTCNow = datetime('now', 'TimeZone', 'UTC');
            JDNow = juliandate(UTCNow);
            ADc(Ipos).CatData.replaceCol(JDNow, 'Reported');
            Reported(OrigRows(Ipos)) = JDNow;
        end

        MatchJD = MatchDB.jd;

        PhotMAG = double(TC.Table.MAG_PSF);
        PhotJD = double(TC.Table.JD);
        PhotMAGERR = double(TC.Table.MAGERR_PSF);
        PhotFLAGS = double(TC.Table.FLAGS_TRANSIENT);
        PhotSCORE = double(TC.Table.SCORE);

        if numel(MatchJD) > 0
            PhotMAG = double([PhotMAG; MatchDB.mag_psf]);
            PhotJD = double([PhotJD; MatchJD]);
            PhotMAGERR = double([PhotMAGERR; MatchDB.magerr_psf]);
            PhotFLAGS = double([PhotFLAGS; MatchDB.flags_transient]);
            PhotSCORE = double([PhotSCORE; MatchDB.score]);
        end
        PhotCatData = AstroCatalog({cast([PhotMAG(:), PhotMAGERR(:), PhotJD(:),...
            PhotFLAGS, PhotSCORE],'double')}, 'ColNames', ...
            {'MAG_PSF', 'MAGERR_PSF', 'JD', 'FLAGS_TRANSIENT', 'SCORE'});

        ADc(Ipos).PhotCatData = PhotCatData;

        RemoveRows = ismember(TranDB.jd, MatchJD);
        ULDB = TranDB;
        ULDB(RemoveRows,:) = [];
        ULJD = ULDB.jd;

        UniqueJDsUL = unique(ULJD);
        NuJD = numel(UniqueJDsUL);

        % If non-detections found, create an UL catalog of JDs and limiting
        % magnitudes, limiting magnitudes are taken as the new image
        % limiting magnitude
        if NuJD > 0
            % Get limiting magnitudes of non-detection
            MagsUL = ULDB.n_limmag;
            MagsULJDs = zeros(NuJD,1);

            % Initialize table for ULs
            variable_names_types = [["JD", "double"]; ...
                        ["MagUL", "double"]];
            ULTable = table('Size',[0,size(variable_names_types,1)],... 
                'VariableNames', variable_names_types(:,1),...
                'VariableTypes', variable_names_types(:,2));

            % Get UL for each unique epoch
            for IuJD = 1:NuJD
                MagsULJDs(IuJD) = MagsUL(find( ...
                    ULJD == UniqueJDsUL(IuJD),1));
                ULTable = [ULTable;{UniqueJDsUL(IuJD),MagsULJDs(IuJD)}];
            end

            % Save UL catalog as property of cutout
            ADc(Ipos).ULCatData = AstroCatalog(ULTable);
        end
    end

    TranCatLevel2.replaceCol(Reported,'Reported');
    Status = 'Succesful exit, transients matched to multi-epochs.';
    
end

%TODO: depreceated, maybe will still be useful?
%{
function [ADc, Status] = matchTransientsToOldCats(ADc, Args)
    %{
    Match transients to previous observations using older single-epoch catalogs.
    Input   : - AstroDiff cutouts on transients.
              * ...,key,val,...
                'BasePath' - Path under which telescope data can be found.
                       If empty, BasePath will be constructed assuming LAST
                       site infrastructure. Default is ''.
    Output  : - AstroDiff cutouts on transients updated with multi-epoch information.
    %}

    arguments
        ADc

        Args.BasePath = '';
    end

    % Get number of transient cutouts.
    Nadc = numel(ADc);

    % Run loop on each transient cutout
    for Iadc = 1:Nadc
        Transient = ADc(Iadc);

        % Get meta data
        RA = Transient.CatData.getCol('RA');
        Dec = Transient.CatData.getCol('Dec');
        JD = Transient.New.julday;
        DT = celestial.time.jd2date(JD,'H','YMD');
        Cam = Transient.CatData.getCol('CAM');

        % Find transient catalogs on the same field observed within one
        % month interval of transient detection.
        FN = FileNames.generateFromFileName(Transient.New.ImageData.FileName);

        FNzogy = FN.copy();
        FNzogy.Time = {'*.*.*'};
        FNzogy.Level = {'coadd.zogyD'};
        FNzogy.Product = {'Cat'};
        FNzogy.CropID = 0;
        MonthTransient = DT(2);
        YearTransient = DT(1);

        if MonthTransient == 1
            MonthBefore = 12;
            YearBefore = YearTransient - 1;
        else
            MonthBefore = MonthTransient-1;
            YearBefore = YearTransient;
        end

        if MonthTransient == 12
            MonthAfter = 1;
            YearAfter = YearTransient + 1;
        else
            MonthAfter = MonthTransient + 1;
            YearAfter = YearTransient;
        end

        if isempty(Args.BasePath)
            DataDir = strcat('data',num2str(2-mod(Cam,2)));
            Args.BasePath = strcat('/',tools.os.get_computer, ...
                '/',DataDir,'/archive');
        end

        SearchStringBefore = strcat(Args.BasePath,'/',FNzogy.ProjName, ...
            '/',num2str(YearBefore),'/',sprintf('%02.0f',MonthBefore), ...
            '/*/proc/*/',FNzogy.genFile);
        SearchStringTransient = strcat(Args.BasePath,'/',FNzogy.ProjName, ...
            '/',num2str(YearTransient),'/',sprintf('%02.0f',MonthTransient), ...
            '/*/proc/*/',FNzogy.genFile);
        SearchStringAfter = strcat(Args.BasePath,'/',FNzogy.ProjName, ...
            '/',num2str(YearAfter),'/',sprintf('%02.0f',MonthAfter), ...
            '/*/proc/*/',FNzogy.genFile);

        TranCats = AstroCatalog(SearchStringTransient{1});
        TranCatsBefore = AstroCatalog(SearchStringBefore{1});
        TranCatsAfter = AstroCatalog(SearchStringAfter{1});

        if numel(TranCatsBefore) > 1
            TranCats = [TranCatsBefore, TranCats];
        elseif numel(TranCatsBefore) == 1 && ~isempty(TranCatsBefore(1).Table)
            TranCats = [TranCatsBefore, TranCats];
        end

        if numel(TranCatsAfter) > 1
            TranCats = [TranCats, TranCatsAfter];
        elseif numel(TranCatsAfter) == 1 && ~isempty(TranCatsAfter(1).Table)
            TranCats = [TranCats, TranCatsAfter];
        end

        TC = merge(TranCats);
        Match = TC.coneSearch(RA, Dec, 3);

        MatchCat = TC.selectRows(Match.Ind);
        ADc(Iadc).CatData = MatchCat;
    end

    Status = 'Succesful exit, transients matched to multi-epochs.';
    
end
%}

% TODO: depreceated, delete at some point
%{
function [ADc, TranCatLevel2, Status] = matchTransientsToDBMat(ADc, TranCatLevel1, Args)
    %{
    Match transients to previous observations using a DB.
    Input   : - AstroDiff cutouts on transients.
              - AstroCat holding transient catalogs.
              * ...,key,val,...
                'SubselectionFalse' - Cell of filter names. All candidates
                       with TranCatLevel1 that failed these filters will be
                       removed before multi-epoch matching.
                       Default is {'BadPixelHard', 'StarMatch'}.
                'TranDB' - Name of the transients DB file. Default is ''.
    Output  : - AstroDiff cutouts on transients updated with multi-epoch information.  
    %}

    arguments
        ADc
        TranCatLevel1
        
        Args.SubselectionFalse = {'BadPixelHard', 'StarMatch', ...
            'LIMMAG', 'MPMatch', 'Negative'};
        Args.TranDB = '';
    end

    Status = 'Uncontrolled exit.';
       
    if isempty(Args.TranDB)
        Status = 'TranDB filename not given.'; 
        return
    end

    TranCatLevel2 = TranCatLevel1;
    NTran = TranCatLevel2.sizeCatalog;
    Reported = zeros(NTran,1);
    TranCatLevel2 = TranCatLevel2.insertCol(Reported,inf,'Reported','');

    % Load filter flags
    BD_TF = BitDictionary('BitMask.TransientsFilter.Default');
    Flags = TranCatLevel2.getCol('FLAGS_TRANSIENT');

    % Filter out candidates that fail selected filters
    if ~isempty(Args.SubselectionFalse)

        Subselect = true(numel(Flags),1);
        
        NFlags = numel(Args.SubselectionFalse);

        for IFlags = 1:NFlags
            Subselect = Subselect & ~BD_TF.findBit(Flags,Args.SubselectionFalse{IFlags});
        end

        TranCatLevel2 = TranCatLevel2.selectRows(Subselect);
        Flags = TranCatLevel2.getCol('FLAGS_TRANSIENT');

    end

    % Load DB file. If file does not exist, create it, save current
    % candidates and return.
    if ~isfile(Args.TranDB)
        TranDB = TranCatLevel2.copy();
        save(Args.TranDB,"TranDB");
        Status = 'TranDB did not exist, created for the first time.';
        return;
    else
        load(Args.TranDB, 'TranDB');
    end

    Npos = sum(Flags == 0);

    % Append current catalog to DB catalog and save.
    TranDB = merge([TranDB, TranCatLevel2]);

    % If no candidates pass all filters, return.
    if Npos < 1
        save(Args.TranDB,"TranDB");
        Status = 'No passing candidates, updated DB and returned.';
        return
    end

    % Sub-select passing candidates
    RealTranCands = TranCatLevel2.selectRows(Flags ==0);

    % Match candidates to DB via cone search
    RA = RealTranCands.getCol('RA');
    Dec = RealTranCands.getCol('Dec');

    ReportedDB = TranDB.Table.Reported;

    % Construct multi-epoch catalog for each passing candidate
    for Ipos = 1:1:Npos

        % Get all matches for a single candidate and define some variables
        % that define the observed field
        Matches = TranDB.coneSearch(RA(Ipos), Dec(Ipos), 3);
        MatchCat = TranDB.selectRows(Matches.Ind);  
        ReportedMatch = MatchCat.Table.Reported;

        AlreadyReported = any(ReportedMatch);
        if AlreadyReported
            ADc(Ipos).AlreadyReported = 1;
        end
        
        Score = ADc(Ipos).CatData.getCol('SCORE');

        % This should be elsewhere probably
        if (Matches.Nsrc > 1) || (Score(1) >= 8.0)
            ReportedDB(Matches.Ind) = 1;
        end        

        TC = ADc(Ipos).CatData;
        Field0 = TC.getCol('OBJECT');
        Mount0 = TC.getCol('MOUNT');
        Camera0 = TC.getCol('CAM');
        CropID0 = TC.getCol('CROPID');

        % Get the same variables for all candidates
        Fields = TranDB.getCol('OBJECT');
        Mounts = TranDB.getCol('MOUNT');
        Cameras = TranDB.getCol('CAM');
        CropIDs = TranDB.getCol('CROPID');

        % Sub-select candidates of the same field
        SameField = ((Fields == Field0) & (Mounts == Mount0) ...
            & (Cameras == Camera0) & (CropIDs == CropID0));
        TranDBsameField = TranDB.selectRows(SameField);

        % Get JDs of all same-field candidas, and all matched candidates
        JDs = TranDBsameField.getCol('JD');
        JD_Match = MatchCat.getCol('JD');

        % Remove matched candidates from same-field candidates via JD to
        % get times of same-field observations without a detection
        Detected = ismember(JDs,JD_Match);
        NonDetectionCat = TranDBsameField.selectRows(~Detected);

        % Get times of non-detections
        JDsUL = NonDetectionCat.getCol('JD');
        UniqueJDsUL = unique(JDsUL);

        NuJD = numel(UniqueJDsUL);

        % If non-detections found, create an UL catalog of JDs and limiting
        % magnitudes, limiting magnitudes are taken as the new image
        % limiting magnitude
        if NuJD > 0
            % Get limiting magnitudes of non-detection
            MagsUL = NonDetectionCat.getCol('N_LIMMAG');
            MagsULJDs = zeros(NuJD,1);

            % Initialize table for ULs
            variable_names_types = [["JD", "double"]; ...
                        ["MagUL", "double"]];
            ULTable = table('Size',[0,size(variable_names_types,1)],... 
                'VariableNames', variable_names_types(:,1),...
                'VariableTypes', variable_names_types(:,2));

            % Get UL for each unique epoch
            for IuJD = 1:NuJD
                MagsULJDs(IuJD) = MagsUL(find( ...
                    JDsUL == UniqueJDsUL(IuJD),1));
                ULTable = [ULTable;{UniqueJDsUL(IuJD),MagsULJDs(IuJD)}];
            end

            % Save UL catalog as property of cutout
            ADc(Ipos).ULCatData = AstroCatalog(ULTable);

        end

        % Save multi-epoch matched catalog by updating cutout catalog
        ADc(Ipos).CatData = MatchCat;
    end

    TranDB = TranDB.replaceCol(ReportedDB,'Reported');
    save(Args.TranDB,"TranDB");
    Status = 'Succesful exit, transients matched to multi-epochs.';

end
%}