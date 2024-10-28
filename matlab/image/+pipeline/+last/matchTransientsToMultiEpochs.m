function [ADc, Status] = matchTransientsToMultiEpochs(ADc, TranCatLevel1, Args)
    %{
    Match transients to previous observations.
    Input   : - AstroDiff cutouts on transients.
              - AstroCat holding transient catalogs.
              * ...,key,val,...
                'SubselectionFalse' - Cell of filter names. All candidates
                       with TranCatLevel1 that failed these filters will be
                       removed before multi-epoch matching.
                       Default is {'BadPixelHard', 'StarMatch'}.
                'useDB' - Bool on wether to use the DB for multi-epoch
                       matching. When false, matching is done by searching 
                       for single epoch catalogs. Default is false.
                       NOTE: The DB right now just a file.
                'TranDB' - Name of the transients DB file. Used when useDB 
                       is true. Default is ''.
                'BasePath' - Path under which telescope data can be found.
                       If empty, BasePath will be constructed assuming LAST
                       site infrastructure. Used when useDB is false. 
                       Default is ''.
    Output  : - AstroDiff cutouts on transients updated with multi-epoch information.
    Author  : Ruslan Konno (Oct 2024)
    Example : VisitPath = '/path/to/visit/dir'
              [AD, ADc, TCL1, Status] = runTransientsPipe(VisitPath)
              ADc = matchTransientsToMultiEpochs(ADc, TCL1)
    %}

    arguments
        ADc
        TranCatLevel1

        Args.SubselectionFalse = {'BadPixelHard', 'StarMatch'};
        Args.useDB logical = false;
        Args.TranDB = '';

        Args.BasePath = '';
    end
    
    Status = 'Uncontrolled exit.';

    % Match to DB if Args.useDB is true, otherwise look for old single
    % epoch catalogs.
    if Args.useDB
        [ADc, Status] = matchTransientsToDB(ADc, TranCatLevel1 ...
            , 'SubselectionFalse', Args.SubselectionFalse, 'TranDB', Args.TranDB);
    else
        ADc = matchTransientsToOldCats(ADc, TranCatLevel1, ...
            'BasePath', Args.BasePath);
    end

end

function [ADc, Status] = matchTransientsToDB(ADc, TranCatLevel1, Args)
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
        
        Args.SubselectionFalse = {'BadPixelHard', 'StarMatch'};
        Args.TranDB = '';
    end

    Status = 'Uncontrolled exit.';
    

    TranCatLevel2 = TranCatLevel1;
    
    if isempty(Args.TranDB)
        Status = 'TranDB filename not given.'; 
        return
    end

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
    save(Args.TranDB,"TranDB");

    % If no candidates pass all filters, return.
    if Npos < 1
        Status = 'No passing candidates, updated DB and returned.';
        return
    end

    % Sub-select passing candidates
    RealTranCands = TranCatLevel2.selectRows(Flags ==0);

    % Match candidates to DB via cone search
    RA = RealTranCands.getCol('RA');
    Dec = RealTranCands.getCol('Dec');
    Matches = TranDB.coneSearch(RA, Dec, 3);

    % Construct multi-epoch catalog for each passing candidate
    for Ipos = 1:1:Npos
        % Get all matches for a single candidate and define some variables
        % that define the observed field
        MatchCat = TranDB.selectRows(Matches(Ipos).Ind);
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

    Status = 'Succesful exit, transients matched to multi-epochs.';

end

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