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
                'LookBackJD' - Number of days back to look for multi-epoch
                       matches. Default is 60.
                'SearchRad' - Cone search radius in which to match
                       multi-epoch candidates, in arcsec. Default is 3.
                'Template' - Template of the table structure. Default is 
                       '~/matlab/data/db/Design-Database-Pipeline-ClickHouse.xlsx'.
                'DB' - A db.Db object with connection open. Default is empty.
                'DbHost' - Database host, DB parameter. Default is 'last0'.
                'DbName' - Database name, DB parameter. Default is 'last'.
                'DbUser' - Database user, DB parameter. Default is 'default'.
                'DbPass' - Database password, DB parameter. Default is ''.
    Output  : - AstroDiff cutouts on transients updated with multi-epoch information.
              - AstroCatalog filtered for subselection criteria and with
                additional multi-epoch information.
              - Printout summarizing the return status.
    Author  : Ruslan Konno (Oct 2024)
    Example : VisitPath = '/path/to/visit/dir'
              [AD, ADc, TCL1, Status] = pipeline.last.transients.runTransientsPipe(VisitPath)
              [ADc, TCL2, Status] = pipeline.last.transients.matchTransientsToMultiEpochs(ADc, TCL1)
    %}

    arguments
        ADc
        TranCatLevel1

        Args.SubselectionFalse = {'BadPixelHard', 'LIMMAG', 'Negative', ...
            'Overdensity', 'PVDist', 'Streak', 'NPSFShape'};
        Args.LookBackJD = 60;
        Args.SearchRad = 3;
        Args.MinTimeDiffMinutes = 1;

        Args.Template = '~/matlab/data/db/Design-Database-Pipeline-ClickHouse.xlsx';

        Args.DB = [];
        Args.DbHost = 'last0';
        Args.DbName = 'last';   
        Args.DbUser = 'default';
        Args.DbPass = ''; 
    end
    
    Status = 'Uncontrolled exit.';

    TranCatLevel2 = [];

    % Return if catalog is empty
    if TranCatLevel1.sizeCatalog < 1
        Status = 'Transients catalog empty.';
        return
    end

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
    DaysToMins = 24*60;

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

    % Get number of candidates after filtering
    NTran = TranCatLevel2.sizeCatalog;
    % Mark all as not yet reported
    Reported = nan(NTran,1);
    TranCatLevel2 = TranCatLevel2.insertCol(Reported,inf,'Reported','');

    DB.useDB(Args.DbName);

    % Get number of positive candidates
    Npos = sum(Flags == 0);

    % If no positive candidates, return.
    if Npos < 1
        Status = 'No passing candidates, returned.';
        return
    end

    % Sub-select passing candidates
    RealTranCands = TranCatLevel2.selectRows(Flags ==0);
    % Remember index of positive candidates in catalog
    OrigRows = find(Flags == 0);

    % Match candidates to DB via cone search
    % Remember RADec of positive candidates in catalog
    RAInCat = RealTranCands.getCol('RA');
    DecInCat = RealTranCands.getCol('Dec');

    DBQueryFails = 0;

    % Construct multi-epoch catalog for each passing candidate
    % loop over cutout candidates
    for Ipos = 1:1:Npos
        % Retrieve sub-image data
        TC = ADc(Ipos).CatData;
        Object0 = TC.getCol('OBJECT');
        Mount0 = TC.getCol('MOUNT');
        Camera0 = TC.getCol('CAM');
        CropID0 = TC.getCol('CROPID');
        JD = TC.getCol('JD');

        % Get RADec of cutout candidate and find its index in the catalog
        RATran = TC.getCol('RA');
        DecTran = TC.getCol('Dec');
        OrigRow = OrigRows(RAInCat == RATran & DecInCat == DecInCat);

        % Get the field ID of the candidate. Strip the dot extension if
        % there is one.
        ObjectStr = '';

        if isnumeric(Object0)
            ObjectStr = sprintf('%i',Object0);
        end

        ObjectParts = split(ObjectStr, '.');
        if numel(ObjectParts) > 1
            ObjectStr = ObjectParts{1};
        end

        % Convert to char for DB query
        MountStr = sprintf('%i',Mount0);
        CameraStr = sprintf('%i',Camera0);
        CropIDStr = sprintf('%i',CropID0);

        % Look back a number of days
        JDBack = JD - Args.LookBackJD;
        JDBackStr = sprintf('%d',JDBack);

        % DB query for candidates in the same field sub-image
        SearchCMD = strcat("SELECT * FROM diff_src WHERE mountnum=",MountStr,...
            " AND camnum=",CameraStr," AND object=",ObjectStr,...
            " AND cropid=",CropIDStr," AND jd >",JDBackStr);
        TranDB = DB.query(SearchCMD);

        if ~isempty(TranDB)
            % Get RADec of found candidates and match to current candidate via
            % cone search
            RA_DB = TranDB.ra;
            Dec_DB = TranDB.dec;
            Dists = celestial.coo.sphere_dist(RA_DB, Dec_DB,...
                RATran, DecTran, 'deg');
            Dists = Dists*Rad2Arcsec;
            MatchDB = TranDB(Dists < Args.SearchRad,:);
    
            % Make sure we don't have duped entries by comparing the JDs
            TimeThr = Args.MinTimeDiffMinutes*DaysToMins;
            JdDiff0 = abs(MatchDB.jd - JD);
            MatchDB = MatchDB(JdDiff0 > TimeThr,:);
    
            if size(MatchDB,1)>1
                MatchJDs = MatchDB.jd;
                NumMatches = numel(MatchJDs);
                KeepMask = false(size(MatchJDs));
    
                for IMatch = 1:NumMatches
                    MatchJD = MatchJDs(IMatch);
    
                    % Check previous kept values
                    PrevKept = MatchJDs(KeepMask);
                    TooClose = abs(PrevKept - MatchJD) <= TimeThr;
    
                    if any(TooClose)
                        % Don't keep too close ones.
                        KeepMask(IMatch) = false;
                    else
                        KeepMask(IMatch) = true;
                    end
                end
                
                MatchDB = MatchDB(KeepMask,:);
            end

            MatchJDs = MatchDB.jd;
    
            % Get JD of report of DB matches.
            ReportedMatch = MatchDB.report_jd;
    
            % See if the candidate was already reported. If yes, set current
            % candidate as already reported.
            AlreadyReported = any((ReportedMatch>0) & ~isnan(ReportedMatch));
            if AlreadyReported
                ADc(Ipos).AlreadyReported = 1;
            end
    
            PassingMatches = sum(MatchDB.flags_transient == 0) + 1;
        else
            MatchJDs = [];
            PassingMatches = 1;
            DBQueryFails = DBQueryFails + 1;
        end
        % See if this candidate is worth reporting. If yes, set its report
        % jd to now.
        % TODO: Currently it is the easiest way to do it here, but it
        % should probably move elsewhere in the future.
        Score = ADc(Ipos).CatData.getCol('SCORE');

        if (PassingMatches > 1) || (Score >= 7.7)
            UTCNow = datetime('now', 'TimeZone', 'UTC');
            JDNow = juliandate(UTCNow);
            ADc(Ipos).CatData.replaceCol(JDNow, 'Reported');
            Reported(OrigRow) = JDNow;
        end

        % Fill the candidates photometry catalog with all matched
        % multi-epoch candidates.
        PhotMAG = double(TC.Table.MAG_PSF);
        PhotJD = double(TC.Table.JD);
        PhotMAGERR = double(TC.Table.MAGERR_PSF);
        PhotFLAGS = double(TC.Table.FLAGS_TRANSIENT);
        PhotSCORE = double(TC.Table.SCORE);

        if numel(MatchJDs) > 0
            PhotMAG = double([PhotMAG; MatchDB.mag_psf]);
            PhotJD = double([PhotJD; MatchJDs]);
            PhotMAGERR = double([PhotMAGERR; MatchDB.magerr_psf]);
            PhotFLAGS = double([PhotFLAGS; MatchDB.flags_transient]);
            PhotSCORE = double([PhotSCORE; MatchDB.score]);
        end
        PhotCatData = AstroCatalog({cast([PhotMAG(:), PhotMAGERR(:), PhotJD(:),...
            PhotFLAGS, PhotSCORE],'double')}, 'ColNames', ...
            {'MAG_PSF', 'MAGERR_PSF', 'JD', 'FLAGS_TRANSIENT', 'SCORE'});

        ADc(Ipos).PhotCatData = PhotCatData;

        % Now we look for non-detections
        % Get the JDs of all matched multi-epoch candidates and remove them
        % from the table returned by the DB query. All remaining JDs are
        % times at which there was no multi-epoch match, we will get ULs
        % for these times.
        if ~isempty(TranDB)
            RemoveRows = ismember(TranDB.jd, MatchJDs);
            ULDB = TranDB;
            ULDB(RemoveRows,:) = [];
            ULJD = ULDB.jd;
    
            % For the list of JDs without a match, keep only the unique JDs.
            UniqueJDsUL = unique(ULJD);
            NuJD = numel(UniqueJDsUL);
        else
            NuJD = 0;
        end

        % If non-detections found, create an UL catalog of JDs and limiting
        % magnitudes, limiting magnitudes are taken as the new image
        % limiting magnitude
        if NuJD > 0
            % Get limiting magnitudes of non-detection
            MagsUL = ULDB.n_limmag;
            MagsULJDs = zeros(NuJD,1);

            % Initialize table for ULs
            ULTable = table(zeros(NuJD,1),zeros(NuJD,1),...
                'VariableNames', ["JD","MagUL"]);

            % Get UL for each unique epoch and save to UL table
            for IuJD = 1:NuJD
                MagsULJDs(IuJD) = MagsUL(find( ...
                    ULJD == UniqueJDsUL(IuJD),1));
                ULTable.JD(IuJD) = UniqueJDsUL(IuJD);
                ULTable.MagUL(IuJD) = MagsULJDs(IuJD);
            end

            % Save UL catalog as property of cutout
            ADc(Ipos).ULCatData = AstroCatalog(ULTable);
        end
    end

    % Update report column of catalog
    TranCatLevel2.replaceCol(Reported,'Reported');
    % Return with succesful status
    Status = 'Succesful exit, transients matched to multi-epochs.';

    if DBQueryFails > 0
        Status = sprintf('Exited, but DB query failed %i out of %i times',...
            DBQueryFails, Npos);
    end
    
end
