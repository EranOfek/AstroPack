function insertArchiveCatalogs2DB_local_perf(RootDir, FileNameTemplate, Args)
    % Performance twin of insertArchiveCatalogs2DB_local (Object Matcher filedrop).
    % Logs visit timings via pipeline.last.insertDB.perf; client splits in resp.timing.

    arguments
        RootDir                = '/mnt/marvin/LAST.01*/';
        FileNameTemplate       = 'LAST*proc_Cat_1.fits*';
        Args.ProcDirTemplate   = '/proc/*';
        Args.ProcDirList       = [];
        Args.Decompress        = false;
        Args.CompressProcessed = false;
        Args.UpdateStatus      = true;
        Args.Template          = '~/matlab/data/db/Design-Database-Pipeline-ClickHouse.xlsx';
        Args.Level             = 'proc';
        Args.DbTable           = 'proc_src';
        Args.RemoteUser        = 'euclid';
        Args.DataDir           = '';
        Args.JobsDir           = '';
        Args.Domain            = 'last';
        Args.DetectionTable    = 'last.proc_src';
        Args.Family            = 'proc';
        Args.Detection         = 'proc';
        Args.Verbose           = false;
    end

    FnName = 'insertArchiveCatalogs2DB_local_perf';
    RunT0 = tic;
    pipeline.last.insertDB.perf.perfLogRunBanner(FnName);
    pipeline.last.insertDB.perf.perfLog(sprintf('%s START RootDir=%s', FnName, RootDir));

    Columns = db.util.read_xls2tableFormat(Args.Template, 'Sheet', 'Sources', ...
        'TableName', Args.DbTable);
    Client = db.sources.SourcesClientLocal('DataDir', Args.DataDir, 'JobsDir', Args.JobsDir);
    Client.Verbose = Args.Verbose;
    Client.Perf = true;

    VisitStats = pipeline.last.insertDB.perf.perfVisitStats('init');
    TotalRows = 0;
    CntInjected = 0;

    Dir = pwd;
    if isempty(Args.ProcDirList)
        D = dir(fullfile(RootDir, Args.ProcDirTemplate));
        Dirs = D([D.isdir]);
        Dirs = Dirs(~ismember({Dirs.name}, {'.', '..'}));
        Dirs = Dirs(contains({Dirs.name}, 'v0'));
        Dirs = Dirs(~contains({Dirs.folder}, 're'));
    else
        Dirs = Args.ProcDirList;
    end

    for Idir = 1:numel(Dirs)
        VisitT0 = tic;
        if isempty(Args.ProcDirList)
            DataDir = strcat(Dirs(Idir).folder, '/', Dirs(Idir).name);
        else
            DataDir = Dirs(Idir);
            if iscell(DataDir)
                DataDir = DataDir{1};
            end
        end
        cd(DataDir);
        try
            StatusText = fileread('.status');
            Injected = contains(StatusText, "injected into the proc catalog DB") ...
                | contains(StatusText, "injected into the proc catalog DB via SourcesClient") ...
                | contains(StatusText, "injected into the proc catalog DB via SourcesClientLocal");
        catch
            cd(Dir);
            continue
        end
        if Injected
            cd(Dir);
            continue
        end

        CatT0 = tic;
        try
            Cat = AstroCatalog(FileNameTemplate, 'UseMex', true);
            AH  = AstroHeader(FileNameTemplate, 3, 'UseMex', true);
        catch
            cd(Dir);
            continue
        end
        CatSec = toc(CatT0);
        cd(Dir);
        if numel(Cat) < 2
            continue
        end

        NCol = max(arrayfun(@(x) size(x.Catalog, 2), Cat));
        Idx  = arrayfun(@(x) size(x.Catalog, 2) < NCol, Cat);
        if Idx(1)
            continue
        end
        Cat(Idx) = [];
        Nobj = numel(Cat);
        for Iobj = 1:Nobj
            if ~Cat(Iobj).isColumn('JD')
                JD   = AH(Iobj).getVal('JD');
                Nrow = repmat(1, height(Cat(Iobj).Table), 1);
                insertCol(Cat(Iobj), JD .* Nrow, Inf, 'JD', '');
            end
        end

        KeyT0 = tic;
        Subdir = AH(1).getVal('SUBDIR');
        if isempty(Subdir)
            Parts  = strsplit(DataDir, '/');
            Subdir = Parts{end};
        end
        JDnow = celestial.time.date2jd;
        for Crop = 1:Nobj
            AH(Crop).replaceVal('INGESTION_TIME_JD', JDnow);
        end
        KeySec = toc(KeyT0);

        BuildT0 = tic;
        [T, Error] = imProc.db.insertCatalog(Cat, 'Header', AH, 'ColNameDic', Columns, ...
            'DbTable', [], 'CreateCsv', false);
        BuildSec = toc(BuildT0);
        if ~isempty(Error) || isempty(T) || height(T) < 1
            continue
        end

        RequestId = sprintf('last-%s-%s', Args.DbTable, regexprep(char(Subdir), '[^a-zA-Z0-9._-]', '_'));
        Resp = Client.insertSources(T, 'RequestId', RequestId, 'Domain', Args.Domain, ...
            'Family', Args.Family, 'Detection', Args.Detection, ...
            'DetectionTable', Args.DetectionTable, ...
            'OriginalName', sprintf('%s_%s.parquet', Args.DbTable, char(Subdir)));

        StatusSec = 0;
        if Args.UpdateStatus
            StT0 = tic;
            UpdateStatus = sprintf(['su %s -c "echo ''%s injected into the proc catalog DB via SourcesClientLocal'' >> %s/.status"'], ...
                Args.RemoteUser, tools.timeStamp.getTimeStamp, DataDir);
            system(UpdateStatus);
            StatusSec = toc(StT0);
        end

        TotalSec = toc(VisitT0);
        FiledropSec = Resp.timing.total_ms / 1000.0;
        VisitStats = pipeline.last.insertDB.perf.perfVisitStats('endVisit', VisitStats, TotalSec, FiledropSec, 'injected');
        TotalRows = TotalRows + height(T);
        CntInjected = CntInjected + 1;

        tm = Resp.timing;
        pipeline.last.insertDB.perf.perfLog(sprintf('%s VISIT %s readAstroCatalog %.2fs keywordPrep %.2fs buildTable %.2fs rows=%d', ...
            FnName, DataDir, CatSec, KeySec, BuildSec, height(T)));
        pipeline.last.insertDB.perf.perfLog(sprintf('%s VISIT %s filedrop prepare_ms=%.0f write_ms=%.0f handoff_ms=%.0f bytes=%d total_ms=%.0f updateStatus %.2fs', ...
            FnName, DataDir, tm.prepare_ms, tm.write_ms, tm.handoff_ms, tm.bytes, tm.total_ms, StatusSec));
    end

    pipeline.last.insertDB.perf.perfVisitStats('logSummary', VisitStats, FnName);
    pipeline.last.insertDB.perf.perfLog(sprintf('%s END visits=%d rows=%d elapsed=%.1fs', ...
        FnName, CntInjected, TotalRows, toc(RunT0)));
end
