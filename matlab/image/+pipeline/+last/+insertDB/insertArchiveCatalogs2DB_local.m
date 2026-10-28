function [Result] = insertArchiveCatalogs2DB_local(RootDir, FileNameTemplate, Args)
    % insert archived proc LAST catalogs via Object Matcher local filedrop
    %     post-processing twin of insertArchiveCatalogs2DB (no direct ClickHouse, no HTTP)
    % Input  : - root directory from where to inject the data
    %          - template of the data file name
    %          * ...,key,val,...
    %        'ProcDirTemplate' - template of dir name containing proc results
    %        'ProcDirList'     - optional explicit list of proc dirs (overrides template)
    %        'Template'        - XLS column map (Sheet Sources, TableName proc_src)
    %        'DataDir'         - OM incoming dir (empty = OM_DATA_DIR env)
    %        'JobsDir'         - OM jobs root (empty = OM_JOBS_DIR env)
    %        'Domain'          - service domain (default 'last')
    %        'DetectionTable'  - qualified detection table (default 'last.proc_src')
    %        'RemoteUser'      - user for appending .status on archive host
    %
    % Output : - catalog rows submitted via SourcesClientLocal (fire-and-forget)
    % See also: insertArchiveCatalogs2DB, db.sources.SourcesClientLocal
    %
    % Example:
    %   pipeline.last.insertDB.insertArchiveCatalogs2DB_local('/mnt/marvin/LAST.01.01.01/2023/04/24/', ...
    %       'ProcDirTemplate', '/proc/*');

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
        Args.ColNameID         = 'id_proc_src';

        Args.RemoteUser        = 'euclid';

        Args.DataDir           = '';
        Args.JobsDir           = '';
        Args.Domain            = 'last';
        Args.DetectionTable    = 'last.proc_src';
        Args.Family            = 'proc';
        Args.Detection         = 'proc';
        Args.Verbose           = false;
        Args.Perf              = false;
    end

    Columns = db.util.read_xls2tableFormat(Args.Template, 'Sheet', 'Sources', ...
        'TableName', Args.DbTable);

    Client = db.sources.SourcesClientLocal('DataDir', Args.DataDir, 'JobsDir', Args.JobsDir);
    Client.Verbose = Args.Verbose;
    Client.Perf = Args.Perf;

    Dir = pwd;
    FIDnostatus   = fopen('cat_no_status_dir.txt', 'a');
    FIDnodata     = fopen('cat_no_data_dir.txt', 'a');
    FIDbrokendata = fopen('cat_broken_data_dir.txt', 'a');

    if isempty(Args.ProcDirList)
        D = dir(fullfile(RootDir, Args.ProcDirTemplate));
        Dirs = D([D.isdir]);
        Dirs = Dirs(~ismember({Dirs.name}, {'.', '..'}));
        Dirs = Dirs(contains({Dirs.name}, 'v0'));
        Dirs = Dirs(~contains({Dirs.folder}, 're'));
    else
        Dirs = Args.ProcDirList;
    end

    Ndir = numel(Dirs);
    for Idir = 1:Ndir
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
                | contains(StatusText, "injected into the proc catalog DB via SourcesClientLocal") ...
                | contains(StatusText, "not injectable into the proc catalog DB due to broken data files");
        catch
            cd(Dir);
            fprintf(FIDnostatus, '%s \n', DataDir);
            continue
        end

        if ~Injected
            if Args.Decompress
                system(sprintf('su %s -c "bunzip2 %s.bz2"', Args.RemoteUser, FileNameTemplate));
                system(sprintf('su %s -c "xz -d %s.xz"', Args.RemoteUser, FileNameTemplate));
            end

            try
                Cat = AstroCatalog(FileNameTemplate, 'UseMex', true);
                AH  = AstroHeader(FileNameTemplate, 3, 'UseMex', true);
            catch
                cd(Dir);
                fprintf(FIDbrokendata, '%s \n', DataDir);
                continue
            end
            cd(Dir);

            if numel(Cat) < 2
                fprintf(FIDnodata, '%s \n', DataDir);
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

            fprintf('Submitting via SourcesClientLocal from %s ..', DataDir);
            Tic = tic;

            Pname = AH(1).getVal('PROJNAME');
            if isnan(AH(1).getVal('NODENUMB'))
                NODENUMB = str2num(Pname(6:7));
                for Crop = 1:Nobj
                    AH(Crop).replaceVal('NODENUMB', NODENUMB);
                end
            end
            if isnan(AH(1).getVal('MOUNTNUM'))
                MOUNTNUM = str2num(Pname(9:10));
                for Crop = 1:Nobj
                    AH(Crop).replaceVal('MOUNTNUM', MOUNTNUM);
                end
            end
            Subdir = AH(1).getVal('SUBDIR');
            if isempty(Subdir)
                Parts  = strsplit(DataDir, '/');
                Subdir = Parts{end};
                for Crop = 1:Nobj
                    AH(Crop).replaceVal('SUBDIR', Subdir);
                end
            end
            JDnow = celestial.time.date2jd;
            for Crop = 1:Nobj
                AH(Crop).replaceVal('INGESTION_TIME_JD', JDnow);
            end

            [T, Error] = imProc.db.insertCatalog(Cat, 'Header', AH, 'ColNameDic', Columns, ...
                'DbTable', [], 'CreateCsv', false);
            if ~isempty(Error)
                error('insertArchiveCatalogs2DB_local:catalogBuildFailed', '%s', Error);
            end
            if isempty(T) || height(T) < 1
                fprintf(FIDnodata, '%s \n', DataDir);
                continue
            end

            RequestId = sprintf('last-%s-%s', Args.DbTable, regexprep(char(Subdir), '[^a-zA-Z0-9._-]', '_'));
            SubmitArgs = {'RequestId', RequestId, 'Domain', Args.Domain, ...
                'Family', Args.Family, 'Detection', Args.Detection, ...
                'DetectionTable', Args.DetectionTable, ...
                'OriginalName', sprintf('%s_%s.parquet', Args.DbTable, char(Subdir))};

            try
                Resp = Client.insertSources(T, SubmitArgs{:});
            catch ME
                error('insertArchiveCatalogs2DB_local:submitFailed', ...
                    'SourcesClientLocal submit failed for %s: %s', DataDir, ME.message);
            end

            if Args.UpdateStatus
                UpdateStatus = sprintf(['su %s -c "echo ''%s injected into the proc catalog DB via SourcesClientLocal'' >> %s/.status"'], ...
                    Args.RemoteUser, tools.timeStamp.getTimeStamp, DataDir);
                system(UpdateStatus);
            end
            fprintf(' ..done in %.1f s (%d rows, job_id=%s)\n', toc(Tic), height(T), Resp.job_id);

            if Args.CompressProcessed
                system(sprintf('su %s -c "bzip2 %s/%s"', Args.RemoteUser, DataDir, FileNameTemplate));
            end
        else
            cd(Dir);
        end
    end

    fclose(FIDnostatus);
    fclose(FIDnodata);
    fclose(FIDbrokendata);
    fprintf('SourcesClientLocal ingestion completed.\n');
end
