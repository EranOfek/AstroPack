function [Result] = insertArchiveCatalogs2DB_perf(RootDir, FileNameTemplate, Args)
    % insert the archived LAST catalogs to DB 
    %     this is post-processing, not intended to be used in real time within the pipeline 
    % Input  : - root directory from where to inject the data
    %          - template of the data file name
    %          * ...,key,val,... 
    %        'ProcDirTemplate' - template of dir name containing the results of data reduction 
    %        'Template'        - template of tables' structure
    %        'Db*'             - database parameters
    %        'DbTable'         - DB table name
    %        'ColNameID'       - column name for the file unique ID
    %        'RemoteUser'      - the name of the user who has a permission to write into the archive folders
    %                           
    % Output : - data injected into the DB
    % Author : A.M. Krassilchtchikov (2024 Nov) 
    % Example: RootDir = '/Data1/LAST.01.01.01/'; 
    %          Template = '*coadd*Ima*fits';
    %          pipeline.last.insertDB.insertArchiveCatalogs2DB(RootDir,Template)    
    %    
    %          pipeline.last.insertDB.insertArchiveCatalogs2DB('/mnt/marvin/LAST.01.01.01/2023/04/24/','ProcDirTemplate','/proc/*')
    %          pipeline.last.insertDB.insertArchiveCatalogs2DB('/mnt/marvin/','ProcDirTemplate','LAST.01.02*/*/*/*/proc/*')
    %
    arguments
        RootDir                = '/mnt/marvin/LAST.01*/';
        FileNameTemplate       = 'LAST*proc_Cat_1.fits*';      
        Args.ProcDirTemplate   = '/proc/*';  
        Args.ProcDirList       = [];
        Args.Decompress        = false;
        Args.CompressProcessed = false;
        Args.Template          = '~/matlab/data/db/Design-Database-Pipeline-ClickHouse.xlsx';
        Args.DbHost            = 'euclid';
        Args.DbName            = 'last';   
        Args.DbUser            = 'default';  
        Args.DbPort            =  9000;
        Args.AstroDBPassFile   = '~/.astropack/Passwords.yml';
        Args.Level             = 'proc';
        Args.DbTable           = 'proc_src';
        Args.DbPassKey         = '';
        Args.ColMapTableName   = '';
        Args.ColNameID         = 'id_proc_src';
        Args.RemoteUser        = 'euclid';
        Args.DBConnector       = 'native'; % 'legacy'; % 'native' or 'legacy'
        Args.ConnectorOpts     = struct('compression', db.mex.Compression.ZSTD);
        Args.Schema            = [];       % the user may input the schema of the DB table 
                                           % as a matlab table output of the "DESCRIBE TABLE" SQL command
        Args.SkipInsert        = false;
        Args.UpdateStatus      = true;
        Args.IgnoreStatus      = false;
        Args.CatalogFileInventory(1,1) logical = false;
        Args.CatalogRawFileRead(1,1) logical   = false;
        Args.CatalogAstroPerf(1,1) logical     = false;
        Args.SkipDbInsert(1,1) logical         = false;
        Args.UseInsertCatalogPerf(1,1) logical   = true;
        Args.UseZipCatalogs(1,1) logical       = false;
        Args.ZipDir (1,:) char                 = '';
        Args.ExtractRoot (1,:) char            = '';
    end    
    FnName = 'insertArchiveCatalogs2DB_perf';
    RunT0 = tic;
    pipeline.last.insertDB.perfLogRunBanner(FnName);
    perfLog(sprintf('%s START RootDir=%s SkipInsert=%d SkipDbInsert=%d UpdateStatus=%d IgnoreStatus=%d UseZipCatalogs=%d CatalogFileInventory=%d CatalogRawFileRead=%d CatalogAstroPerf=%d', ...
        FnName, RootDir, Args.SkipInsert, Args.SkipDbInsert, Args.UpdateStatus, Args.IgnoreStatus, Args.UseZipCatalogs, ...
        Args.CatalogFileInventory, Args.CatalogRawFileRead, Args.CatalogAstroPerf));
    CntSkippedInjected = 0;
    CntNoStatus = 0;
    CntNoData = 0;
    CntBroken = 0;
    CntInserted = 0;
    CntSkipInsert = 0;
    VisitStats = pipeline.last.insertDB.perfVisitStats('init');
    InsertStats = perfInsertStatsInit_();
    CatalogReadStats = perfCatalogReadStatsInit_();
    CatalogAstroStats = perfCatalogAstroStatsInit_();
    % create a DB object and connect   
    TicDb = tic;
    Configuration.getSingleton().loadFile(Args.AstroDBPassFile); % tell the PM where to look for passwords
    PM  = PasswordsManager;    
    if ~isempty(Args.DbPassKey)
        Pwd = PM.search(Args.DbPassKey).Pass;
    else
        Pwd = PM.search(Args.DbName).Pass;
    end
    if strcmpi(Args.DBConnector,'legacy')
        DB          = db.Db;
        DB.Host     = Args.DbHost;
        DB.DbName   = Args.DbName;
        DB.User     = Args.DbUser;
        DB.Password = Pwd;
        DB.Conn;
        DB.useDB(Args.DbName);
        fprintf('DB in use: %s\n',DB.showCurrentDB);
    elseif strcmpi(Args.DBConnector,'native')        
        DB = db.mex.ClickHouseClient(Args.DbHost, Args.DbPort, Args.DbUser, Pwd, Args.ConnectorOpts);
        DB.query(sprintf('use %s',Args.DbName));
        if isempty(Args.Schema)
            Args.Schema = DB.describe(Args.DbTable);
        end
    else
        error('Asked for unknown DB connector')
    end
    perfLog(sprintf('%s dbConnect DbName=%s DbTable=%s', FnName, Args.DbName, Args.DbTable), toc(TicDb));
%     fprintf('Table list: '); fprintf('%s ',DB.showTables{:}); fprintf('\n');        
    % read the column list from the xls template
    TicXls = tic;
    TableNameForXls = Args.DbTable;
    if ~isempty(Args.ColMapTableName)
        TableNameForXls = Args.ColMapTableName;
    end
    Columns = db.util.read_xls2tableFormat(Args.Template,'Sheet','Sources','TableName',TableNameForXls);
    perfLog(sprintf('%s readXlsTemplate', FnName), toc(TicXls));
    %
    Dir = pwd; 
    FIDnostatus     = fopen('cat_no_status_dir.txt', 'a');
    FIDnodata       = fopen('cat_no_data_dir.txt', 'a'); 
    FIDbrokendata   = fopen('cat_broken_data_dir.txt', 'a');
    % find all the directories according to the template or read from Args
    TicDir = tic;
    if isempty(Args.ProcDirList)
        D = dir(fullfile(RootDir, Args.ProcDirTemplate));
        Dirs = D([D.isdir]);
        Dirs = Dirs(~ismember({Dirs.name}, {'.', '..'}));
        Dirs = Dirs(contains({Dirs.name}, 'v0'));
        Dirs = Dirs(~contains({Dirs.folder},'re'));
    else
        Dirs = Args.ProcDirList;
    end
    Ndir = numel(Dirs);
    perfLog(sprintf('%s dirDiscovery Ndir=%d', FnName, Ndir), toc(TicDir));
    % 
    for Idir = 1:Ndir
        if isempty(Args.ProcDirList)
            DataDir = strcat(Dirs(Idir).folder,'/',Dirs(Idir).name);
        else
            DataDir = Dirs(Idir);
            if iscell(DataDir)
                DataDir = DataDir{1};
            end
            if isstring(DataDir)
                DataDir = char(DataDir);
            end
        end
        TicVisit = tic;
        VisitInsertSec = 0;
        VisitCatalogRead = [];
        VisitCatalogAstro = [];
        VisitCatSec = NaN;
        perfLog(sprintf('%s visitStart %s', FnName, DataDir));
        cd(DataDir);    
        if Args.IgnoreStatus
            Injected = false;
        else
            try
                TicStatus = tic;
                Injected = contains(fileread('.status'), "injected into the proc catalog DB") ...
                    | contains(fileread('.status'), "not injectable into the proc catalog DB due to broken data files");
                perfLog(sprintf('%s readStatus %s', FnName, DataDir), toc(TicStatus));
            catch
                cd(Dir);
                fprintf(FIDnostatus,'%s \n',DataDir);
                CntNoStatus = CntNoStatus + 1;
                VisitStats = perfVisitEnd_(VisitStats, TicVisit, VisitInsertSec, 'noStatus', FnName, DataDir);
                continue
            end
        end
        ZipVisit = perfZipVisitStateInit_();
        if ~Injected
            if Args.UseZipCatalogs
                [ZipOk, ZipVisit, ZipFailOutcome] = perfZipPrepareAndExtract_(FnName, DataDir, Dir, Args);
                if ~ZipOk
                    cd(Dir);
                    VisitStats = perfVisitEnd_(VisitStats, TicVisit, VisitInsertSec, ZipFailOutcome, FnName, DataDir);
                    continue
                end
            elseif Args.Decompress
                % decompress the data files if requested:
                TicDec = tic;
                Decompress = sprintf('su %s -c "bunzip2 %s.bz2"',Args.RemoteUser,FileNameTemplate);
                [~, Err.Decompress] = system(Decompress); 
                Decompress = sprintf('su %s -c "xz -d %s.xz"',Args.RemoteUser,FileNameTemplate);
                [~, Err.Decompress] = system(Decompress); 
                perfLog(sprintf('%s decompress %s', FnName, DataDir), toc(TicDec));
            end            
            %
            try
                HasCatalogStats = Args.CatalogFileInventory || Args.CatalogRawFileRead;
                if HasCatalogStats
                    [Ncat, SizeMB, List] = perfCatalogFileStats_(FileNameTemplate);
                end
                if Args.CatalogFileInventory
                    perfLog(sprintf('%s catalogFileInventory n=%d sizeMB=%.2f %s', FnName, Ncat, SizeMB, DataDir));
                end
                RawSec = NaN;
                if Args.CatalogRawFileRead
                    TicRaw = tic;
                    perfReadCatalogFilesRaw_(List);
                    RawSec = toc(TicRaw);
                    if SizeMB > 0
                        perfLog(sprintf('%s readCatalogFilesRaw n=%d sizeMB=%.2f mbPerSec=%s %s', ...
                            FnName, Ncat, SizeMB, perfMbPerSecStr_(SizeMB, RawSec), DataDir), RawSec);
                    else
                        perfLog(sprintf('%s readCatalogFilesRaw n=%d %s', FnName, Ncat, DataDir), RawSec);
                    end
                end
                TicCat = tic;
                Cat = AstroCatalog(FileNameTemplate,'UseMex',true); % read the data
                CatSec = toc(TicCat);
                VisitCatSec = CatSec;
                if HasCatalogStats && SizeMB > 0
                    perfLog(sprintf('%s readAstroCatalog sizeMB=%.2f mbPerSec=%s %s', ...
                        FnName, SizeMB, perfMbPerSecStr_(SizeMB, CatSec), DataDir), CatSec);
                else
                    perfLog(sprintf('%s readAstroCatalog %s', FnName, DataDir), CatSec);
                end
                TicHdr = tic;
                AH  = AstroHeader(FileNameTemplate,3,'UseMex',true);
                HdrSec = toc(TicHdr);
                perfLog(sprintf('%s readAstroHeader %s', FnName, DataDir), HdrSec);
                if HasCatalogStats && SizeMB > 0
                    if Args.CatalogRawFileRead && ~isnan(RawSec)
                        RawMbpsStr = perfMbPerSecStr_(SizeMB, RawSec);
                    else
                        RawMbpsStr = 'n/a';
                    end
                    perfLog(sprintf('%s catalogReadRates sizeMB=%.2f rawMBps=%s catMBps=%s hdrMBps=%s n=%d %s', ...
                        FnName, SizeMB, RawMbpsStr, perfMbPerSecStr_(SizeMB, CatSec), perfMbPerSecStr_(SizeMB, HdrSec), Ncat, DataDir));
                    VisitCatalogRead = struct( ...
                        'SizeMB', SizeMB, ...
                        'RawSec', RawSec, ...
                        'RawMBps', perfMbPerSec_(SizeMB, RawSec), ...
                        'CatMBps', perfMbPerSec_(SizeMB, CatSec), ...
                        'HdrMBps', perfMbPerSec_(SizeMB, HdrSec));
                end
            catch
                cd(Dir);
                perfZipRmExtract_(FnName, ZipVisit, Dir);
                fprintf(FIDbrokendata,'%s \n',DataDir);
                CntBroken = CntBroken + 1;
                VisitStats = perfVisitEnd_(VisitStats, TicVisit, VisitInsertSec, 'broken', FnName, DataDir);
                continue
            end   
            cd(Dir);
            if numel(Cat) < 2 % likely no data have been read
                perfZipRmExtract_(FnName, ZipVisit, Dir);
                fprintf(FIDnodata,'%s \n',DataDir);
                CntNoData = CntNoData + 1;
                VisitStats = perfVisitEnd_(VisitStats, TicVisit, VisitInsertSec, 'noData', FnName, DataDir);
                continue
            end
            % remove the elements with insufficient number of columns:            
            NCol = max(arrayfun(@(x) size(x.Catalog, 2), Cat));
            Idx  = arrayfun(@(x) size(x.Catalog,2) < NCol, Cat);
            if Idx(1) % if the first catalog is broken, no Table properties have been read
                perfZipRmExtract_(FnName, ZipVisit, Dir);
                VisitStats = perfVisitEnd_(VisitStats, TicVisit, VisitInsertSec, 'brokenFirstCatalog', FnName, DataDir);
                continue
            end
            Cat(Idx) = [];
            Nobj = numel(Cat);            
            for Iobj = 1:Nobj  % insert JD from the header if it is missing in the catalog
                if ~Cat(Iobj).isColumn('JD')
                    JD   = AH(Iobj).getVal('JD');
                    Nrow = repmat(1,height(Cat(Iobj).Table),1);
                    insertCol(Cat(Iobj),JD.*Nrow,Inf,'JD','');
                end
            end
            if Args.CatalogAstroPerf
                NrowsAstro = perfSumCatRows_(Cat);
                RowsPerSec = perfRowsPerSec_(NrowsAstro, VisitCatSec);
                perfLog(sprintf('%s catalogAstroPerf nCat=%d nRows=%d catSec=%.3f rowsPerSec=%s %s', ...
                    FnName, Nobj, NrowsAstro, VisitCatSec, perfRowsPerSecStr_(NrowsAstro, VisitCatSec), DataDir));
                VisitCatalogAstro = struct( ...
                    'Ncat', Nobj, ...
                    'Nrows', NrowsAstro, ...
                    'CatSec', VisitCatSec, ...
                    'RowsPerSec', RowsPerSec);
            end
            TicPrep = tic;
            % check and add essential KEYWORDS if they are missing                  
            Pname = AH(1).getVal('PROJNAME');
            if isnan(AH(1).getVal('NODENUMB'))
                NODENUMB = str2num(Pname(6:7));
                for Crop=1:Nobj
                    AH(Crop).replaceVal('NODENUMB',NODENUMB);
                end
            end
            if isnan(AH(1).getVal('MOUNTNUM'))
                MOUNTNUM = str2num(Pname(9:10));
                for Crop=1:Nobj
                    AH(Crop).replaceVal('MOUNTNUM',MOUNTNUM);
                end
            end
            Subdir = AH(1).getVal('SUBDIR'); 
            if isempty(Subdir)          
                Parts  = strsplit(DataDir, '/');
                Subdir = Parts{end};    % Extract the last part of the full dir name
                for Crop=1:Nobj
                    AH(Crop).replaceVal('SUBDIR',Subdir);
                end
            end
            % insert the ingestion time
            JDnow = celestial.time.date2jd;
            for Crop=1:Nobj
                    AH(Crop).replaceVal('INGESTION_TIME_JD',JDnow);
            end
            perfLog(sprintf('%s keywordPrep Nobj=%d %s', FnName, Nobj, DataDir), toc(TicPrep));
            if strcmpi(Args.DBConnector,'legacy')
                % prepare file name for the CSV dump
                A = AstroFileName;
                A.ProjName = Pname;
                A.SubDir   = Subdir;
                A.Level    = Args.Level;
                A.Product  ='Cat';
                A.FieldID  = AH(1).getVal('FIELDID');
                A.JD       = AH(1).getVal('JD');
                A.CCDID = 1; A.Counter = 0; A.CropID = 0;
                A.FileType = "csv"; A.julday2time;
                CsvFN = erase(A.genFile,' ');
                if Args.SkipInsert
                    perfLog(sprintf('%s SKIP insertCatalog %s', FnName, DataDir));
                    CntSkipInsert = CntSkipInsert + 1;
                else
                    TicInsert = tic;
                    [T, Error] = perfCallInsertCatalog_(Cat, AH, Columns, DB, Args, CsvFN, DataDir);
                    InsertSec = toc(TicInsert);
                    [VisitInsertSec, CntInserted, InsertStats] = perfInsertCatalogDone_( ...
                        VisitInsertSec, CntInserted, InsertStats, FnName, DataDir, AH, Nobj, T, Error, InsertSec, Args.SkipDbInsert);
                end
                % move the CSV file into the proc catalog and edit the .status file
                if Args.UpdateStatus
                    CopyCSV = sprintf('su %s -c "cp -f %s/%s %s"',Args.RemoteUser,Dir,CsvFN,DataDir);
                    [~, Err.Copy] = system(CopyCSV);
                    TicSu = tic;
                    UpdateStatus = sprintf('su %s -c "echo ''%s injected into the proc catalog DB'' >> %s/.status"',...
                        Args.RemoteUser,tools.timeStamp.getTimeStamp,DataDir);
                    [~, Err.Update] = system(UpdateStatus);
                    perfLog(sprintf('%s updateStatus %s', FnName, DataDir), toc(TicSu));
                    if isempty(Err.Copy) && isempty(Err.Update)
                        RemLocalFile = sprintf('rm %s',CsvFN);
                        [~, Err.RemoveLocal] = system(RemLocalFile);
                    end
                end                
            else
                if Args.SkipInsert
                    perfLog(sprintf('%s SKIP insertCatalog %s', FnName, DataDir));
                    CntSkipInsert = CntSkipInsert + 1;
                else
                    TicInsert = tic;
                    [T, Error] = perfCallInsertCatalog_(Cat, AH, Columns, DB, Args, [], DataDir);
                    InsertSec = toc(TicInsert);
                    [VisitInsertSec, CntInserted, InsertStats] = perfInsertCatalogDone_( ...
                        VisitInsertSec, CntInserted, InsertStats, FnName, DataDir, AH, Nobj, T, Error, InsertSec, Args.SkipDbInsert);
                end
                % edit the .status file
                if Args.UpdateStatus
                    TicSu = tic;
                    UpdateStatus = sprintf('su %s -c "echo ''%s injected into the proc catalog DB'' >> %s/.status"',...
                        Args.RemoteUser,tools.timeStamp.getTimeStamp,DataDir);
                    [~, Err.Update] = system(UpdateStatus);
                    perfLog(sprintf('%s updateStatus %s', FnName, DataDir), toc(TicSu));
                end
            end
            if Args.CompressProcessed
                TicComp = tic;
                Compress = sprintf('su %s -c "bzip2 %s/%s"',Args.RemoteUser,DataDir,FileNameTemplate);
                [~, Err.Decompress] = system(Compress); 
                perfLog(sprintf('%s compress %s', FnName, DataDir), toc(TicComp));
            end
            CatalogReadStats = perfCatalogReadStatsRecord_(CatalogReadStats, VisitCatalogRead);
            CatalogAstroStats = perfCatalogAstroStatsRecord_(CatalogAstroStats, VisitCatalogAstro);
            perfZipRmExtract_(FnName, ZipVisit, Dir);
            VisitStats = perfVisitEnd_(VisitStats, TicVisit, VisitInsertSec, 'injected', FnName, DataDir);
        else
            cd(Dir);
            CntSkippedInjected = CntSkippedInjected + 1;
        end                        
    end
    fclose(FIDnostatus);
    fclose(FIDnodata);
    fclose(FIDbrokendata);
    % disconnect the DB   
    if strcmpi(Args.DBConnector,'legacy')
        DB.disconnectCH_Java;
    else
        DB.delete;
    end
    pipeline.last.insertDB.perfVisitStats('logSummary', VisitStats, FnName);
    perfCatalogReadStatsLogSummary_(FnName, CatalogReadStats);
    perfCatalogAstroStatsLogSummary_(FnName, CatalogAstroStats);
    perfInsertStatsLogSummary_(FnName, InsertStats);
    perfLog(sprintf('%s SUMMARY skippedInjected=%d noStatus=%d noData=%d broken=%d inserted=%d skipInsert=%d', ...
        FnName, CntSkippedInjected, CntNoStatus, CntNoData, CntBroken, CntInserted, CntSkipInsert));
    perfLog(sprintf('%s FINISH', FnName), toc(RunT0));
    fprintf('Ingestion completed. \n');
end
function VisitStats = perfVisitEnd_(VisitStats, TicVisit, VisitInsertSec, Outcome, FnName, DataDir)
    TotalSec = toc(TicVisit);
    fprintf(' ..visit done in %.1fs (log: insertDB_perf.log)\n', TotalSec);
    VisitStats = pipeline.last.insertDB.perfVisitStats('endVisit', VisitStats, TotalSec, VisitInsertSec, Outcome);
    perfLog(sprintf('%s visitEnd %s %s', FnName, Outcome, DataDir), TotalSec);
    pipeline.last.insertDB.perfLogBlank();
end
function perfLog(Message, Seconds)
    if nargin < 2
        pipeline.last.insertDB.perfLog(Message);
    else
        pipeline.last.insertDB.perfLog(Message, Seconds);
    end
end

function [Nfiles, SizeMB, List] = perfCatalogFileStats_(Template)
    List = io.files.filelist(Template, 'AddPath', true);
    Nfiles = numel(List);
    TotalBytes = 0;
    for I = 1:Nfiles
        Info = dir(List{I});
        if ~isempty(Info)
            TotalBytes = TotalBytes + Info.bytes;
        end
    end
    SizeMB = TotalBytes / (1024^2);
end

function perfReadCatalogFilesRaw_(FileList)
    for I = 1:numel(FileList)
        Fid = fopen(FileList{I}, 'rb');
        if Fid < 0
            error('insertArchiveCatalogs2DB_perf:CannotOpenCatalogFile', '%s', FileList{I});
        end
        fread(Fid, inf, '*uint8');
        fclose(Fid);
    end
end

function Nrows = perfSumCatRows_(Cat)
    Nrows = 0;
    for I = 1:numel(Cat)
        if ~isempty(Cat(I).Table)
            Nrows = Nrows + height(Cat(I).Table);
        else
            Nrows = Nrows + size(Cat(I).Catalog, 1);
        end
    end
end

function Rate = perfRowsPerSec_(Nrows, Sec)
    if Sec > 0 && Nrows > 0
        Rate = Nrows / Sec;
    else
        Rate = NaN;
    end
end

function Str = perfRowsPerSecStr_(Nrows, Sec)
    Rate = perfRowsPerSec_(Nrows, Sec);
    if isnan(Rate)
        Str = 'n/a';
    else
        Str = sprintf('%.1f', Rate);
    end
end

function S = perfCatalogAstroStatsInit_()
    S.Ncat = [];
    S.Nrows = [];
    S.CatSec = [];
    S.RowsPerSec = [];
end

function S = perfCatalogAstroStatsRecord_(S, Visit)
    if isempty(Visit)
        return
    end
    S.Ncat(end + 1, 1) = Visit.Ncat;
    S.Nrows(end + 1, 1) = Visit.Nrows;
    S.CatSec(end + 1, 1) = Visit.CatSec;
    S.RowsPerSec(end + 1, 1) = Visit.RowsPerSec;
end

function perfCatalogAstroStatsLogSummary_(FnName, S)
    N = numel(S.Nrows);
    if N == 0
        return
    end
    WeightedStr = 'n/a';
    TotalSec = sum(S.CatSec);
    if TotalSec > 0
        WeightedStr = sprintf('%.1f', sum(S.Nrows) / TotalSec);
    end
    Msg = sprintf('%s AVG catalogAstroPerf n=%d nRows=%d nRowsMean=%.0f rowsPerSec mean=%.1f med=%.1f weighted=%s', ...
        FnName, N, sum(S.Nrows), mean(S.Nrows), mean(S.RowsPerSec), perfInsertMedian_(S.RowsPerSec), WeightedStr);
    perfLog(Msg);
end

function S = perfCatalogReadStatsInit_()
    S.SizeMB = [];
    S.RawSec = [];
    S.RawMBps = [];
    S.CatMBps = [];
    S.HdrMBps = [];
end

function S = perfCatalogReadStatsRecord_(S, Visit)
    if isempty(Visit)
        return
    end
    S.SizeMB(end + 1, 1) = Visit.SizeMB;
    S.RawSec(end + 1, 1) = Visit.RawSec;
    S.RawMBps(end + 1, 1) = Visit.RawMBps;
    S.CatMBps(end + 1, 1) = Visit.CatMBps;
    S.HdrMBps(end + 1, 1) = Visit.HdrMBps;
end

function perfCatalogReadStatsLogSummary_(FnName, S)
    N = numel(S.SizeMB);
    if N == 0
        return
    end
    RawWeightedStr = 'n/a';
    HasRaw = ~isnan(S.RawSec);
    if any(HasRaw)
        TotalMB = sum(S.SizeMB(HasRaw));
        TotalRawSec = sum(S.RawSec(HasRaw));
        if TotalRawSec > 0
            RawWeightedStr = sprintf('%.2f', TotalMB / TotalRawSec);
        end
    end
    RawV = S.RawMBps(~isnan(S.RawMBps));
    if isempty(RawV)
        RawMeanStr = 'n/a';
        RawMedStr = 'n/a';
    else
        RawMeanStr = sprintf('%.2f', mean(RawV));
        RawMedStr = sprintf('%.2f', perfInsertMedian_(RawV));
    end
    Msg = sprintf('%s AVG catalogReadRates n=%d rawMBps mean=%s med=%s weighted=%s catMBps mean=%.2f med=%.2f hdrMBps mean=%.2f med=%.2f', ...
        FnName, N, RawMeanStr, RawMedStr, RawWeightedStr, ...
        mean(S.CatMBps), perfInsertMedian_(S.CatMBps), mean(S.HdrMBps), perfInsertMedian_(S.HdrMBps));
    perfLog(Msg);
end

function Rate = perfMbPerSec_(SizeMB, Sec)
    if Sec > 0 && SizeMB > 0
        Rate = SizeMB / Sec;
    else
        Rate = NaN;
    end
end

function Str = perfMbPerSecStr_(SizeMB, Sec)
    Rate = perfMbPerSec_(SizeMB, Sec);
    if isnan(Rate)
        Str = 'n/a';
    else
        Str = sprintf('%.2f', Rate);
    end
end

function S = perfInsertStatsInit_()
    S.Nrows = [];
    S.Ncat = [];
    S.SrcPerCat = [];
    S.Sec = [];
end

function [T, Error] = perfCallInsertCatalog_(Cat, AH, Columns, DB, Args, CsvFN, DataDir)
    InsertArgs = {'Header', AH, 'ColNameDic', Columns, 'Db', DB, 'DbName', Args.DbName, ...
        'DbTable', Args.DbTable, 'DBConnector', Args.DBConnector, 'SkipDbInsert', Args.SkipDbInsert};
    if Args.UseInsertCatalogPerf
        InsertArgs = [InsertArgs, {'PerfLogFcn', @pipeline.last.insertDB.perfLog, ...
            'PerfLogTag', 'insertArchiveCatalogs2DB_perf', 'PerfContext', DataDir}];
        if strcmpi(Args.DBConnector, 'legacy')
            [T, Error] = imProc.db.insertCatalog_perf(Cat, InsertArgs{:}, ...
                'CreateCsv', true, 'FileName', CsvFN);
        else
            [T, Error] = imProc.db.insertCatalog_perf(Cat, InsertArgs{:}, ...
                'CreateCsv', false, 'Schema', Args.Schema, 'MaxBatchLines', 200000, 'Verbosity', 0);
        end
    else
        if strcmpi(Args.DBConnector, 'legacy')
            [T, Error] = imProc.db.insertCatalog(Cat, InsertArgs{:}, ...
                'CreateCsv', true, 'FileName', CsvFN);
        else
            [T, Error] = imProc.db.insertCatalog(Cat, InsertArgs{:}, ...
                'CreateCsv', false, 'Schema', Args.Schema, 'MaxBatchLines', 200000, 'Verbosity', 0);
        end
    end
end

function [VisitInsertSec, CntInserted, InsertStats] = perfInsertCatalogDone_( ...
        VisitInsertSec, CntInserted, InsertStats, FnName, DataDir, AH, Ncat, T, Error, InsertSec, SkipDbInsert)
    FieldStr = perfFieldIdStr_(AH);
    Nrows = size(T, 1);
    SrcPerCat = Nrows / max(Ncat, 1);
    InsertStats.Nrows(end + 1, 1) = Nrows;
    InsertStats.Ncat(end + 1, 1) = Ncat;
    InsertStats.SrcPerCat(end + 1, 1) = SrcPerCat;
    InsertStats.Sec(end + 1, 1) = InsertSec;
    SkipDbStr = '';
    if SkipDbInsert
        SkipDbStr = ' skipDb=1';
    end
    if Nrows > 0
        SecPer100k = InsertSec / Nrows * 1e5;
        perfLog(sprintf('%s insertCatalog fieldId=%s nCat=%d nRows=%d srcPerCat=%.1f secPer100k=%.3f%s %s', ...
            FnName, FieldStr, Ncat, Nrows, SrcPerCat, SecPer100k, SkipDbStr, DataDir), InsertSec);
    else
        perfLog(sprintf('%s insertCatalog fieldId=%s nCat=%d nRows=0 srcPerCat=0.0 secPer100k=n/a%s %s', ...
            FnName, FieldStr, Ncat, SkipDbStr, DataDir), InsertSec);
    end
    if ~isempty(Error)
        error('catalog injection failed');
    end
    VisitInsertSec = VisitInsertSec + InsertSec;
    CntInserted = CntInserted + 1;
end

function perfInsertStatsLogSummary_(FnName, InsertStats)
    Nvisit = numel(InsertStats.Nrows);
    if Nvisit == 0
        return
    end
    NrowsAll = InsertStats.Nrows;
    SrcPerCat = InsertStats.SrcPerCat;
    SecV = InsertStats.Sec;
    MedNrows = perfInsertMedian_(NrowsAll);
    MedSrcPerCat = perfInsertMedian_(SrcPerCat);
    SecPer100kV = SecV ./ NrowsAll * 1e5;
    SecPer100kV(NrowsAll == 0) = NaN;
    WeightedStr = 'n/a';
    if sum(NrowsAll) > 0
        WeightedStr = sprintf('%.3f', 1e5 * sum(SecV) / sum(NrowsAll));
    end
    Msg = sprintf('%s AVG insertCatalog n=%d nRows=%d nRowsMean=%.0f nRowsMed=%.0f srcPerCat mean=%.1f med=%.1f min=%.1f max=%.1f insertSec mean=%.2fs med=%.2fs secPer100k mean=%.3f weighted=%s', ...
        FnName, Nvisit, sum(NrowsAll), mean(NrowsAll), MedNrows, mean(SrcPerCat), MedSrcPerCat, min(SrcPerCat), max(SrcPerCat), ...
        mean(SecV), perfInsertMedian_(SecV), mean(SecPer100kV, 'omitnan'), WeightedStr);
    perfLog(Msg);
end

function Val = perfInsertMedian_(V)
    V = sort(V(:));
    if isempty(V)
        Val = NaN;
        return
    end
    Idx = max(1, min(numel(V), ceil(0.5 * numel(V))));
    Val = V(Idx);
end

function FieldStr = perfFieldIdStr_(AH)
    FieldId = AH(1).getVal('FIELDID');
    if isempty(FieldId) || (isnumeric(FieldId) && all(isnan(FieldId(:))))
        FieldStr = 'n/a';
    elseif ischar(FieldId) || isstring(FieldId)
        FieldStr = strtrim(char(FieldId));
    else
        FieldStr = num2str(FieldId);
    end
end

function S = perfZipVisitStateInit_()
    S.Active = false;
    S.ExtractRoot = '';
    S.VisitId = '';
    S.ExtractDir = '';
end

function Root = perfZipExtractRoot_(ExtractRootArg)
    if ~isempty(ExtractRootArg)
        Root = ExtractRootArg;
        return
    end
    if ispc
        Root = 'C:/Temp';
    elseif isfolder('/dev/shm')
        Root = '/dev/shm';
    else
        Root = '/tmp';
    end
end

function VisitId = perfVisitIdFromDataDir_(DataDir)
    DataDir = strtrim(DataDir);
    DataDir = regexprep(DataDir, '[\\/]+$', '');
    [~, VisitId, ~] = fileparts(DataDir);
end

function Ok = perfZipVisitIdValid_(VisitId)
    Ok = ~isempty(VisitId) && ~contains(VisitId, '/') && ~contains(VisitId, '\') ...
        && ~contains(VisitId, '..');
end

function Ok = perfZipIsAllowedExtractDir_(ExtractDir, ExtractRoot, VisitId)
    if ~perfZipVisitIdValid_(VisitId) || isempty(ExtractRoot)
        Ok = false;
        return
    end
    Expected = fullfile(ExtractRoot, 'insertArchiveCatalogs2DB', VisitId);
    ParentOnly = fullfile(ExtractRoot, 'insertArchiveCatalogs2DB');
    ExtractDir = fullfile(ExtractDir);
    ExtractRoot = fullfile(ExtractRoot);
    Ok = strcmp(ExtractDir, Expected) && ~strcmp(ExtractDir, ExtractRoot) ...
        && ~strcmp(ExtractDir, ParentOnly);
end

function [Ok, ZipVisit, FailOutcome] = perfZipPrepareAndExtract_(FnName, DataDir, Dir, Args)
    ZipVisit = perfZipVisitStateInit_();
    FailOutcome = 'noZip';
    Ok = false;
    VisitId = perfVisitIdFromDataDir_(DataDir);
    if ~perfZipVisitIdValid_(VisitId)
        perfLog(sprintf('%s noZip invalidVisitId=%s %s', FnName, VisitId, DataDir));
        return
    end
    ExtractRoot = perfZipExtractRoot_(Args.ExtractRoot);
    if isempty(Args.ZipDir)
        ZipDir = fullfile(fileparts(DataDir), '_zip');
    else
        ZipDir = Args.ZipDir;
    end
    ZipPath = fullfile(ZipDir, [VisitId '.zip']);
    if ~isfile(ZipPath)
        perfLog(sprintf('%s noZip zip=%s %s', FnName, ZipPath, DataDir));
        return
    end
    ExtractDir = fullfile(ExtractRoot, 'insertArchiveCatalogs2DB', VisitId);
    if ~perfZipIsAllowedExtractDir_(ExtractDir, ExtractRoot, VisitId)
        perfLog(sprintf('%s noZip extractPathRejected dest=%s %s', FnName, ExtractDir, DataDir));
        return
    end
    cd(Dir);
    perfZipRmExtract_(FnName, struct('Active', true, 'ExtractRoot', ExtractRoot, ...
        'VisitId', VisitId, 'ExtractDir', ExtractDir), Dir);
    if ~isfolder(ExtractDir)
        mkdir(ExtractDir);
    end
    TicUnzip = tic;
    try
        unzip(ZipPath, ExtractDir);
    catch ME
        cd(Dir);
        perfZipRmExtract_(FnName, struct('Active', true, 'ExtractRoot', ExtractRoot, ...
            'VisitId', VisitId, 'ExtractDir', ExtractDir), Dir);
        perfLog(sprintf('%s unzipVisit ERROR=%s zip=%s dest=%s %s', ...
            FnName, ME.message, ZipPath, ExtractDir, DataDir), toc(TicUnzip));
        FailOutcome = 'broken';
        return
    end
    perfLog(sprintf('%s unzipVisit zip=%s dest=%s %s', FnName, ZipPath, ExtractDir, DataDir), toc(TicUnzip));
    cd(ExtractDir);
    ZipVisit.Active = true;
    ZipVisit.ExtractRoot = ExtractRoot;
    ZipVisit.VisitId = VisitId;
    ZipVisit.ExtractDir = ExtractDir;
    Ok = true;
    FailOutcome = '';
end

function perfZipRmExtract_(FnName, ZipVisit, Dir)
    if ~ZipVisit.Active
        return
    end
    ExtractDir = ZipVisit.ExtractDir;
    if ~perfZipIsAllowedExtractDir_(ExtractDir, ZipVisit.ExtractRoot, ZipVisit.VisitId)
        return
    end
    if ~isempty(Dir)
        cd(Dir);
    end
    if isfolder(ExtractDir)
        TicRm = tic;
        rmdir(ExtractDir, 's');
        perfLog(sprintf('%s rmExtract dest=%s', FnName, ExtractDir), toc(TicRm));
    end
end
