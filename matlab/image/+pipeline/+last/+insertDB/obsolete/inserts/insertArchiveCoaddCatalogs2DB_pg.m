function [Result] = insertArchiveCoaddCatalogs2DB_pg(RootDir, FileNameTemplate, Args)
    % insert the archived coadd LAST catalogs to DB 
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
    %          pipeline.last.insertDB.insertArchiveCoaddCatalogs2DB(RootDir,Template)    
    %    
    %          pipeline.last.insertDB.insertArchiveCoaddCatalogs2DB('/mnt/marvin/LAST.01.01.01/2023/04/24/','ProcDirTemplate','/proc/*')
    %          pipeline.last.insertDB.insertArchiveCoaddCatalogs2DB('/mnt/marvin/','ProcDirTemplate','LAST.01.01*/*/*/*/proc/*')
    %          pipeline.last.insertDB.insertArchiveCoaddCatalogs2DB('/mnt/marvin/','ProcDirTemplate','LAST.01.*/2023/*/*/proc/*')
    %
    arguments
        RootDir                = '/mnt/marvin/LAST.01*/';
        FileNameTemplate       = 'LAST*coadd_Cat_1.fits';      
        Args.ProcDirTemplate   = '/proc/*';
        Args.ProcDirList       = [];
        Args.Decompress        = false;
        Args.CompressProcessed = false;
        Args.UpdateStatus      = true;
        Args.Template          = '~/matlab/data/db/Design-Database-Pipeline-ClickHouse.xlsx';
        Args.DbHost = 'euclid';
        Args.DbName = 'last';   
        Args.DbUser = 'default';
        Args.DbPort =  9000;
        Args.AstroDBPassFile   = '~/.astropack/Passwords.yml';
        Args.Level  = 'coadd';
        Args.DbTable= 'visit_src';
        Args.DbPassKey       = '';
        Args.ColMapTableName = '';
        Args.KeyID     = 'id_visit_im';
        Args.ColNameID = 'id_visit_src';
        Args.RemoteUser  = 'euclid';
        Args.DBConnector = 'native'; % 'legacy'; % 'native' or 'legacy'
        Args.ConnectorOpts = struct('compression', db.mex.Compression.ZSTD);
        Args.Schema        = [];       % the user may input the schema of the DB table 
                                           % as a matlab table output of the "DESCRIBE TABLE" SQL command
        Args.SkipInsert        = false;
        Args.IgnoreStatus      = false;
        Args.PgServer         (1,1) string = "euclid";
        Args.PgPort           (1,1) double = 5432;
        Args.PgDatabase       (1,1) string = "obs";
        Args.PgUser           (1,1) string = "pipeline";
        Args.PgPassKey        (1,1) string = "euclid_pg_status";
        Args.PgStatusRoot     (1,1) string = "/bigdata2/projects/last/data";
    end    
    FnName = 'insertArchiveCoaddCatalogs2DB_pg';
    RunT0 = tic;
    pipeline.last.insertDB.perfLogRunBanner(FnName);
    perfLog(sprintf('%s START RootDir=%s SkipInsert=%d UpdateStatus=%d IgnoreStatus=%d', ...
        FnName, RootDir, Args.SkipInsert, Args.UpdateStatus, Args.IgnoreStatus));
    CntSkippedInjected = 0;
    CntNoStatus = 0;
    CntNoData = 0;
    CntBroken = 0;
    CntInserted = 0;
    CntSkipInsert = 0;
    VisitStats = pipeline.last.insertDB.perfVisitStats('init');
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
    TicPg = tic;
    VisitStatusDB = pipeline.last.insertDB.status.openVisitStatusDB(Args);
    perfLog(sprintf('%s pgStatusConnect Db=%s', FnName, Args.PgDatabase), toc(TicPg));
    StatusPhrases = ["injected into the visit catalog DB", ...
        "not injectable into the visit catalog DB due to broken data files"];
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
    FIDnostatus     = fopen('cat_visit_no_status_dir.txt', 'a');
    FIDnodata       = fopen('cat_visit_no_data_dir.txt', 'a'); 
    FIDbrokendata   = fopen('cat_visit_broken_data_dir.txt', 'a');
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
    for IDir = 1:Ndir
        if isempty(Args.ProcDirList)
            DataDir = strcat(Dirs(IDir).folder,'/',Dirs(IDir).name);
        else
            DataDir = Dirs(IDir);
            if iscell(DataDir)
                DataDir = DataDir{1};
            end
            if isstring(DataDir)
                DataDir = char(DataDir);
            end
        end
        TicVisit = tic;
        VisitInsertSec = 0;
        perfLog(sprintf('%s visitStart %s', FnName, DataDir));
        cd(DataDir);    
        try
            [Injected, ~, StatusOutcome] = pipeline.last.insertDB.visitStatusPg( ...
                'read', FnName, VisitStatusDB, DataDir, StatusPhrases, Args.IgnoreStatus);
        catch
            cd(Dir);
            fprintf(FIDnostatus,'%s \n',DataDir);
            CntNoStatus = CntNoStatus + 1;
            VisitStats = perfVisitEnd_(VisitStats, TicVisit, VisitInsertSec, 'noStatus', FnName, DataDir);
            continue
        end
        if strcmp(StatusOutcome, 'noStatus')
            cd(Dir);
            fprintf(FIDnostatus,'%s \n',DataDir);
            CntNoStatus = CntNoStatus + 1;
            VisitStats = perfVisitEnd_(VisitStats, TicVisit, VisitInsertSec, 'noStatus', FnName, DataDir);
            continue
        end
        if ~Injected
            % decompress the data files if requested:
            if Args.Decompress
                TicDec = tic;
                Decompress = sprintf('su %s -c "bunzip2 %s.bz2"',Args.RemoteUser,FileNameTemplate);
                [~, Err.Decompress] = system(Decompress); 
                perfLog(sprintf('%s decompress %s', FnName, DataDir), toc(TicDec));
            end            
            %
            try
                TicCat = tic;
                Cat = AstroCatalog(FileNameTemplate); % read the data
                perfLog(sprintf('%s readAstroCatalog %s', FnName, DataDir), toc(TicCat));
                TicHdr = tic;
                AH  = AstroHeader(FileNameTemplate,3);
                perfLog(sprintf('%s readAstroHeader %s', FnName, DataDir), toc(TicHdr));
            catch
                cd(Dir);
                fprintf(FIDbrokendata,'%s \n',DataDir);
                CntBroken = CntBroken + 1;
                VisitStats = perfVisitEnd_(VisitStats, TicVisit, VisitInsertSec, 'broken', FnName, DataDir);
                continue
            end
            cd(Dir);
            if numel(Cat) < 2 % likely no data have been read 
                cd(Dir);
                fprintf(FIDnodata,'%s \n',DataDir);
                CntNoData = CntNoData + 1;
                VisitStats = perfVisitEnd_(VisitStats, TicVisit, VisitInsertSec, 'noData', FnName, DataDir);
                continue
            end
            % remove the elements with insufficient number of columns:            
            NCol = max(arrayfun(@(x) size(x.Catalog, 2), Cat));
            Idx  = arrayfun(@(x) size(x.Catalog,2) < NCol, Cat);
            if Idx(1) % if the first catalog is broken, no Table properties have been read
                VisitStats = perfVisitEnd_(VisitStats, TicVisit, VisitInsertSec, 'brokenFirstCatalog', FnName, DataDir);
                continue
            end
            Cat(Idx) = [];
            Nobj = numel(Cat);    
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
            if ~all(Cat.isColumn('JD'))
                for Crop=1:Nobj
                    if ~Cat(Crop).isColumn('JD')                        
                        Cat(Crop).insertCol(repmat(AH(Crop).getVal('JD'), height(Cat(Crop).Table),1), Inf, 'JD', 'day');
                    end
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
                    [~, Error]=imProc.db.insertCatalog(Cat,'Header',AH,'ColNameDic',Columns,'Db',DB,'DbName',Args.DbName,'DbTable',Args.DbTable,...
                        'CreateCsv',true,'FileName',CsvFN,'ColSrcID',Args.ColNameID,'KeyID',Args.KeyID,'DBConnector',Args.DBConnector);
                    InsertSec = toc(TicInsert);
                    VisitInsertSec = VisitInsertSec + InsertSec;
                    perfLog(sprintf('%s insertCatalog %s', FnName, DataDir), InsertSec);
                    if ~isempty(Error)
                        fprintf('catalog injection failed: %s\n', Error);
                        error('exiting because of an unrecoverable error');
                        %                 continue
                    end
                    CntInserted = CntInserted + 1;
                end
                if Args.UpdateStatus
                    StampLine = sprintf('%s injected into the visit catalog DB', tools.timeStamp.getTimeStamp);
                    pipeline.last.insertDB.visitStatusPg('append', FnName, VisitStatusDB, DataDir, StampLine, true);
                end
            else
                if Args.SkipInsert
                    perfLog(sprintf('%s SKIP insertCatalog %s', FnName, DataDir));
                    CntSkipInsert = CntSkipInsert + 1;
                else
                    TicInsert = tic;
                    [~, Error]=imProc.db.insertCatalog(Cat,'Header',AH,'ColNameDic',Columns,'Db',DB,'DbName',Args.DbName,'DbTable',Args.DbTable,...
                        'CreateCsv',false,'ColSrcID',Args.ColNameID,'KeyID',Args.KeyID,'DBConnector',Args.DBConnector,...
                        'Schema',Args.Schema, 'MaxBatchLines',200000,'Verbosity',0);                               
                    InsertSec = toc(TicInsert);
                    VisitInsertSec = VisitInsertSec + InsertSec;
                    perfLog(sprintf('%s insertCatalog %s', FnName, DataDir), InsertSec);
                    if ~isempty(Error)
                        error('catalog injection failed');
                    end
                    CntInserted = CntInserted + 1;
                end
                if Args.UpdateStatus
                    StampLine = sprintf('%s injected into the visit catalog DB', tools.timeStamp.getTimeStamp);
                    pipeline.last.insertDB.visitStatusPg('append', FnName, VisitStatusDB, DataDir, StampLine, true);
                end
            end
            % compress the data files if requested:
            if Args.CompressProcessed
                TicComp = tic;
                Decompress = sprintf('su %s -c "bzip2 %s/%s"',Args.RemoteUser,DataDir,FileNameTemplate);
                [~, Err.Decompress] = system(Decompress); 
                perfLog(sprintf('%s compress %s', FnName, DataDir), toc(TicComp));
            end
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
