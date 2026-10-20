function [Result] = insertArchiveAsteroids2DB_pg(RootDir, FileNameTemplate, Args)
    % insert the archived LAST asteroid tables to DB 
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
    % Author : A.M. Krassilchtchikov (2024 Dec) 
    % Example: RootDir = '/Data1/LAST.01.01.01/'; 
    %          Template = '*coadd*Aster*mat';
    %          pipeline.last.insertDB.insertArchiveAsteroids2DB(RootDir,Template)    
    %    
    %          pipeline.last.insertDB.insertArchiveAsteroids2DB('/mnt/marvin/LAST.01.02.01/2023/04/24/','ProcDirTemplate','proc/*')
    %          pipeline.last.insertDB.insertArchiveAsteroids2DB('/mnt/marvin/LAST.01.02.01/','ProcDirTemplate','*/*/*/proc/*')
    %          pipeline.last.insertDB.insertArchiveAsteroids2DB('/mnt/marvin/','ProcDirTemplate','LAST.01.02*/*/*/*/proc/*')
    %
    arguments
        RootDir                = '/mnt/marvin/LAST.01*/';
        FileNameTemplate       = 'LAST*coadd_Aster*.mat';      
        Args.ProcDirTemplate   = '/proc/*';
        Args.ProcDirList       = [];
        
        Args.Template          = '~/matlab/data/db/Design-Database-Pipeline-ClickHouse.xlsx';
        
        Args.DbHost = 'euclid';
        Args.DbName = 'last';   
        Args.DbUser = 'default';
        Args.DbPort =  9000;
        Args.DbTable= 'visit_asteroids';  %  'visit_asteroids'
        Args.DbPassKey         = '';
        Args.ColMapTableName   = '';
        Args.AstroDBPassFile   = '~/.astropack/Passwords.yml';
        Args.DBConnector       = 'native'; % 'legacy'; % 'native' or 'legacy'
        Args.ConnectorOpts     = struct('compression', db.mex.Compression.ZSTD);
        Args.Schema            = [];       % the user may input the schema of the DB table 
                                           % as a matlab table output of the "DESCRIBE TABLE" SQL command        
        Args.Level  = 'coadd';
       
        Args.KeyID     = 'id_visit_im';
        Args.ColNameID = 'id_visit_src';                
        Args.RemoteUser = 'samar';
        Args.SkipInsert        = false;
        Args.UpdateStatus      = true;
        Args.IgnoreStatus      = false;
        Args.PgServer         (1,1) string = "euclid";
        Args.PgPort           (1,1) double = 5432;
        Args.PgDatabase       (1,1) string = "obs";
        Args.PgUser           (1,1) string = "pipeline";
        Args.PgPassKey        (1,1) string = "euclid_pg_status";
        Args.PgStatusRoot     (1,1) string = "/bigdata2/projects/last/data";
    end    
    FnName = 'insertArchiveAsteroids2DB_pg';
    RunT0 = tic;
    pipeline.last.insertDB.perf.perfLogRunBanner(FnName);
    perfLog(sprintf('%s START RootDir=%s SkipInsert=%d UpdateStatus=%d IgnoreStatus=%d', ...
        FnName, RootDir, Args.SkipInsert, Args.UpdateStatus, Args.IgnoreStatus));
    CntSkippedInjected = 0;
    CntNoStatus = 0;
    CntInserted = 0;
    CntSkipInsert = 0;
    VisitStats = pipeline.last.insertDB.perf.perfVisitStats('init');
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
    StatusPhrases = "injected into the visit asteriods catalog DB";
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
    FID = fopen('aster_no_status_dir.txt', 'a');
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
        perfLog(sprintf('%s visitStart %s', FnName, DataDir));
        cd(DataDir);        
        try
            [Injected, ~, StatusOutcome] = pipeline.last.insertDB.visitStatusPg( ...
                'read', FnName, VisitStatusDB, DataDir, StatusPhrases, Args.IgnoreStatus);
        catch
            cd(Dir);
            fprintf(FID,'%s \n',DataDir);
            CntNoStatus = CntNoStatus + 1;
            VisitStats = perfVisitEnd_(VisitStats, TicVisit, VisitInsertSec, 'noStatus', FnName, DataDir);
            continue
        end
        if strcmp(StatusOutcome, 'noStatus')
            cd(Dir);
            fprintf(FID,'%s \n',DataDir);
            CntNoStatus = CntNoStatus + 1;
            VisitStats = perfVisitEnd_(VisitStats, TicVisit, VisitInsertSec, 'noStatus', FnName, DataDir);
            continue
        end
        if ~Injected
            try
                TicLoad = tic;
                load(dir(FileNameTemplate).name,'');  % the .mat file will produce Obj variable
                if height(Obj.Table) < 1              % if the table is empty, skip to the next visit
                    cd(Dir);
                    VisitStats = perfVisitEnd_(VisitStats, TicVisit, VisitInsertSec, 'emptyTable', FnName, DataDir);
                    continue
                end
                if ismember('CropID', Obj.Table.Properties.VariableNames)
                    Obj.Table.CropID = Obj.Table.SubImageIndex;                     % copy data from the right column
                else
                    Obj.Table.Properties.VariableNames{'SubImageIndex'} = 'CropID'; % repair the column name
                end
                %
                CropID = unique(Obj.Table.CropID);
                NCrop  = numel(CropID);
                ObjNew = struct([]);
                AH     = repmat(AstroHeader,1,NCrop);
                Headers= dir('*coadd_Cat*fits'); % get the list of all the coadd*Cat files from where to read the headers                                
                for Icrop = 1:NCrop
                    ObjNew(Icrop).Table = Obj.Table(Obj.Table.CropID == CropID(Icrop), :); % select the lines by cropid
                    AH(Icrop) = AstroHeader(Headers(CropID(Icrop)).name,3);                % for each cropid read the appropriate header
                end                
                perfLog(sprintf('%s loadMat NCrop=%d %s', FnName, NCrop, DataDir), toc(TicLoad));
                %                                
            catch
                cd(Dir);
                fprintf(FID,'%s \n',DataDir);
                VisitStats = perfVisitEnd_(VisitStats, TicVisit, VisitInsertSec, 'broken', FnName, DataDir);
                continue
            end
            cd(Dir);
            TicPrep = tic;

            % check and add essential KEYWORDS if they are missing                  
            Pname = AH(1).getStructKey('PROJNAME').PROJNAME;
            if isnan(AH(1).getStructKey('NODENUMB').NODENUMB)
                NODENUMB = str2num(Pname(6:7));
                for Icrop = 1:NCrop
                    AH(Icrop).replaceVal('NODENUMB',NODENUMB);
                end                
            end
            if isnan(AH(1).getStructKey('MOUNTNUM').MOUNTNUM)
                MOUNTNUM = str2num(Pname(9:10));
                for Icrop = 1:NCrop
                    AH(Icrop).replaceVal('MOUNTNUM',MOUNTNUM);
                end
            end
            Subdir = AH(1).getStructKey('SUBDIR').SUBDIR; 
            if isempty(Subdir)          
                Parts  = strsplit(DataDir, '/');
                Subdir = Parts{end};    % Extract the last part of the full dir name      
                for Icrop=1:NCrop
                    AH(Icrop).replaceVal('SUBDIR',Subdir);
                end               
            end
            % insert the ingestion time
            JDnow = celestial.time.date2jd;
            for Icrop=1:NCrop
                    AH(Icrop).replaceVal('INGESTION_TIME_JD',JDnow);
            end
            perfLog(sprintf('%s keywordPrep %s', FnName, DataDir), toc(TicPrep));
                                                                       
            if strcmpi(Args.DBConnector,'legacy')
                % prepare file name for the CSV dump
                A = AstroFileName;
                A.ProjName = Pname;
                A.SubDir   = Subdir;
                A.Level    = Args.Level;
                A.Product  = "Asteroids";
                A.FieldID  = AH.getStructKey('FIELDID').FIELDID;
                A.JD       = AH.getStructKey('JD').JD;
                A.CCDID = 1; A.Counter = 0; A.CropID = 0;
                A.FileType = "csv"; A.julday2time;
                CsvFN = erase(A.genFile,' ');
                
                if Args.SkipInsert
                    perfLog(sprintf('%s SKIP insertCatalog %s', FnName, DataDir));
                    CntSkipInsert = CntSkipInsert + 1;
                else
                    TicInsert = tic;
                    [~, Error]=imProc.db.insertCatalog(ObjNew,'Header',AH,'ColNameDic',Columns,'Db',DB,'DbName',Args.DbName,'DbTable',Args.DbTable,...
                        'CreateCsv',true,'FileName',CsvFN,'ColSrcID',Args.ColNameID,'KeyID',Args.KeyID);
                    InsertSec = toc(TicInsert);
                    VisitInsertSec = VisitInsertSec + InsertSec;
                    perfLog(sprintf('%s insertCatalog %s', FnName, DataDir), InsertSec);
                    if ~isempty(Error)
                        error('catalog injection failed');
                    end
                    CntInserted = CntInserted + 1;
                end
                if Args.UpdateStatus
                    StampLine = sprintf('%s injected into the visit asteriods catalog DB', tools.timeStamp.getTimeStamp);
                    pipeline.last.insertDB.visitStatusPg('append', FnName, VisitStatusDB, DataDir, StampLine, true);
                end
            else
                if Args.SkipInsert
                    perfLog(sprintf('%s SKIP insertCatalog %s', FnName, DataDir));
                    CntSkipInsert = CntSkipInsert + 1;
                else
                    TicInsert = tic;
                    [~, Error]=imProc.db.insertCatalog(ObjNew,'Header',AH,'ColNameDic',Columns,'Db',DB,'DbName',Args.DbName,'DbTable',Args.DbTable,...
                        'ColSrcID',Args.ColNameID,'KeyID',Args.KeyID,...
                        'CreateCsv',false,'DBConnector',Args.DBConnector,'Schema',Args.Schema, 'MaxBatchLines',200000,'Verbosity',0); 
                    InsertSec = toc(TicInsert);
                    VisitInsertSec = VisitInsertSec + InsertSec;
                    perfLog(sprintf('%s insertCatalog %s', FnName, DataDir), InsertSec);
                    if ~isempty(Error)
                        error('catalog injection failed');
                    end
                    CntInserted = CntInserted + 1;
                end
                if Args.UpdateStatus
                    StampLine = sprintf('%s injected into the visit asteriods catalog DB', tools.timeStamp.getTimeStamp);
                    pipeline.last.insertDB.visitStatusPg('append', FnName, VisitStatusDB, DataDir, StampLine, true);
                end
            end
            VisitStats = perfVisitEnd_(VisitStats, TicVisit, VisitInsertSec, 'injected', FnName, DataDir);
        else
            cd(Dir);
            CntSkippedInjected = CntSkippedInjected + 1;
        end                        
    end
    fclose(FID);    
    % disconnect the DB     
    if strcmpi(Args.DBConnector,'legacy')
        DB.disconnectCH_Java;
    else
        DB.delete;
    end
    pipeline.last.insertDB.perf.perfVisitStats('logSummary', VisitStats, FnName);
    perfLog(sprintf('%s SUMMARY skippedInjected=%d noStatus=%d inserted=%d skipInsert=%d', ...
        FnName, CntSkippedInjected, CntNoStatus, CntInserted, CntSkipInsert));
    perfLog(sprintf('%s FINISH', FnName), toc(RunT0));
end

function VisitStats = perfVisitEnd_(VisitStats, TicVisit, VisitInsertSec, Outcome, FnName, DataDir)
    TotalSec = toc(TicVisit);
    fprintf(' ..visit done in %.1fs (log: insertDB_perf.log)\n', TotalSec);
    VisitStats = pipeline.last.insertDB.perf.perfVisitStats('endVisit', VisitStats, TotalSec, VisitInsertSec, Outcome);
    perfLog(sprintf('%s visitEnd %s %s', FnName, Outcome, DataDir), TotalSec);
    pipeline.last.insertDB.perf.perfLogBlank();
end
function perfLog(Message, Seconds)
    if nargin < 2
        pipeline.last.insertDB.perf.perfLog(Message);
    else
        pipeline.last.insertDB.perf.perfLog(Message, Seconds);
    end
end
