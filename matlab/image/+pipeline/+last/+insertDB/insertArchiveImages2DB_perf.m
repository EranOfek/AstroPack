function [Result] = insertArchiveImages2DB_perf(RootDir, FileNameTemplate, Args)
    % insert the archived LAST images to DB 
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
    %          pipeline.last.insertDB.insertArchiveImages2DB(RootDir,Template)    
    %    
    %          pipeline.last.insertDB.insertArchiveImages2DB('/mnt/marvin/LAST.01.02.01/2024/10/12/','ProcDirTemplate','/proc/*')
    %          pipeline.last.insertDB.insertArchiveImages2DB('/mnt/marvin/','ProcDirTemplate','LAST.01.02*/*/*/*/proc/*')
    %          pipeline.last.insertDB.insertArchiveImages2DB('/mnt/marvin/','ProcDirTemplate','LAST*/2024/*/*/proc/*')
    %
    %          pipeline.last.insertDB.insertArchiveImages2DB('/mnt/euclid/last/data/references/','LAST*_Image_1.fits',...
    %                  'ProcDirTemplate','/v4/*','DbTable','ref_images','ColNameID','id_ref',...
    %                  'StatusStamp','Injected into the reference image table')
    %
    arguments
        RootDir                = '/mnt/marvin/LAST.01*/';
        FileNameTemplate       = 'LAST*coadd_Image_1.fits';          
        Args.ProcDirTemplate   = '/proc/*';
        Args.ProcDirList       = [];
        
        Args.Template          = '~/matlab/data/db/Design-Database-Pipeline-ClickHouse.xlsx';
         
        Args.DbHost = 'euclid';
        Args.DbName = 'last';   
        Args.DbUser = 'default';
        Args.DbPort =  9000;
        Args.AstroDBPassFile   = '~/.astropack/Passwords.yml';
        
        Args.DbTable     = 'visit_images'; % 'visit_images'
        Args.DbPassKey         = '';
        Args.ColMapTableName   = '';
        Args.ColNameID   = 'id_visit';
        Args.StatusStamp = "Injected into the visit image table"; % "injected into the visit image DB"; % "Injected into the visit image table";
               
        Args.RemoteUser  = 'euclid';
        Args.DBConnector = 'native'; % 'legacy'; % 'native' or 'legacy'
        Args.ConnectorOpts = struct('compression', db.mex.Compression.ZSTD);
        Args.Schema        = [];       % the user may input the schema of the DB table 
                                           % as a matlab table output of the "DESCRIBE TABLE" SQL command
        Args.SkipInsert        = false;
        Args.UpdateStatus      = true;
        Args.IgnoreStatus      = false;
    end    
    FnName = 'insertArchiveImages2DB_perf';
    RunT0 = tic;
    pipeline.last.insertDB.perfLogRunBanner(FnName);
    perfLog(sprintf('%s START RootDir=%s SkipInsert=%d UpdateStatus=%d IgnoreStatus=%d', ...
        FnName, RootDir, Args.SkipInsert, Args.UpdateStatus, Args.IgnoreStatus));
    CntSkippedInjected = 0;
    CntNoStatus = 0;
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
    % read the column list from the xls template
    TicXls = tic;
    TableNameForXls = Args.DbTable;
    if ~isempty(Args.ColMapTableName)
        TableNameForXls = Args.ColMapTableName;
    end
    Columns = db.util.read_xls2tableFormat(Args.Template,'Sheet','Images','TableName',TableNameForXls);
    perfLog(sprintf('%s readXlsTemplate', FnName), toc(TicXls));
    %
    Dir = pwd; 
    FID = fopen('no_status_dir.txt', 'a');
    % find all the directories according to the template or read from Args
    TicDir = tic;
    if isempty(Args.ProcDirList)
        D = dir(fullfile(RootDir, Args.ProcDirTemplate));
        Dirs = D([D.isdir]);
        Dirs = Dirs(~ismember({Dirs.name}, {'.', '..'}));
        Dirs = Dirs(contains({Dirs.name}, 'v0')); % comment this out for making ref_images table
        Dirs = Dirs(~contains({Dirs.folder},'_re'));
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
        if Args.IgnoreStatus
            Injected = false;
        else
            try
                TicStatus = tic;
                Injected = contains(fileread('.status'), Args.StatusStamp);
                perfLog(sprintf('%s readStatus %s', FnName, DataDir), toc(TicStatus));
            catch
                cd(Dir);
                fprintf(FID,'%s \n',DataDir);
                CntNoStatus = CntNoStatus + 1;
                VisitStats = perfVisitEnd_(VisitStats, TicVisit, VisitInsertSec, 'noStatus', FnName, DataDir);
                continue
            end
        end
        if ~Injected
            TicRead = tic;
            R=AstroImage(FileNameTemplate); % read the data
            Coadd=R(~R.isemptyImage);
            Nobj = numel(Coadd);
            perfLog(sprintf('%s readImages Nobj=%d %s', FnName, Nobj, DataDir), toc(TicRead));
            if Nobj < 1 || Coadd(1).isemptyImage % no images have been read 
                cd(Dir);
                fprintf(FID,'%s \n',DataDir);
                VisitStats = perfVisitEnd_(VisitStats, TicVisit, VisitInsertSec, 'noData', FnName, DataDir);
                continue
            end
            cd(Dir);
            TicPrep = tic;
            
            % check and add essential KEYWORDS if they are missing             
            Pname = Coadd(1).getStructKey('PROJNAME').PROJNAME;
            if isnan(Coadd(1).getStructKey('NODENUMB').NODENUMB)
                NODENUMB = str2num(Pname(6:7));
                for Crop=1:Nobj
                    Coadd(Crop).HeaderData.replaceVal('NODENUMB',NODENUMB);
                end
            end
            if isnan(Coadd(1).getStructKey('MOUNTNUM').MOUNTNUM)
                MOUNTNUM = str2num(Pname(9:10));
                for Crop=1:Nobj
                    Coadd(Crop).HeaderData.replaceVal('MOUNTNUM',MOUNTNUM);
                end
            end
            Subdir = Coadd(1).getStructKey('SUBDIR').SUBDIR; 
            if isempty(Subdir)          
                Parts = strsplit(DataDir, '/');
                Subdir = Parts{end};    % Extract the last part of the full dir name
                for Crop=1:Nobj
                    Coadd(Crop).HeaderData.replaceVal('SUBDIR',Subdir);
                end
            end         
            % insert the ingestion time
            JDnow = celestial.time.date2jd;
            for Crop=1:Nobj
                Coadd(Crop).HeaderData.replaceVal('INGESTION_TIME_JD',JDnow);
            end
            
            % prepare file name for the CSV dump
                A = AstroFileName;
                A.ProjName = Pname;
                A.SubDir   = Subdir;
                A.Level    = Coadd(1).getStructKey('LEVEL').LEVEL;
                A.FieldID  = Coadd(1).getStructKey('FIELDID').FIELDID;
                A.JD       = Coadd(1).getStructKey('JD').JD;
                A.CCDID = 1; A.Counter = 0; A.CropID = 0;
                A.FileType = "csv"; A.julday2time;
                CsvFN = erase(A.genFile,' ');
            
            % add the keywords to be used for filename construction            
            for Crop = 1:Nobj
                FN = Coadd(Crop).HeaderData.getStructKey('FILENAME').FILENAME;
                if ~isnan(FN)
                    Parts = strsplit(FN, '/');
                    FN = Parts{end};
                else
                    FN = char(CsvFN);
                end
                Coadd(Crop).HeaderData.replaceVal('FILETIME',FN(24:33));                
                DateTime0 = datetime(FN(15:25), 'InputFormat', 'yyyyMMdd.HH');
                if DateTime0.Hour < 12
                    DateTime = DateTime0-1;                    
                else
                    DateTime = DateTime0;
                end                
                Coadd(Crop).HeaderData.replaceVal('DIRYEAR',DateTime.Year);
                Coadd(Crop).HeaderData.replaceVal('DIRMON' ,DateTime.Month);
                Coadd(Crop).HeaderData.replaceVal('DIRDAY' ,DateTime.Day);                
            end
            perfLog(sprintf('%s keywordPrep %s', FnName, DataDir), toc(TicPrep));

            if strcmpi(Args.DBConnector,'legacy')                                
                if Args.SkipInsert
                    perfLog(sprintf('%s SKIP insertImages %s', FnName, DataDir));
                    CntSkipInsert = CntSkipInsert + 1;
                else
                    TicInsert = tic;
                    [~, Error]=imProc.db.insertImages(Coadd,'ColNameDic',Columns,'Db',DB,'DbName',Args.DbName,'DbTable',Args.DbTable,...
                        'CreateCsv',true,'FileName',CsvFN, 'ColNameID',Args.ColNameID);
                    InsertSec = toc(TicInsert);
                    VisitInsertSec = VisitInsertSec + InsertSec;
                    perfLog(sprintf('%s insertImages %s', FnName, DataDir), InsertSec);
                    if ~isempty(Error)
                        error('image injection failed');
                    end
                    CntInserted = CntInserted + 1;
                end
                % copy the CSV file into the proc catalog and edit the .status file
                if Args.UpdateStatus
                    CopyCSV = sprintf('su - %s -c "cp -f %s/%s %s"',Args.RemoteUser,Dir,CsvFN,DataDir);
                    [~, Err1] = system(CopyCSV);
                    TicSu = tic;
                    UpdateStatus = sprintf('su - %s -c "echo ''%s %s'' >> %s/.status"',...
                        Args.RemoteUser,tools.timeStamp.getTimeStamp,Args.StatusStamp,DataDir);
                    [~, Err2] = system(UpdateStatus);
                    perfLog(sprintf('%s updateStatus %s', FnName, DataDir), toc(TicSu));
                    if isempty(Err1) && isempty(Err2)
                        RemLocalFile = sprintf('rm %s',CsvFN);
                        [~, Err3] = system(RemLocalFile);
                    end
                end                        
            else
                if Args.SkipInsert
                    perfLog(sprintf('%s SKIP insertImages %s', FnName, DataDir));
                    CntSkipInsert = CntSkipInsert + 1;
                else
                    TicInsert = tic;
                    [~, Error]=imProc.db.insertImages(Coadd,'ColNameDic',Columns,'Db',DB,'DbName',Args.DbName,'DbTable',Args.DbTable,...
                        'CreateCsv',false,'ColNameID',Args.ColNameID,...
                        'DBConnector',Args.DBConnector,'Schema',Args.Schema);
                    InsertSec = toc(TicInsert);
                    VisitInsertSec = VisitInsertSec + InsertSec;
                    perfLog(sprintf('%s insertImages %s', FnName, DataDir), InsertSec);
                    if ~isempty(Error)
                        error('image injection failed');
                    end
                    CntInserted = CntInserted + 1;
                end
                if Args.UpdateStatus
                    TicSu = tic;
                    UpdateStatus = sprintf('su - %s -c "echo ''%s %s'' >> %s/.status"',...
                        Args.RemoteUser,tools.timeStamp.getTimeStamp,Args.StatusStamp,DataDir);
                    [~, Err2] = system(UpdateStatus);
                    perfLog(sprintf('%s updateStatus %s', FnName, DataDir), toc(TicSu));
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
    pipeline.last.insertDB.perfVisitStats('logSummary', VisitStats, FnName);
    perfLog(sprintf('%s SUMMARY skippedInjected=%d noStatus=%d inserted=%d skipInsert=%d', ...
        FnName, CntSkippedInjected, CntNoStatus, CntInserted, CntSkipInsert));
    perfLog(sprintf('%s FINISH', FnName), toc(RunT0));
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
