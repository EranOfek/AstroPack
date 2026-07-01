function [Result] = insertArchiveCatalogs2DB(RootDir, FileNameTemplate, Args)
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
        Args.ColNameID         = 'id_proc_src';
        
        Args.RemoteUser        = 'euclid';
        Args.DBConnector       = 'native'; % 'legacy'; % 'native' or 'legacy'
        Args.ConnectorOpts     = struct('compression', db.mex.Compression.ZSTD);
        Args.Schema            = [];       % the user may input the schema of the DB table 
                                           % as a matlab table output of the "DESCRIBE TABLE" SQL command
    end    
    % create a DB object and connect   
    Configuration.getSingleton().loadFile(Args.AstroDBPassFile); % tell the PM where to look for passwords
    PM  = PasswordsManager;    
    Pwd = PM.search(Args.DbName).Pass;
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
%     fprintf('Table list: '); fprintf('%s ',DB.showTables{:}); fprintf('\n');        
    % read the column list from the xls template
    Columns = db.util.read_xls2tableFormat(Args.Template,'Sheet','Sources','TableName',Args.DbTable);   
    %
    Dir = pwd; 
    FIDnostatus     = fopen('cat_no_status_dir.txt', 'a');
    FIDnodata       = fopen('cat_no_data_dir.txt', 'a'); 
    FIDbrokendata   = fopen('cat_broken_data_dir.txt', 'a');
    
    % find all the directories according to the template or read from Args
    if isempty(Args.ProcDirList)
        D = dir(fullfile(RootDir, Args.ProcDirTemplate));
        Dirs = D([D.isdir]);
        Dirs = Dirs(~ismember({Dirs.name}, {'.', '..'}));
        Dirs = Dirs(contains({Dirs.name}, 'v0'));
        Dirs = Dirs(~contains({Dirs.folder},'re'));
    else
        Dirs = Args.ProcDirList;
    end
    % 
    Ndir = numel(Dirs);
    for Idir = 1:Ndir
        if isempty(Args.ProcDirList)
            DataDir = strcat(Dirs(Idir).folder,'/',Dirs(Idir).name);
        else
            DataDir = Dirs(Idir);
        end
        cd(DataDir);    
        try
            Injected = contains(fileread('.status'), "injected into the proc catalog DB") ...
                | contains(fileread('.status'), "not injectable into the proc catalog DB due to broken data files");
        catch
            cd(Dir);
            fprintf(FIDnostatus,'%s \n',DataDir);
            continue
        end
        if ~Injected
            % decompress the data files if requested:
            if Args.Decompress
                Decompress = sprintf('su %s -c "bunzip2 %s.bz2"',Args.RemoteUser,FileNameTemplate);
                [~, Err.Decompress] = system(Decompress); 
                Decompress = sprintf('su %s -c "xz -d %s.xz"',Args.RemoteUser,FileNameTemplate);
                [~, Err.Decompress] = system(Decompress); 
            end            
            %
            try
                Cat = AstroCatalog(FileNameTemplate,'UseMex',true); % read the data
                AH  = AstroHeader(FileNameTemplate,3,'UseMex',true);                                               
            catch
                cd(Dir);
                fprintf(FIDbrokendata,'%s \n',DataDir);
                continue
            end   
            cd(Dir);
             
            if numel(Cat) < 2 % likely no data have been read
                fprintf(FIDnodata,'%s \n',DataDir);
                continue
            end
            
            % remove the elements with insufficient number of columns:            
            NCol = max(arrayfun(@(x) size(x.Catalog, 2), Cat));
            Idx  = arrayfun(@(x) size(x.Catalog,2) < NCol, Cat);
            if Idx(1) % if the first catalog is broken, no Table properties have been read
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
            
            fprintf('Injecting from %s ..',DataDir); tic
            
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
                
                [~, Error]=imProc.db.insertCatalog(Cat,'Header',AH,'ColNameDic',Columns,'Db',DB,'DbName',Args.DbName,'DbTable',Args.DbTable,...
                    'CreateCsv',true,'FileName',CsvFN,'DBConnector',Args.DBConnector); % , 'ColNameID',Args.ColNameID);
                
                if ~isempty(Error)
                    error('catalog injection failed');
                end
                
                % move the CSV file into the proc catalog and edit the .status file
                CopyCSV = sprintf('su %s -c "cp -f %s/%s %s"',Args.RemoteUser,Dir,CsvFN,DataDir);
                [~, Err.Copy] = system(CopyCSV);
                UpdateStatus = sprintf('su %s -c "echo ''%s injected into the proc catalog DB'' >> %s/.status"',...
                    Args.RemoteUser,tools.timeStamp.getTimeStamp,DataDir);
                [~, Err.Update] = system(UpdateStatus);
                if isempty(Err.Copy) && isempty(Err.Update)
                    RemLocalFile = sprintf('rm %s',CsvFN);
                    [~, Err.RemoveLocal] = system(RemLocalFile);
                end                
            else
                [~, Error]=imProc.db.insertCatalog(Cat,'Header',AH,'ColNameDic',Columns,'Db',DB,'DbName',Args.DbName,'DbTable',Args.DbTable,...
                    'CreateCsv',false,'DBConnector',Args.DBConnector,'Schema',Args.Schema, 'MaxBatchLines',200000,'Verbosity',0); 
                
                if ~isempty(Error)
                    error('catalog injection failed');
                end
                % edit the .status file
                UpdateStatus = sprintf('su %s -c "echo ''%s injected into the proc catalog DB'' >> %s/.status"',...
                    Args.RemoteUser,tools.timeStamp.getTimeStamp,DataDir);
                [~, Err.Update] = system(UpdateStatus);
            end
                        
            fprintf(' ..done in %1.f s\n',toc);
            % compress the data files if requested:
            if Args.CompressProcessed
                Compress = sprintf('su %s -c "bzip2 %s/%s"',Args.RemoteUser,DataDir,FileNameTemplate);
                [~, Err.Decompress] = system(Compress); 
            end  
        else
            cd(Dir); 
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
    fprintf('Ingestion completed. \n');
end
