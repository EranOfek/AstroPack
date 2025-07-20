function [Result] = insertArchiveRawImages2DB(RootDir, FileNameTemplate, Args)
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
    % Example: RootDir = '/mnt/euclid/last/data/LAST.01.01.01/2023/04/24/'; 
    %          Template = '*raw*Ima*fits';
    %          pipeline.last.insertDB.insertArchiveRawImages2DB(RootDir,Template)    
    %
    arguments
        RootDir                = '/mnt/euclid/last/data/LAST.01.*/';
        FileNameTemplate       = 'LAST*raw_Image_1.fits';          
        Args.ProcDirTemplate   = 'raw';  
        Args.Decompress        = true;
        
        Args.Template          = '~/matlab/data/db/Design-Database-Pipeline-ClickHouse.xlsx';
        
        Args.DbHost = 'euclid';
        Args.DbName = 'last';   
        Args.DbUser = 'default';
        Args.AstroDBPassFile   = '~/.astropack/Passwords.yml'; 
        
        Args.DbTable     = 'raw_images'; 
        Args.ColNameID   = 'id_raw';
        Args.StatusStamp = "Injected into the raw image table"; % "injected into the visit image DB"; % "Injected into the visit image table";
        
        Args.RemoteUser  = 'euclid';
    end    
    % create a DB object and connect
    DB          = db.Db;
    DB.Host     = Args.DbHost;
    DB.DbName   = Args.DbName;
    DB.User     = Args.DbUser;
    Configuration.getSingleton().loadFile(Args.AstroDBPassFile); % tell the PM where to look for passwords
    PM = PasswordsManager;    
    DB.Password = PM.search(Args.DbName).Pass;
    DB.Conn;
    DB.useDB(Args.DbName);
    fprintf('DB in use: %s\n',DB.showCurrentDB);
    fprintf('Table list: '); fprintf('%s ',DB.showTables{:}); fprintf('\n');        
    % read the column list from the xls template
    Columns = db.util.read_xls2tableFormat(Args.Template,'Sheet','Images','TableName',Args.DbTable);      
    %
    Dir = pwd; 
    FID = fopen('no_status_dir.txt', 'a');
    tic
    % find all the directories according to the template
    AllDirs = strsplit(genpath(RootDir), pathsep);
    Dirs    = AllDirs(endsWith(AllDirs, Args.ProcDirTemplate));   
    % 
    Ndir = numel(Dirs);
    for Idir = 1:Ndir
        DataDir = Dirs{Idir};
        cd(DataDir);    
        try
            Injected = contains(fileread('.status'), Args.StatusStamp);
        catch
            cd(Dir);
            fprintf(FID,'%s \n',DataDir);
            continue
        end
        if ~Injected
            % decompress the data files if requested:
            if Args.Decompress
                Decompress = sprintf('su %s -c "funpack %s.fz"',Args.RemoteUser,FileNameTemplate);
                [~, Err.Decompress] = system(Decompress); 
            end     
            Raw=AstroHeader(FileNameTemplate); % read the data
            Nobj = numel(Raw);
            if Nobj < 1                        % no headers have been read 
                cd(Dir);
                fprintf(FID,'%s \n',DataDir);
                continue
            end
            cd(Dir);
            fprintf('Injecting from %s ..',DataDir);
            
            % check and add essential KEYWORDS if they are missing             
            FN = Raw(1).getStructKey('FILENAME').FILENAME;
            if isnan(Raw(1).getStructKey('NODENUMB').NODENUMB)
                NODENUMB = str2num(FN(6:7));
                for Crop=1:Nobj
                    Raw(Crop).replaceVal('NODENUMB',NODENUMB);
                end
            end
            if isnan(Raw(1).getStructKey('MOUNTNUM').MOUNTNUM)
                MOUNTNUM = str2num(FN(9:10));
                for Crop=1:Nobj
                    Raw(Crop).replaceVal('MOUNTNUM',MOUNTNUM);
                end
            end            
            % insert the ingestion time
            JDnow = celestial.time.date2jd;
            for Crop=1:Nobj
                Raw(Crop).replaceVal('INGESTION_TIME_JD',JDnow);
            end
            % prepare file name for the CSV dump 
            A = AstroFileName;
            A.ProjName = 'LAST';
            A.Level    = 'raw';
            A.FieldID  = Raw(1).getStructKey('OBJECT').OBJECT;
            A.JD       = Raw(1).getStructKey('JD').JD; 
            A.CCDID = 1; A.Counter = 0; A.CropID = 0; 
            A.FileType = "csv"; A.julday2time;
            CsvFN = erase(A.genFile,' ');        
            % add the keywords to be used for filename construction            
            for Crop = 1:Nobj                
                if ~isnan(FN)
                    Parts = strsplit(FN, '/');
                    FN = Parts{end};
                else
                    FN = char(CsvFN);
                end
                Raw(Crop).replaceVal('FILETIME',FN(24:33));                
                DateTime0 = datetime(FN(15:25), 'InputFormat', 'yyyyMMdd.HH');
                if DateTime0.Hour < 12
                    DateTime = DateTime0-1;                    
                else
                    DateTime = DateTime0;
                end                
                Raw(Crop).replaceVal('DIRYEAR',DateTime.Year);
                Raw(Crop).replaceVal('DIRMON' ,DateTime.Month);
                Raw(Crop).replaceVal('DIRDAY' ,DateTime.Day);                
            end

            [~, Error]=imProc.db.insertImages(Raw,'ColNameDic',Columns,'Db',DB,'DbName',Args.DbName,'DbTable',Args.DbTable,...
                                    'CreateCsv',true,'FileName',CsvFN, 'ColNameID',Args.ColNameID);
            if ~isempty(Error)
                error('image injection failed');
            end
            % copy the CSV file into the proc catalog and edit the .status file
            CopyCSV = sprintf('su - %s -c "cp -f %s/%s %s"',Args.RemoteUser,Dir,CsvFN,DataDir);
            [~, Err1] = system(CopyCSV);            
            UpdateStatus = sprintf('su - %s -c "echo ''%s %s'' >> %s/.status"',...
                                    Args.RemoteUser,tools.timeStamp.getTimeStamp,Args.StatusStamp,DataDir);
            [~, Err2] = system(UpdateStatus); 
            if isempty(Err1) && isempty(Err2)
                RemLocalFile = sprintf('rm %s',CsvFN);
                [~, Err3] = system(RemLocalFile);
            end
            fprintf(' ..done\n');  
        else
            cd(Dir); 
        end                        
    end
    toc
    fclose(FID);
    % disconnect the DB     
    DB.disconnectCH_Java;  
end
