function [Result] = insertArchiveImages2DB(RootDir, FileNameTemplate, Args)
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
    arguments
        RootDir                = '/mnt/marvin/LAST.01*/';
        FileNameTemplate       = 'LAST*coadd_Image_1.fits';          
        Args.ProcDirTemplate   = '/proc/*';  
        
        Args.Template          = '~/matlab/data/db/Design-Database-Pipeline-ClickHouse.xlsx';
        
        Args.DbHost = 'euclid';
        Args.DbName = 'last';   
        Args.DbUser = 'default';
        Args.AstroDBPassFile   = '~/.astropack/Passwords.yml'; 
        
        Args.DbTable     = 'visit_ima'; % 'visit_images'; % 'visit_ima'; 
        Args.ColNameID   = 'id_visit';
        Args.StatusStamp = "Injected into the visit image table"; % "injected into the visit image DB"; % "Injected into the visit image table";
        
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
    D = dir(fullfile(RootDir, Args.ProcDirTemplate));
    Dirs = D([D.isdir]);
    Dirs = Dirs(~ismember({Dirs.name}, {'.', '..'})); 
    Dirs = Dirs(contains({Dirs.name}, 'v0')); 
    Dirs = Dirs(~contains({Dirs.folder},'re'));
    % 
    Ndir = numel(Dirs);
    for Idir = 1:Ndir
        DataDir = strcat(Dirs(Idir).folder,'/',Dirs(Idir).name);         
        cd(DataDir);    
        try
            Injected = contains(fileread('.status'), Args.StatusStamp);
        catch
            cd(Dir);
            fprintf(FID,'%s \n',DataDir);
            continue
        end
        if ~Injected
            Coadd=AstroImage(FileNameTemplate); % read the data
            Nobj = numel(Coadd);
            if Nobj < 1 || Coadd(1).isemptyImage % no images have been read 
                cd(Dir);
                fprintf(FID,'%s \n',DataDir);
                continue
            end
            cd(Dir);
            fprintf('Injecting from %s ..',DataDir);
            
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
            % prepare file name for the CSV dump 
            A = AstroFileName;
            A.ProjName = Pname;
            A.SubDir   = Subdir;
            A.Level    = Coadd(1).getStructKey('LEVEL').LEVEL;
            A.FieldID  = Coadd(1).getStructKey('FIELDID').FIELDID;
            A.JD       = Coadd(1).getStructKey('JD').JD; 
            A.CCDID = 1; A.Counter = 0; A.CropID = 0; 
            A.FileType = "csv"; A.julday2time;
            CsvFN = A.genFile;      
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

            [~, Error]=imProc.db.insertImages(Coadd,'ColNameDic',Columns,'Db',DB,'DbName',Args.DbName,'DbTable',Args.DbTable,...
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
