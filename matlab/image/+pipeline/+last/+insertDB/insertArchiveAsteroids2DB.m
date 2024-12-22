function [Result] = insertArchiveAsteroids2DB(RootDir, FileNameTemplate, Args)
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
    %          pipeline.last.insertArchiveAsteroids2DB(RootDir,Template)    
    %    
    %          pipeline.last.insertArchiveAsteroids2DB('/mnt/marvin/LAST.01.02.01/','ProcDirTemplate','*/*/*/proc/*')
    %          pipeline.last.insertArchiveAsteroids2DB('/mnt/marvin/','ProcDirTemplate','LAST.01.02*/*/*/*/proc/*')
    %
    arguments
        RootDir                = '/Data1/LAST.01.01.01/';
        FileNameTemplate       = 'LAST*coadd_Aster*.mat';      
%         Args.FileNameCoaddIma  = 'LAST*coadd_Ima*.fits';
        Args.ProcDirTemplate   = '*/*/*/proc/*';  
        
        Args.Template          = '~/matlab/data/db/Design-Database-Pipeline-ClickHouse.xlsx';
        
        Args.DbHost = 'socsrv';
        Args.DbName = 'last';   
        Args.DbUser = 'default';
        Args.DbPass = 'PassRoot'; 
        
        Args.Level  = 'coadd';
        Args.DbTable= 'visit_asteroids';   
        Args.KeyID     = 'id_visit_im';
        Args.ColNameID = 'id_visit_src';        
        
        Args.RemoteUser = 'samar';
    end    
    % create a DB object and connect
    DB          = db.Db;
    DB.Host     = Args.DbHost;
    DB.DbName   = Args.DbName;
    DB.User     = Args.DbUser;
    DB.Password = Args.DbPass;
    DB.Conn;
    DB.useDB(Args.DbName);
    fprintf('DB in use: %s\n',DB.showCurrentDB);
    fprintf('Table list: '); fprintf('%s ',DB.showTables); fprintf('\n');        
    % read the column list from the xls template  
    Columns = db.util.read_xls2tableFormat(Args.Template,'Sheet','Sources','TableName',Args.DbTable);   
    %
    Dir = pwd; 
    FID = fopen('aster_no_status_dir.txt', 'a');
    tic
    % find all the directories according to the template
    D = dir(fullfile(RootDir, Args.ProcDirTemplate));
    Dirs = D([D.isdir]);
    Dirs = Dirs(~ismember({Dirs.name}, {'.', '..'})); 
    % 
    Ndir = numel(Dirs);
    for Crop = 1:Ndir
        DataDir = strcat(Dirs(Crop).folder,'/',Dirs(Crop).name);         
        cd(DataDir);    
        try
            Injected = contains(fileread('.status'), "injected into the coadd asteriods catalog DB");
        catch
            cd(Dir);
            fprintf(FID,'%s \n',DataDir);
            continue
        end
        if ~Injected
            try
                load(dir(FileNameTemplate).name,'');
                Obj.Table.Properties.VariableNames{'SubImageIndex'} = 'cropid'; % repair the column name
                Headers=dir('*coadd*Cat*');
                AH=AstroHeader(Headers(1).name,3);
            catch
                cd(Dir);
                fprintf(FID,'%s \n',DataDir);
                continue
            end
            cd(Dir);
            fprintf('Injecting from %s ..',DataDir);
            % check and add essential KEYWORDS if they are missing                  
            Pname = AH.getStructKey('PROJNAME').PROJNAME;
            if isnan(AH.getStructKey('NODENUMB').NODENUMB)
                NODENUMB = str2num(Pname(6:7));
                AH.replaceVal('NODENUMB',NODENUMB);
            end
            if isnan(AH.getStructKey('MOUNTNUM').MOUNTNUM)
                MOUNTNUM = str2num(Pname(9:10));
                AH.replaceVal('MOUNTNUM',MOUNTNUM);                
            end
            Subdir = AH.getStructKey('SUBDIR').SUBDIR; 
            if isempty(Subdir)          
                Parts  = strsplit(DataDir, '/');
                Subdir = Parts{end};    % Extract the last part of the full dir name                
                AH.replaceVal('SUBDIR',Subdir);
            end
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
            CsvFN = A.genFile;                                                      

            T=imProc.db.insertCatalog(Obj,'Header',AH,'ColNameDic',Columns,'Db',DB,'DbName',Args.DbName,'DbTable',Args.DbTable,...
                                    'CreateCsv',true,'FileName',CsvFN,'ColSrcID',Args.ColNameID,'KeyID',Args.KeyID);
            
            % copy the CSV file into the proc catalog and edit the .status file
            CopyCSV = sprintf('su - %s -c "cp -f %s/%s %s"',Args.RemoteUser,Dir,CsvFN,DataDir);
            [~, Err1] = system(CopyCSV);            
            UpdateStatus = sprintf('su - %s -c "echo ''%s injected into the coadd asteriods catalog DB'' >> %s/.status"',...
                                    Args.RemoteUser,tools.timeStamp.getTimeStamp,DataDir);
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
