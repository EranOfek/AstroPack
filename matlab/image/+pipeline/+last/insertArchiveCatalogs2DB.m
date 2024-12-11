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
    %          pipeline.last.insertArchiveCatalogs2DB(RootDir,Template)    
    %    
    %          pipeline.last.insertArchiveCatalogs2DB('/mnt/marvin/LAST.01.01.01/2023/04/24/','ProcDirTemplate','/proc/*')
    %          pipeline.last.insertArchiveCatalogs2DB('/mnt/marvin/','ProcDirTemplate','LAST.01.02*/*/*/*/proc/*')
    %
    arguments
        RootDir                = '/Data1/LAST.01.01.01/';
        FileNameTemplate       = 'LAST*proc_Cat_1.fits';      
        Args.ProcDirTemplate   = '*/*/*/proc/*';  
        Args.Decompress        = true;
        Args.CompressProcessed = true;
        
        Args.Template          = '~/matlab/data/db/Design-Database-Pipeline-ClickHouse.xlsx';
        
        Args.DbHost = 'socsrv';
        Args.DbName = 'last';   
        Args.DbUser = 'default';
        Args.DbPass = 'PassRoot'; 
        
        Args.Level  = 'proc';
        Args.DbTable= 'proc_src';         
        Args.ColNameID = 'id_proc_src';
        
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
    FID = fopen('no_status_dir_cat.txt', 'a');
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
            Injected = contains(fileread('.status'), "injected into the proc catalog DB");
        catch
            cd(Dir);
            fprintf(FID,'%s \n',DataDir);
            continue
        end
        if ~Injected
            % decompress the data files if requested:
            if Args.Decompress
                Decompress = sprintf('su %s -c "bunzip2 %s.bz2"',Args.RemoteUser,FileNameTemplate);
                [~, Err.Decompress] = system(Decompress); 
            end            
            %
            Cat = AstroCatalog(FileNameTemplate); % read the data
            AH  = AstroHeader(FileNameTemplate,3);
            Nobj = numel(Cat);
            if Nobj < 2 % likely no data have been read 
                cd(Dir);
                fprintf(FID,'%s \n',DataDir);
                continue
            end
            cd(Dir);
            fprintf('Injecting from %s ..',DataDir);
            
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
            CsvFN = A.genFile;                                                      

            T=imProc.db.insertCatalog(Cat,'Header',AH,'ColNameDic',Columns,'Db',DB,'DbName',Args.DbName,'DbTable',Args.DbTable,...
                                    'CreateCsv',true,'FileName',CsvFN); % , 'ColNameID',Args.ColNameID);
            
            % copy the CSV file into the proc catalog and edit the .status file
            CopyCSV = sprintf('su %s -c "cp -f %s/%s %s"',Args.RemoteUser,Dir,CsvFN,DataDir);
            [~, Err.Copy] = system(CopyCSV);            
            UpdateStatus = sprintf('su %s -c "echo ''%s injected into the proc catalog DB'' >> %s/.status"',...
                                    Args.RemoteUser,tools.timeStamp.getTimeStamp,DataDir);
            [~, Err.Update] = system(UpdateStatus); 
            if isempty(Err.Copy) && isempty(Err.Update)
                RemLocalFile = sprintf('rm %s',CsvFN);
                [~, Err.RemoveLocal] = system(RemLocalFile);
            end
            fprintf(' ..done\n');
            % compress the data files if requested:
            if Args.CompressProcessed
                Decompress = sprintf('su %s -c "bzip2 %s/%s"',Args.RemoteUser,DataDir,FileNameTemplate);
                [~, Err.Decompress] = system(Decompress); 
            end  
        else
            cd(Dir); 
        end                        
    end
    toc
    fclose(FID);
    % disconnect the DB     
    DB.disconnectCH_Java;  
end
