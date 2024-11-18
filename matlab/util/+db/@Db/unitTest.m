function Result = unitTest()

% testing DB-related functionality with Coadd images of LAST
        
    % read a LAST Coadd object from .mat archive or 24 FITS image files
%     load('~/coadd_db_test.mat'); % 'Coadd','FN_Coadd','RawImageList'

    % read a list of proc catalogs on Marvin:
    ProcDirs = readlines('proclist_sorted.txt');
    Ndirs    = size(ProcDirs,1);
    
    % create a DB object and connect
    DB = db.Db;
    DB.Host     = 'socsrv';
    DB.DbName   = 'last';
    DB.User     = 'default';
    DB.Password = 'PassRoot';
    DB.Conn;
    DB.useDB('last');
    fprintf('DB in use: %s\n',DB.showCurrentDB);
    fprintf('Table list: '); fprintf('%s ',DB.showTables); fprintf('\n');
    
    CoaddImageTable = 'visit_images';
    ColNameID       = 'id_visit';
    
    tic
    
    Dir = pwd;
    
    for Idir = 1:1 % Ndirs  % loop over the directories 
        
%         DataDir = ProcDirs(Idir);
        
        DataDir = '/mnt/marvin/LAST.01.01.01/2024/11/01/proc/001225v0/';
        
        cd(DataDir);
        
        if ~contains(fileread('.status'), "injected into the visit image DB")
            Coadd=AstroImage('LAST*coadd_Image_1.fits'); % read the data
            cd(Dir);
            fprintf('Injecting from %s ..',DataDir);
            
            A = AstroFileName;
            A.ProjName = Coadd(1).getStructKey('PROJNAME').PROJNAME;
            A.SubDir   = Coadd(1).getStructKey('SUBDIR').SUBDIR;
            A.Level    = Coadd(1).getStructKey('LEVEL').LEVEL;
            A.FieldID  = Coadd(1).getStructKey('FIELDID').FIELDID;
            A.JD       = Coadd(1).getStructKey('JD').JD; 
            A.CCDID = 1; A.Counter = 0; A.CropID = 0; 
            A.FileType = "csv";
            A.julday2time;
            CsvFN = sprintf("%s.csv",A.genFile);
            
            Columns = db.util.read_xls2tableFormat('~/matlab/data/db/Design-Database-Pipeline-ClickHouse.xlsx',...
                'Sheet','Images','TableName','visit_images');            

            T=imProc.db.insertImages(Coadd,'ColNameDic',Columns,'Db',DB,'DbName','last','DbTable',...
                CoaddImageTable,'CreateCsv',true,'FileName',CsvFN, 'ColNameID',ColNameID);
            
            % copy the CSV file into the proc catalog and edit the .status file
            CopyCSV = sprintf('su - samar -c "cp ~sasha/%s %s"',CsvFN,DataDir);
            [~, Err1] = system(CopyCSV);            
            UpdateStatus = sprintf('su - samar -c "echo ''%s injected into the visit image DB'' >> %s/.status"',tools.timeStamp.getTimeStamp,DataDir);
            [~, Err2] = system(UpdateStatus); 
            % system(rm fprintf('%s',CsvFN));
            fprintf(' ..done\n');  
        else
            cd(Dir); 
        end                        
    end
                                         
    toc

    % disconnect the DB     
    DB.disconnectCH_Java;    
end


            % Coadd.setKeyVal('CAMNAME',int16(999)); % column type mismatch
            % Coadd.setKeyVal('ID_DARK',int16(999)); % dealing with NaNs is in db.Db.table2csv
            
            % convert char representations of ID_DARK and ID_FLAT back to int64:
%             Coadd.setKeyVal('ID_DARK',int64([Coadd.getStructKey('ID_DARK').ID_DARK])); % converter moved to the Excel template;
%             Coadd.setKeyVal('ID_FLAT',int64([Coadd.getStructKey('ID_FLAT').ID_FLAT]));
            