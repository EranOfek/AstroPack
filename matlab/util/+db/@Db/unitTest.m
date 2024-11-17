function Result = unitTest()

% testing DB-related functionality with Coadd images of LAST
    
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
    
    % read a LAST Coadd object from .mat archive or 24 FITS image files

%     load('~/coadd_db_test.mat'); % 'Coadd','FN_Coadd','RawImageList'

    Dir = pwd;
    
    cd('/mnt/marvin/LAST.01.01.01/2024/11/01/proc/001225v0/'); 
    Coadd=AstroImage('LAST*coadd_Image_1.fits');   

%  sasha@WRX80:~$ ls -1 /mnt/marvin/LAST.*/202*/*/*/proc/ |wc
%  140582  137986 1470802  % number of visits 
%  sasha@WRX80:~$ ls -1 /mnt/marvin/LAST.*/202*/*/*/proc |grep -v v0 |wc
%    5529    2933  117737  % number of visits with reprocessed proc versions

    CoaddImageTable = 'visit_images';
    
    CsvFN = 'test.csv'; % should be created with the AstroFileName 
    
%     A = AstroFileName;
%     A.ProjName = Coadd(1).getStructKey('PROJNAME').PROJNAME;
%     A.SubDir = Coadd(1).getStructKey('SUBDIR').SUBDIR;
%     A.Level = Coadd(1).getStructKey('LEVEL').LEVEL;
%     A.FieldID = Coadd(1).getStructKey('FIELDID').FIELDID;
%     % A.Time = Coadd(1).getStructKey('DATEOBS').DATEOBS;
%     A.CCDID = 1;
%     A.Counter = 0;
%     A.FileType = "csv";
%     CsvFN = sprintf("%s.csv",A.genFile);
        
    Columns = db.util.read_xls2tableFormat('~/matlab/data/db/Design-Database-Pipeline-ClickHouse.xlsx',...
                                            'Sheet','Images','TableName','visit_images');
                                        
                                        Coadd.setKeyVal('CAMNAME',int16(999)); % column type mismatch
                                        Coadd.setKeyVal('PIPEVER',int16(999));
                                        Coadd.setKeyVal('COADDOP',int16(999));
                                        
%                                         Coadd.setKeyVal('ID_DARK',int16(999)); % dealing with NaNs is in db.Db.table2csv 
%                                         Coadd.setKeyVal('ID_FLAT',int16(999));

                                        % convert char representations of ID_DARK and ID_FLAT back to int64:                                       
                                        Coadd.setKeyVal('ID_DARK',int64([Coadd.getStructKey('ID_DARK').ID_DARK]));
                                        Coadd.setKeyVal('ID_FLAT',int64([Coadd.getStructKey('ID_FLAT').ID_FLAT]));
                                        
    T=imProc.db.insertImages(Coadd,'ColNameDic',Columns,'Db',DB,'DbName','last','DbTable',...
                             CoaddImageTable,'CreateCsv',true,'FileName',CsvFN,...
                             'ColNameID','id_visit');
    
    % disconnect DB
    
    DB.disconnectCH_Java
    
    cd(Dir);

end