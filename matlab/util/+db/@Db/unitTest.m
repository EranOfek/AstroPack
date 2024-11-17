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

    load('~/coadd_db_test.mat'); % 'Coadd','FN_Coadd','RawImageList' 
    
%     Coadd=AstroImage('LAST*coadd_Image_1.fits');
    
    % convert the headers into a CSV 

    CoaddImageTable = 'visit_images';
    
    CsvFN = 'test.csv'; % should be created with the AstroFileName 
        
    Columns = db.util.read_xls2tableFormat('~/matlab/data/db/Design-Database-Pipeline-ClickHouse.xlsx',...
                                            'Sheet','Images','TableName','visit_images');
                                        
                                        Coadd.setKeyVal('CAMNAME',int16(999)); % column type mismatch
                                        Coadd.setKeyVal('PIPEVER',int16(999));
                                        Coadd.setKeyVal('COADDOP',int16(999));
                                        
                                        Coadd.setKeyVal('ID_DARK',int16(999)); % dealing with NaNs 
                                        Coadd.setKeyVal('ID_FLAT',int16(999));
    
    T=imProc.db.insertImages(Coadd,'ColNameDic',Columns,'Db',DB,'DbName','last','DbTable',...
                             CoaddImageTable,'CreateCsv',true,'FileName',CsvFN,...
                             'ColNameID','id_visit');
    
    % disconnect DB
    
    DB.disconnectCH_Java

end