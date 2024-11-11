function Result = unitTest()

% testing DB-related functionality with Coadd images of LAST
    
    % create a DB object and connect

    DB = db.Db;
    DB.Host   = 'socsrv';
    DB.DbName = '';
    DB.User   = 'default';
    DB.Password = 'PassRoot';
    DB.Conn;
    fprintf('DB in use: %s\n',DB.showCurrentDB);
    fprintf('Table list: %s\n',DB.showTables);
    
    % read a LAST Coadd object from .mat archive or 24 FITS image files

    load('~/coadd_db_test.mat'); % 'Coadd','FN_Coadd','RawImageList' 
    
%     Coadd=AstroImage('LAST*coadd_Image_1.fits');
    
    % convert the headers into a CSV 

    CoaddImageTable = [];
    
    CsvFN = 'test.csv'; % should be created with the AstroFileName 
        
    Columns = db.util.read_xls2tableFormat('~/matlab/data/db/Design-Database-Pipeline-ClickHouse.xlsx',...
                                            'Sheet','Images','TableName','visit_images');
    
    T=imProc.db.insertImages(Coadd,'ColNameDic',Columns,'Db',DB,'DbTable',CoaddImageTable,'CreateCsv',true,'FileName',CsvFN);
    
    % disconnect DB
    
    DB.disconnectCH_Java

end