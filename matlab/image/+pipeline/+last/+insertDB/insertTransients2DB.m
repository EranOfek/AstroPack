function [Result] = insertTransients2DB(Cat, Headers, Args)
    % insert the catalog of transients into a DB 
    %     this is intended to be used in real time within the pipeline 
    % Input  : - transients' catalog   
    %          - AstroHeaders of the "new" images 
    %          * ...,key,val,... 
    %        'Template'        - template of tables' structure
    %        'Db*'             - database parameters
    %        'DbTable'         - DB table name
    %        'ColNameID'       - column name for the file unique ID    
    %                           
    % Output : - data injected into the DB
    % Author : A.M. Krassilchtchikov (2024 Dec) 
    % Example: pipeline.last.insertDB.insertTransients2DB(TCL1)    
    %
    arguments
        Cat
        Headers
        
        Args.Template          = '~/matlab/data/db/Design-Database-Pipeline-ClickHouse.xlsx';
        
        Args.DbHost = 'socsrv';
        Args.DbName = 'last';   
        Args.DbUser = 'default';
        Args.DbPass = 'PassRoot'; 
        
        Args.Level  = 'coadd';
        Args.DbTable= 'diff_src';     %  
        Args.KeyID     = 'id_new_im'; % 'id_visit_im' ???  
        Args.ColNameID = 'id_diff_src';                        
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
    fprintf('Table list: '); fprintf('%s ',DB.showTables{:}); fprintf('\n');        
    % read the column list from the xls template  
    Columns = db.util.read_xls2tableFormat(Args.Template,'Sheet','Sources','TableName',Args.DbTable);   
    %    
    if height(Cat.Table) < 1              % if the table is empty, skip to the next visit
        return; % continue
    end
    % chop into objects with single CropID 
    CropID = unique(Cat.Table.CROPID);
    NCrop  = numel(CropID);
    CatByCrop  = struct([]);
    HeadByCrop = repmat(AstroHeader,1,NCrop);
    for Icrop = 1:NCrop
        CatByCrop(Icrop).Table = Cat.Table(Cat.Table.CROPID == CropID(Icrop), :); % select the lines by cropid
        HeadByCrop(Icrop) = Headers(CropID(Icrop));                                    % for each cropid read the appropriate header
    end
    %    
    CsvFN = sprintf('/tmp/tempDBinsert%.20f.csv',rand); % temporary csv file name
    %
    T=imProc.db.insertCatalog(CatByCrop,'Header',HeadByCrop,'ColNameDic',Columns,'Db',DB,'DbName',Args.DbName,'DbTable',Args.DbTable,...
        'CreateCsv',true,'FileName',CsvFN,'ColSrcID',Args.ColNameID,'KeyID',Args.KeyID,'DeleteFile',1);
    % disconnect the DB
    DB.disconnectCH_Java;  
end
