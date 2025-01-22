function [Result] = insertTranDBTransients2DB(Args)
    % insert the catalog of transients from TranDb.mat objects into a DB 
    %     this is intended to be used only once by hand to load the legacy DB of Ruslan 
    % Input  : 
    %          * ...,key,val,... 
    %        'Template'        - template of tables' structure
    %        'DB'              - a db.Db object with connection open
    %        'Db*'             - database parameters
    %        'DbTable'         - DB table name
    %        'ColNameID'       - column name for the file unique ID    
    %                           
    % Output : - data injected into the DB
    % Author : A.M. Krassilchtchikov (2024 Dec) 
    % Example: pipeline.last.insertDB.insertTransients2DB(TCL2, [Coadd.Headers])    
    %
    arguments              
        Args.Template = '~/matlab/data/db/Design-Database-Pipeline-ClickHouse.xlsx';
        
        Args.DbHost = 'last0';
        Args.DbName = 'last';   
        Args.DbUser = 'default';
        Args.DbPass = 'PassRoot'; 
        
        Args.Level  = 'coadd';
        Args.DbTable= 'diff_src';     %  
        Args.KeyID     = 'id_new_im'; % 
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
%     fprintf('DB in use: %s\n',DB.showCurrentDB);
%     fprintf('Table list: '); fprintf('%s ',DB.showTables{:}); fprintf('\n');        
    % read the column list from the xls template  
    Columns = db.util.read_xls2tableFormat(Args.Template,'Sheet','Sources','TableName',Args.DbTable);   
    %    
    load('TranDB.mat');
    Cat = TranDB;
    % chop into objects with single CropID 
    CropID = unique(Cat.Table.CROPID);
    NCrop  = numel(CropID);
    CatByCrop  = struct([]);
    HeadByCrop = repmat(AstroHeader,1,NCrop);
    HeadByCrop.insertKey('NODENUMB');
    HeadByCrop.insertKey('CAMNUM');
    HeadByCrop.insertKey('MOUNTNUM');
    HeadByCrop.insertKey('CROPID');
    HeadByCrop.insertKey('JD');
    HeadByCrop.insertKey({'IMTYPE','sci',''});
    HeadByCrop.insertKey({'LEVEL','coadd',''});
    for Icrop = 1:NCrop
        CatByCrop(Icrop).Table = Cat.Table(Cat.Table.CROPID == CropID(Icrop), :); % select the lines by cropid        
        HeadByCrop(Icrop).setVal('NODENUMB',1);
        HeadByCrop(Icrop).setVal('CAMNUM',CatByCrop(Icrop).Table.CAM(1));
        HeadByCrop(Icrop).setVal('MOUNTNUM',CatByCrop(Icrop).Table.MOUNT(1));
        HeadByCrop(Icrop).setVal('JD',CatByCrop(Icrop).Table.JD(1));
        HeadByCrop(Icrop).setVal('CROPID',CropID(Icrop));
    end   
    %    
    CsvFN = sprintf('/tmp/tempDBinsert%.20f.csv',rand); % temporary csv file name
    %
    [~,Error]=imProc.db.insertCatalog(CatByCrop,'Header',HeadByCrop,'ColNameDic',Columns,'Db',DB,'DbName',Args.DbName,'DbTable',Args.DbTable,...
        'CreateCsv',true,'FileName',CsvFN,'ColSrcID',Args.ColNameID,'KeyID',Args.KeyID,'DeleteFile',1);
    if ~isempty(Error)
        error('');
    end
    % disconnect the DB
    DB.disconnectCH_Java;
end
