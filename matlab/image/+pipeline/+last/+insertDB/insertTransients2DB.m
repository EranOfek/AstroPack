function [Result] = insertTransients2DB(Cat, Headers, Args)
    % insert the catalog of transients into a DB 
    %     this is intended to be used in real time within the pipeline 
    % Input  : - transients' catalog   
    %          - AstroHeaders of the "new" images 
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
        Cat
        Headers
        
        Args.Template = '~/matlab/data/db/Design-Database-Pipeline-ClickHouse.xlsx';
        
        Args.DB     = [];
        
        Args.DbHost = 'socsrv';
        Args.DbName = 'last';   
        Args.DbUser = 'default';
        Args.DbPass = ''; 
        
        Args.Level  = 'coadd';
        Args.DbTable= 'diff_src';     %  
        Args.KeyID     = 'id_new_im'; % 'id_visit_im' ???  
        Args.ColNameID = 'id_diff_src';                        
    end    
    % create a DB object and connect or use a preloaded object with connection
    if isempty(Args.DB)        
        DB          = db.Db;
        DB.Host     = Args.DbHost;
        DB.DbName   = Args.DbName;
        DB.User     = Args.DbUser;
        DB.Password = Args.DbPass;
        DB.Conn;    
    else
        DB = Args.DB;
    end    
    DB.useDB(Args.DbName);
    % read the column list from the xls template  
    Columns = db.util.read_xls2tableFormat(Args.Template,'Sheet','Sources','TableName',Args.DbTable);   
    %    
    if height(Cat.Table) < 1              % if the table is empty, skip to the next visit
        return; % continue
    end

    NumTran = size(Cat.Catalog,1);
    OnesArray = ones(NumTran,1);

    UTCNow = datetime('now', 'TimeZone', 'UTC');
    JDNow = juliandate(UTCNow)*OnesArray;

    Cat.insertCol(cell2mat({cast(JDNow,'double')}), inf, ...
        {'ingestion_time_jd'}, {'jd'});

    % chop into objects with single CropID 
    CropID = unique(Cat.Table.CROPID);
    NCrop  = numel(CropID);
    CatByCrop  = struct([]);
    
    NHeaders = numel(Headers);
    HeadByCrop = repmat(AstroHeader,1,NCrop);
    HeaderCrop = zeros(NHeaders,1);
    
    for IHeader = 1:NHeaders
        IHeaderCrop = Headers(IHeader).getVal('CROPID');
        HeaderCrop(IHeader) = IHeaderCrop;
    end
    
    for Icrop = 1:NCrop
        CurrentCrops = Cat.Table.CROPID == CropID(Icrop);
        CatByCrop(Icrop).Table = Cat.Table(CurrentCrops, :); % select the lines by cropid
        HeadByCrop(Icrop) = Headers(HeaderCrop == CropID(Icrop)); % for each cropid read the appropriate header
    end
    %    
    CsvFN = sprintf('/tmp/tempDBinsert%.20f.csv',rand); % temporary csv file name
    %
    [~, Error,~]=imProc.db.insertCatalog(CatByCrop,'Header',HeadByCrop,'ColNameDic',Columns,'Db',DB,'DbName',Args.DbName,'DbTable',Args.DbTable,...
        'CreateCsv',true,'FileName',CsvFN,'ColSrcID',Args.ColNameID,'KeyID',Args.KeyID,'DeleteFile',1);
    if ~isempty(Error)
        Result = Error;
    else
        Result = [];
    end
    % disconnect the DB
    if isempty(Args.DB)
        DB.disconnectCH_Java;
    end
end
