function [Result] = downloadAll(X, Y, Args)
    % One line description
    %     Optional detailed description
    % Input  : - 
    %          - 
    %          * ...,key,val,... 
    % Output : - 
    % Author : Eran Ofek (2024 Jul) 
    % Example: 

    arguments
        X
        Y
        Args.A                 = [];
        Args.B                 = [];
    end
    
    TNS_ID     = 159514;
    TNS_Type   = 'bot';
    TNS_Name   = 'LAST_Bot1';
    TNS_APIKEY = 'df99d99d706602e781756e26231033f857111e2d';
    %TNS_Server = 'https://www.wis-tns.org/api/get/search';
    TNS_File    = 'https://www.wis-tns.org/system/files/tns_public_objects/tns_public_objects.csv.zip';
    LocalFile   = 'tns_public_objects.csv.zip';
    
    CommandStr = sprintf('curl -X POST -H ''user-agent: tns_marker{"tns_id":%d,"type": "%s", "name":"%s"}'' -d ''api_key=%s'' %s > %s', TNS_ID, TNS_Type, TNS_Name, TNS_APIKEY, TNS_File, LocalFile);
    [Stat1, OutStr] = system(CommandStr);
    
    unzip(LocalFile);
    Tbl = readtable(LocalFile(1:end-4));

    TNS_File = 'https://www.wis-tns.org/system/files/tns_public_objects/tns_public_objects_20240718.csv.zip'
    LocalFile = 'tns_public_objects_20240718.csv.zip'
    
    
    

end
