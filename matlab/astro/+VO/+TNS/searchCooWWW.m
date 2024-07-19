function [Result] = searchCooWWW(RA, Dec, SearchRadius, Args)
    % One line description
    %     Optional detailed description
    % Input  : - 
    %          - 
    %          * ...,key,val,... 
    % Output : - 
    % Author : Eran Ofek (2024 Jul) 
    % Example: Result=VO.TNS.searchCooWWW('22:00:51', '+43:24:45')

    arguments
        RA
        Dec
        SearchRadius           = 5;
        Args.CooUnits          = 'deg';
        Args.SearchRadiusUnits = 'arcsec';
    end
    
    SearchRadiusAS = convert.angular(Args.SearchRadiusUnits, 'arcsec', SearchRadius);
    Nsrc = numel(RA);
    for Isrc=1:1:Nsrc
        if isnumeric(RA)
            switch lower(Args.CooUnits)
                case 'deg'
                    RA1 = celestial.coo.convertdms(RA(Isrc), 'd','SH');
                case 'rad'
                    RA1 = celestial.coo.convertdms(RA(Isrc), 'r','SH');
                otherwise
                    error('Unknown CooUnits option');
            end
        elseif ischar(RA)
            RA1 = RA;
        elseif isstring(RA) || iscell(RA)
            RA1 = RA{1};
        else
            error('Unknown RA type');
        end
        
        if isnumeric(Dec)
            switch lower(Args.CooUnits)
                case 'deg'
                    Dec1 = celestial.coo.convertdms(RA(Isrc), 'd','SD');
                case 'rad'
                    Dec1 = celestial.coo.convertdms(RA(Isrc), 'r','SD');
                otherwise
                    error('Unknown CooUnits option');
            end
        elseif ischar(Dec)
            Dec1 = Dec;
        elseif isstring(RA) || iscell(RA)
            Dec1 = Dec{1};
        else
            error('Unknown Dec type');
        end
        
        TNS_ID     = 159514;
        TNS_Type   = 'bot';
        TNS_Name   = 'LAST_Bot1';
        TNS_APIKEY = 'df99d99d706602e781756e26231033f857111e2d';
        TNS_Server = 'https://www.wis-tns.org/api/get/search';
        
        %LD_Path = sprintf('set LD_LIBRARY_PATH "%s"; ',getenv('LD_LIBRARY_PATH'));
        %LD_Path = sprintf('set LD_LIBRARY_PATH ""; ');%,getenv('LD_LIBRARY_PATH'));
        LD_Path = getenv('LD_LIBRARY_PATH');
        LD_Path = [LD_Path, ':', '/usr/lib/x86_64-linux-gnu'];  %/usr/local/MATLAB/R2021a/bin/glnxa64'];
        setenv('LD_LIBRARY_PATH',LD_Path);
        
        LD_Path = '';
        %CommandStr = sprintf('%s curl -X POST -H ''user-agent: tns_marker{"tns_id":%d,"type": "%s", "name":"%s"}'' -d ''api_key=%s&data={"ra": "%s",  "dec": "%s", "radius": "%f", "units": "arcsec"}'' %s', LD_Path, TNS_ID, TNS_Type, TNS_Name, TNS_APIKEY, RA1, Dec1, SearchRadiusAS, TNS_Server);
        CommandStr = sprintf('curl -X POST -H ''user-agent: tns_marker{"tns_id":%d,"type": "%s", "name":"%s"}'' -d ''api_key=%s&data={"ra": "%s",  "dec": "%s", "radius": "%f", "units": "arcsec"}'' %s', TNS_ID, TNS_Type, TNS_Name, TNS_APIKEY, RA1, Dec1, SearchRadiusAS, TNS_Server);
        [Status, OutputJ] = system(CommandStr);
        
        JSt = jsondecode(OutputJ);
        Result = JSt.data.reply;
        
    end
        
end
