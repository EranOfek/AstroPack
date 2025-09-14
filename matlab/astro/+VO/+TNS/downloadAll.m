function [T, CsvFile, ZipFile] = downloadAll(Args)
    % Download the TNS public objects CSV (ZIP), unzip, and read into a table.
    % Input  : * ...,key,val,...
    %            'TNS_APIKEY' -  string = ""          % <-- your API key (required)
    %            'TNS_ID' -      double = 159514
    %            'TNS_Type' -    string = "bot"
    %            'TNS_Name' -    string = "AstroPack_Bot1"
    %            'Url' -         string = "https://www.wis-tns.org/system/files/tns_public_objects/tns_public_objects.csv.zip"
    %            'OutDir' -      string = string(tempdir)   % where to save ZIP/CSV
    %            'ZipFileName' - string = "tns_public_objects.csv.zip"
    %            'CsvFileName' - string = ""                % optional rename of extracted CSV
    %            'TextType' -    string = "string"
    %            'KeepFiles' -   logical = false            % delete ZIP/CSV after reading if false
    %            'Verbose' -     logical = false
    % Output : - TNS Table
    %          - CSV file name.
    %          - Zip file name.
    % Author : ChatGPT, Eran Ofek (Sep 2025)
    % Example: [T, CsvFile, ZipFile]=VO.TNS.downloadAll;
    
    
    arguments
        Args.TNS_APIKEY  = "df99d99d706602e781756e26231033f857111e2d";
        Args.TNS_ID      = 159514
        Args.TNS_Type    = "bot"
        Args.TNS_Name    = "LAST_Bot1"
        Args.Url         = "https://www.wis-tns.org/system/files/tns_public_objects/tns_public_objects.csv.zip";
        Args.OutDir      = string(tempdir);
        Args.ZipFileName = "tns_public_objects.csv.zip"
        Args.CsvFileName = ""
        Args.TextType    = "string"
        Args.KeepFiles   = false
        Args.Verbose     = false
    end
    
    % --- TNS user-agent header ---
    Ua = sprintf('tns_marker{"tns_id":%d,"type":"%s","name":"%s"}', ...
                 Args.TNS_ID, char(Args.TNS_Type), char(Args.TNS_Name));
    
    % --- ensure output dir + target path ---
    if ~exist(Args.OutDir, 'dir'); mkdir(Args.OutDir); end
    ZipFile = fullfile(Args.OutDir, Args.ZipFileName);
    
    % --- POST and save ZIP like curl ---
    opts = weboptions( ...
        'HeaderFields', {'User-Agent' Ua; 'Accept' 'application/zip'}, ...
        'RequestMethod','post', ...
        'MediaType','application/x-www-form-urlencoded', ...
        'Timeout', 600);
    
    % IMPORTANT: Name,Value pair (no struct)
    ZipFile = websave(ZipFile, Args.Url, 'api_key', char(Args.TNS_APIKEY), opts);
    
    if Args.Verbose
        d = dir(ZipFile);
        fprintf('Saved ZIP: %s (%d bytes)\n', ZipFile, d.bytes);
    end
    
    % --- verify it’s a ZIP (magic 'PK') ---
    fid = fopen(ZipFile,'r'); assert(fid>=0, 'Could not open ZIP: %s', ZipFile);
    magic = fread(fid, 2, '*uint8'); fclose(fid);
    if numel(magic)<2 || ~(magic(1)==80 && magic(2)==75)
        % Not a ZIP — show first bytes for debugging
        txt = fileread(ZipFile);
        error('Downloaded content is not a ZIP. First ~600 chars:\n%s', extractBefore(txt, 600));
    end
    
    % --- unzip and pick first CSV ---
    files = unzip(ZipFile, Args.OutDir);
    idx = find(endsWith(string(files), ".csv", "IgnoreCase", true), 1, "first");
    assert(~isempty(idx), 'ZIP contained no .csv file.');
    CsvFile = string(files{idx});
    
    % optional rename
    if strlength(Args.CsvFileName) > 0
        NewCsv = fullfile(Args.OutDir, Args.CsvFileName);
        if ~strcmpi(CsvFile, NewCsv)
            if exist(NewCsv,'file'); delete(NewCsv); end
            movefile(CsvFile, NewCsv, 'f');
            CsvFile = string(NewCsv);
        end
    end
    
    if Args.Verbose
        fprintf('CSV extracted to: %s\n', CsvFile);
    end
    
    % --- read to table ---
    T = readtable(CsvFile, 'TextType', Args.TextType);
    
    % --- cleanup if desired ---
    if ~Args.KeepFiles
        try
            delete(ZipFile);
        end %#ok<TRYNC>
        try
            delete(CsvFile);
        end %#ok<TRYNC>
    end
end


% function [CsvText, TableOut] = downloadAll(Args)
%     % One line description
%     %     Optional detailed description
%     % Input  : - 
%     %          - 
%     %          * ...,key,val,... 
%     % Output : - 
%     % Author : Eran Ofek (2024 Jul) 
%     % Example: VO.TNS.downloadAll
% 
%     arguments
%     Args.TNS_APIKEY (1,1) string = ""   % <-- your API key
%     Args.TNS_ID     (1,1) double {mustBeFinite,mustBeNonnegative} = 159514
%     Args.TNS_Type   (1,1) string = "bot"
%     Args.TNS_Name   (1,1) string = "LAST_Bot1"
%     Args.Url        (1,1) string = ...
%         "https://www.wis-tns.org/system/files/tns_public_objects/tns_public_objects.csv.zip"
%     Args.Return     (1,1) string {mustBeMember(Args.Return,["text","table"])} = "text"
%     Args.Verbose    (1,1) logical = false
%         Args.A                 = [];
%         Args.B                 = [];
%     end
% 
%     TNS_ID     = 159514;
%     TNS_Type   = 'bot';
%     TNS_Name   = 'LAST_Bot1';
%     TNS_APIKEY = 'df99d99d706602e781756e26231033f857111e2d';
%     %TNS_Server = 'https://www.wis-tns.org/api/get/search';
%     TNS_File    = 'https://www.wis-tns.org/system/files/tns_public_objects/tns_public_objects.csv.zip';
%     LocalFile   = 'tns_public_objects.csv.zip';
% 
%     CommandStr = sprintf('curl -X POST -H ''user-agent: tns_marker{"tns_id":%d,"type": "%s", "name":"%s"}'' -d ''api_key=%s'' %s > %s', TNS_ID, TNS_Type, TNS_Name, TNS_APIKEY, TNS_File, LocalFile);
%     [Stat1, OutStr] = system(CommandStr);
% 
%     unzip(LocalFile);
%     Result = readtable(LocalFile(1:end-4));
% 
%     TNS_File = 'https://www.wis-tns.org/system/files/tns_public_objects/tns_public_objects_20240718.csv.zip'
%     LocalFile = 'tns_public_objects_20240718.csv.zip'
% 
% 
% end
% 
% 
% 