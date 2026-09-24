function [T, CsvFiles] = downloadHistoricSNe(Args)
    % Download all TNS search CSV pages and concatenate into one table.
    % Input  : * ...,key,val,...
    %            'Url'       - string, must contain %d for page number
    %            'OutDir'    - string
    %            'TextType'  - string = "string"
    %            'KeepFiles' - logical = false
    %            'Verbose'   - logical = false
    %            'MaxPages'  - double = Inf
    %            'Pause'     - Psuse [s] between retrivals. Default is 10.
    %            'FixColumns' - make the columns similar to that of the
    %                   TNS. Default is true.
    % Output : - TNS table with all pages concatenated
    %          - CSV file names
    %
    % Example:
    %   T = VO.TNS.downloadHistoricSNe;

    arguments
        Args.FirstPage   = 0;
        Args.Url (1,1) string = ...
            "https://www.wis-tns.org/search?&isTNS_AT=no&name=SN&num_page=500&format=csv&page=%d"
        Args.OutDir (1,1) string = string(tempdir)
        Args.TextType (1,1) string {mustBeMember(Args.TextType,["string","char"])} = "string"
        Args.KeepFiles (1,1) logical = false
        Args.Verbose (1,1) logical = false
        Args.MaxPages (1,1) double {mustBePositive} = Inf
        Args.Pause        = 10; 
        Args.FixColumns   = true;
    end

    if ~exist(Args.OutDir, 'dir')
        mkdir(Args.OutDir);
    end

    OptsWeb = weboptions( ...
        'HeaderFields', {'User-Agent', 'Mozilla/5.0'}, ...
        'Timeout', 600);

    T = table;
    CsvFiles = strings(0,1);

    Page = Args.FirstPage;

    while Page < Args.MaxPages
        pause(Args.Pause);

        PageUrl = sprintf(Args.Url, Page);
        CsvFile = fullfile(Args.OutDir, sprintf('tns_search_SN_page_%06d.csv', Page));

        if Args.Verbose
            fprintf('Downloading page %d: %s\n', Page, PageUrl);
        end

        try
            CsvText = webread(PageUrl, OptsWeb);
        catch ME
            if Args.Verbose
                fprintf('Stopping: failed to download page %d\n%s\n', Page, ME.message);
            end
            break;
        end

        CsvText = string(CsvText);

        % Stop if page is empty or not really CSV
        if strlength(strtrim(CsvText)) == 0
            if Args.Verbose
                fprintf('Stopping: empty page %d\n', Page);
            end
            break;
        end

        % Save page to file
        Fid = fopen(CsvFile, 'w');
        assert(Fid > 0, 'Could not open file for writing: %s', CsvFile);
        fwrite(Fid, char(CsvText), 'char');
        fclose(Fid);

        try
            %ImportOpts = detectImportOptions(CsvFile, 'TextType', Args.TextType);
            %Tpage = readtable(CsvFile, ImportOpts);

            ImportOpts = detectImportOptions(CsvFile, ...
                'TextType', 'string', ...
                'DatetimeType', 'text');
            
            % Force all columns to string to avoid duration/datetime/numeric guessing
            ImportOpts = setvartype(ImportOpts, ImportOpts.VariableNames, 'string');
            
            Tpage = readtable(CsvFile, ImportOpts);
        catch ME
            if Args.Verbose
                fprintf('Stopping: failed to read page %d as table\n%s\n', Page, ME.message);
            end
            break;
        end

        % Stop when no rows are returned
        if height(Tpage) == 0
            if Args.Verbose
                fprintf('Stopping: page %d has zero rows\n', Page);
            end
            break;
        end

        % Append
        if isempty(T)
            T = Tpage;
        else
            T = [T; Tpage]; %#ok<AGROW>
        end

        CsvFiles(end+1,1) = string(CsvFile); %#ok<AGROW>

        if Args.Verbose
            fprintf('Read page %d: %d rows, total %d rows\n', ...
                Page, height(Tpage), height(T));
        end

        Page = Page + 1;
    end

    if ~Args.KeepFiles
        for I = 1:numel(CsvFiles)
            try
                delete(CsvFiles(I));
            end %#ok<TRYNC>
        end
    end

    % fix columns and make it similar to new format:
    if Args.FixColumns
        Nrow          = size(T,1);
        T.ra          = celestial.coo.convertdms(T.RA, 'SH','d');
        T.declination = celestial.coo.convertdms(T.DEC, 'SD','d');
        T.Properties.VariableNames{'ID'} = 'objid';
        T.name_prefix = repmat("SN",Nrow,1);
        T.name        = extractAfter(T.Name,3);
        T.Properties.VariableNames{'Redshift'} = 'redshift';
        T.Properties.VariableNames{'Obj_Type'} = 'type';
        T.typeid      = nan(Nrow,1);
        T.reporting_groupid = nan(Nrow,1);
        T.Properties.VariableNames{'ReportingGroup_s'} = 'reporting_group';
        T.source_groupid    = nan(Nrow,1);
        T.source_group      = repmat("",Nrow,1);
        T.Properties.VariableNames{'DiscoveryDate_UT_'} = 'discoverydate';
        T.Properties.VariableNames{'DiscoveryMag_Flux'} = 'discoverymag';
        T.Properties.VariableNames{'DiscoveryFilter'} = 'discmagfilter';
        T.Properties.VariableNames{'DiscoveryBibcode'} = 'Discovery_ADS_bibcode';
        T.Properties.VariableNames{'ClassificationBibcodes'} = 'Class_ADS_bibcodes';
        T.redshift = str2double(T.redshift);

        RmCol = {'RA','DEC','Name','HostName','HostRedshift','DiscoveryDataSource_s',...
         'ClassifyingGroup_s','AssociatedGroup_s','Disc_InternalName','Disc_Instrument_s',...
         'Class_Instrument_s','TNSAT','Public',...   
         'EndProp_Period','Sender','Remarks',...                  
         'Ext_Catalog_s',...
         'AutoClassification'};
        Nrm = numel(RmCol);
        for Irm=1:1:Nrm
            T.(RmCol{Irm}) = [];
        end

        T.objid = str2double(T.objid);
        T.discoverymag = str2double(T.discoverymag);

    end


end