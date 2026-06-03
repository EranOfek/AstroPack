function Cat = buildChandraObsIDcat(Args)
% Build a catalog of all Chandra ObsID observations via the CXC TAP service.
% Description: Queries the Chandra Observation Catalog TAP service
%   (https://cda.cfa.harvard.edu/cxctap) and returns a MATLAB table with
%   one row per ObsID.  As of 2026 the catalog contains ~28 000 rows.
%   The table is sorted by obsid.  Optionally filters by observation
%   status or instrument and saves the result to a .mat file.
%
%   Note: This function cannot delegate to VO.TopCat.queryHttp because
%   the Chandra TAP only accepts FORMAT=text or FORMAT=votable (not CSV).
%   The Chandra sync endpoint also returns a 303 (See Other) redirect,
%   which neither webwrite() nor matlab.net.http follow for POST requests
%   in the MATLAB versions tested.  The function therefore delegates the
%   HTTP transfer to curl (-sL), which handles POST+303 correctly on all
%   platforms where curl is available.
%
% Input  : * ...,key,val,...
%            'TapUrl'     - Chandra Observation Catalog TAP base URL.
%                           Default: 'https://cda.cfa.harvard.edu/cxctap'
%            'Status'     - Filter by observation status.  Use '' for all.
%                           Options: 'archived', 'observed', 'scheduled',
%                           'unobserved'.
%                           Default: '' (no filter).
%            'Instrument' - Filter by instrument name.  Use '' for all.
%                           Examples: 'ACIS-I', 'ACIS-S', 'HRC-I', 'HRC-S'.
%                           Default: '' (no filter).
%            'SaveFile'   - Full path of a .mat file in which to save the
%                           output table as the variable 'ChandraCat'.
%                           If empty, nothing is saved.  Default: ''.
%            'TimeoutSec' - HTTP timeout in seconds.  Default: 300.
%            'Verbose'    - Print progress messages.  Default: true.
% Output : - A MATLAB table with one row per Chandra ObsID, sorted by
%            obsid.  Column names and units follow the CXC TAP schema:
%              obsid             - Chandra observation identifier
%              target_name       - Target name
%              ra          [deg] - Right ascension J2000 (ICRS)
%              dec         [deg] - Declination J2000 (ICRS)
%              gal_l       [deg] - Galactic longitude
%              gal_b       [deg] - Galactic latitude
%              instrument        - ACIS-I / ACIS-S / HRC-I / HRC-S
%              grating           - LETG / HETG / NONE
%              proposal_number   - Chandra proposal number
%              status            - archived/observed/scheduled/unobserved
%              start_date        - UTC start of observation
%              public_avail_date - Date of public data availability
%              exposure_time [ks] - Total exposure time
%              exposure_mode     - TE / CC / HRC Timing
%              event_count       - Total event count
%              event_count_rate [Hz] - Events per second
%              sequence_num      - Links related observations
%              grid_name         - Grid grouping name
%              joint_obs         - Joint-observatory flag
%              obs_ao_str        - Chandra observing cycle / AO string
%              category          - Science category, used in old CDA path catN
% Author : Eran Ofek (May 2026)
% Example: Cat = VO.Chandra.buildChandraObsIDcat;
%          Cat = VO.Chandra.buildChandraObsIDcat('Status','archived');
%          Cat = VO.Chandra.buildChandraObsIDcat('Instrument','HRC-S','SaveFile','/tmp/chandra.mat');

    arguments
        Args.TapUrl      char    = 'https://cda.cfa.harvard.edu/cxctap'
        Args.Status      char    = ''
        Args.Instrument  char    = ''
        Args.SaveFile    char    = ''
        Args.TimeoutSec  double  = 300
        Args.Verbose     logical = true
    end

    TapUrl = Args.TapUrl;
    if endsWith(TapUrl, '/'), TapUrl = TapUrl(1:end-1); end

    % ------------------------------------------------------------------ %
    %  Build ADQL query                                                   %
    % ------------------------------------------------------------------ %
    ColList = ['o.obsid, o.target_name, o.ra, o.dec, o.gal_l, o.gal_b, ' ...
               'o.instrument, o.grating, o.proposal_number, o.status, '  ...
               'o.start_date, o.public_avail_date, o.exposure_time, '    ...
               'o.exposure_mode, o.event_count, o.event_count_rate, '    ...
               'o.sequence_num, o.grid_name, o.joint_obs'];
               %'o.obs_ao_str, o.category'];

    WhereClause = '';
    if ~isempty(Args.Status)
        WhereClause = sprintf(" WHERE o.status = '%s'", Args.Status);
    end
    if ~isempty(Args.Instrument)
        if isempty(WhereClause)
            WhereClause = sprintf(" WHERE o.instrument = '%s'", Args.Instrument);
        else
            WhereClause = [WhereClause, sprintf(" AND o.instrument = '%s'", Args.Instrument)];
        end
    end

    Q = sprintf('SELECT %s FROM cxc.observation o%s ORDER BY o.obsid', ...
                ColList, WhereClause);

    % ------------------------------------------------------------------ %
    %  Execute TAP query via matlab.net.http                              %
    %                                                                     %
    %  The Chandra sync endpoint returns HTTP 303 (See Other); curl -sL   %
    %  follows the redirect transparently.  The ADQL query is written to %
    %  a temp file and read via --data-urlencode QUERY@file so that      %
    %  single quotes inside WHERE clauses require no shell escaping.     %
    % ------------------------------------------------------------------ %
    if Args.Verbose
        fprintf('Querying Chandra TAP (%s/sync)...\n', TapUrl);
    end

    % Use curl (-sL follows all redirects including the 303 that Chandra
    % returns on sync queries).  The ADQL query is written to a temp file
    % and passed via --data-urlencode QUERY@file to avoid any shell-quoting
    % issues with single quotes inside ADQL WHERE clauses.
    QueryFile = [tempname '.adql'];
    OutFile   = [tempname '.tsv'];
    fid = fopen(QueryFile, 'w');
    if fid < 0
        error('VO:Chandra:buildChandraObsIDcat:TempFile', ...
              'Cannot create temp file for ADQL query.');
    end
    fprintf(fid, '%s', Q);
    fclose(fid);

    Cmd = sprintf(['curl -sL --max-time %d --request POST "%s/sync" ' ...
                   '--data-urlencode "REQUEST=doQuery" '              ...
                   '--data-urlencode "FORMAT=text" '                  ...
                   '--data-urlencode "LANG=ADQL" '                    ...
                   '--data-urlencode "QUERY@%s" '                     ...
                   '-o "%s"'], ...
                  Args.TimeoutSec, TapUrl, QueryFile, OutFile);

    [Status, ErrMsg] = system(Cmd);
    try, delete(QueryFile); end %#ok<TRYNC>

    if Status ~= 0
        try, delete(OutFile); end %#ok<TRYNC>
        error('VO:Chandra:buildChandraObsIDcat:CurlError', ...
              'curl failed (exit %d): %s', Status, strtrim(ErrMsg));
    end

    RawText = fileread(OutFile);
    try, delete(OutFile); end %#ok<TRYNC>

    % ------------------------------------------------------------------ %
    %  Parse TSV response                                                 %
    % ------------------------------------------------------------------ %
    Cat = localParseTsv(RawText);

    if Args.Verbose
        fprintf('Retrieved %d observations.\n', height(Cat));
    end

    % ------------------------------------------------------------------ %
    %  Reformat: convert any cell-string columns to numeric, rename      %
    %  ra/dec to RA/Dec.  Done before the optional save so the stored    %
    %  file matches the returned table.                                  %
    % ------------------------------------------------------------------ %
    Cat = tools.table.table_cell2string(Cat);
    % NumericCols = {'ra','dec','gal_l','gal_b','exposure_time', ...
    %                'event_count','event_count_rate','obsid','sequence_num'};
    % for Ic = 1:numel(NumericCols)
    %     Cn = NumericCols{Ic};
    %     if ismember(Cn, Cat.Properties.VariableNames)
    %         Cat.(Cn) = str2double(Cat.(Cn));
    %     end
    % end
    Cat = renamevars(Cat, 'ra',  'RA');
    Cat = renamevars(Cat, 'dec', 'Dec');


    PropNum = Cat.proposal_number;
    
    if ~isnumeric(PropNum)
        PropNum = str2double(string(PropNum));
    end
    
    Cat.AO = floor(PropNum ./ 1e6);
    Cat.CatNum = floor((PropNum - Cat.AO.*1e6) ./ 1e5);


    % ------------------------------------------------------------------ %
    %  Optional save                                                      %
    % ------------------------------------------------------------------ %
    if ~isempty(Args.SaveFile)
        ChandraCat = Cat; %#ok<NASGU>
        save(Args.SaveFile, 'ChandraCat', '-v7.3');
        if Args.Verbose
            fprintf('Catalog saved to %s\n', Args.SaveFile);
        end
    end


end % buildChandraObsIDcat



% ==================================================================== %
%  Local helper: parse the Chandra TAP "text" (TSV) response           %
% ==================================================================== %
function T = localParseTsv(Txt)
% Parse the tab-separated text reply from the Chandra TAP service.
%
% Format:
%   # col_name  [unit]  description   <- comment line per column (skip)
%   col1  col2  col3 ...              <- header row
%   val1  val2  val3 ...              <- data rows (tab-delimited)

    if ~ischar(Txt), Txt = char(Txt); end

    % Split into lines (handle LF, CRLF, CR)
    Lines = strsplit(Txt, {char(10), char(13)});

    % Harvest unit annotations and collect data lines
    ColUnits  = struct();
    DataLines = {};
    for Il = 1:numel(Lines)
        L = strtrim(Lines{Il});
        if isempty(L), continue; end
        if L(1) == '#'
            % "# colname \t [unit \t] description"
            Parts = strsplit(L(2:end), char(9));
            Parts = strtrim(Parts);
            if ~isempty(Parts{1})
                Vn = matlab.lang.makeValidName(Parts{1});
                if numel(Parts) >= 3 && ~isempty(Parts{2})
                    ColUnits.(Vn) = Parts{2};
                end
            end
        else
            DataLines{end+1} = L; %#ok<AGROW>
        end
    end

    if numel(DataLines) < 2
        warning('VO:Chandra:buildChandraObsIDcat:EmptyResponse', ...
                'TAP response contained no data rows.');
        T = table();
        return
    end

    % First data line is the column header
    RawHdrs    = strsplit(DataLines{1}, char(9));
    RawHdrs    = strtrim(RawHdrs);
    ValidHdrs  = matlab.lang.makeValidName(RawHdrs);
    Ncol = numel(RawHdrs);
    Nrow = numel(DataLines) - 1;

    % Parse data into a cell matrix
    CellMat = repmat({''}, Nrow, Ncol);
    for Ir = 1:Nrow
        Parts  = strsplit(DataLines{Ir+1}, char(9));
        Nparts = min(numel(Parts), Ncol);
        for Ic = 1:Nparts
            CellMat{Ir, Ic} = strtrim(Parts{Ic});
        end
    end

    % Convert each column to numeric where all values parse cleanly
    ColData = cell(1, Ncol);
    for Ic = 1:Ncol
        Col       = CellMat(:, Ic);
        Nums      = str2double(Col);
        EmptyMask = cellfun(@isempty, Col);
        if all(~isnan(Nums) | EmptyMask)
            Nums(EmptyMask) = NaN;
            ColData{Ic} = Nums;
        else
            ColData{Ic} = Col;
        end
    end

    T = table(ColData{:}, 'VariableNames', ValidHdrs);

    % Restore un-mangled column names where possible
    for Ic = 1:Ncol
        if ~strcmp(ValidHdrs{Ic}, RawHdrs{Ic})
            try, T.Properties.VariableNames{Ic} = RawHdrs{Ic}; catch, end
        end
    end

    % Attach unit strings harvested from comment block
    for Fn = fieldnames(ColUnits)'
        try, T.Properties.VariableUnits{Fn{1}} = ColUnits.(Fn{1}); catch, end
    end

end % localParseTsv
