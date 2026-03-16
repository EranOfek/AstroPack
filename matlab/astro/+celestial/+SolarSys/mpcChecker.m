function Results = mpcChecker(RA, Dec, Date, Args)
    % Query the Minor Planet Center mpchecker service.
    %   Note that the executaion of this function many times may be limited by the MPC
    %   checker server.
    % Input  : - J2000.0 RA [deg, sexagesimal, rad, Target name].
    %            See celestial.convert.cooResolve for options.
    %          - J2000.0 Dec.
    %          - Date: [D M Y Frac], 'YYYY-MM-DD HH:MM:SS' or JD (UTC).
    %            If empty, then use current time. Default is [].
    %          * ...,key,val,...
    %            'SearchRadius'   - Search radius [arcsec]. Default is 10.
    %            'MagLimit' - Limiting V magnitude. Default 24.0
    %            'ObsCode' - Observatory code.
    %                   '500' for geocentric observer.
    %                   'M01' for LAST.
    %                    Default '500'
    %            'Sort' - Options: 'd' = distance, 'r' = RA, default 'd'
    %            'Mot' - Motion units: 'm','h','d', default 'h'
    %            'Filter' - 'f','n','u','t','a', default 'f'
    %            'Csv' - CSV output file path, default ""
    %            'Verbose'  - Print summary, default false.
    % Output : - As tructure array with the asteroids found in search
    %            radius/time.
    % Authour : Claude + ChatGPT + Eran Ofek (Mar 2026)
    % Example: R=celestial.SolarSystem.mpcChecker(10,0,2461010.1, 'SearchRadius',600, 'ObsCode','M01');

    arguments
        RA  
        Dec  
        Date        = [];
        Args.SearchRadius  (1,1) double = 10;  % [arcsec]
        Args.MagLimit   (1,1) double = 24.0
        Args.ObsCode    = "500";
        Args.Sort    {mustBeTextScalar} = "d"
        Args.Mot     {mustBeTextScalar} = "h"
        Args.Filter  {mustBeTextScalar} = "f"
        Args.Csv     {mustBeTextScalar} = ""
        Args.Verbose (1,1) logical = false;

        Args.InUnits  = 'deg';
        Args.Server   = 'SIMBAD';
    end

    MpcUrl = 'https://www.minorplanetcenter.net/cgi-bin/mpcheck.cgi';

    % convert search radius to arcmin:
    Args.Radius = Args.SearchRadius./60;  % [arcmin]

    % Optional manual checks kept outside arguments block to avoid validator issues
    if Args.Radius > 300
        error('mpcChecker:badRadius', 'Radius must be in the range 5 to 300 arcmin.');
    end
    if ~any(strcmp(char(string(Args.Sort)), {'d','r'}))
        error('mpcChecker:badSort', 'Sort must be ''d'' or ''r''.');
    end
    if ~any(strcmp(char(string(Args.Mot)), {'m','h','d'}))
        error('mpcChecker:badMot', 'Mot must be ''m'', ''h'', or ''d''.');
    end
    if ~any(strcmp(char(string(Args.Filter)), {'f','n','u','t','a'}))
        error('mpcChecker:badFilter', 'Filter must be one of: f, n, u, t, a.');
    end

    [RARAD, DecRAD] = celestial.convert.cooResolve(RA, Dec, 'InUnits',Args.InUnits, 'OutUnits','rad', 'Server',Args.Server);
    RA        = celestial.coo.convertdms(RARAD, 'r','SHb');
    Dec       = celestial.coo.convertdms(DecRAD,'r','SDb');

    if isempty(Date)
        Date = celestial.time.julday();
    end

    if isscalar(Date)
        % assume Date is JD
        JD = Date;
    else
        JD = celestial.time.julday(Date);
    end
    Date = celestial.time.jd2date(JD,'f');
    

    %% Determine date
    % if strlength(string(Date)) == 0
    %     T = datetime('now', 'TimeZone', 'UTC');
    %     Year = T.Year;
    %     Month = T.Month;
    %     FractionalDay = T.Day + (T.Hour + T.Minute./60 + T.Second./3600)./24;
    % else
    %     T = parseDate(Date);
    %     Year = T.Year;
    %     Month = T.Month;
    %     FractionalDay = T.Day + (T.Hour + T.Minute./60 + T.Second./3600)./24;
    % end

    %% Build POST body
    Params = { ...
        'year',     sprintf('%04d',Date(3)); ...
        'month',    sprintf('%02d', Date(2)); ...
        'day',      sprintf('%.4f', Date(1)+Date(4)); ...
        'which',    'pos'; ...
        'ra',       char(string(RA)); ...
        'decl',     char(string(Dec)); ...
        'TextArea', ''; ...
        'radius',   num2str(Args.Radius); ...
        'limit',    num2str(Args.MagLimit); ...
        'oc',       char(string(Args.ObsCode)); ...
        'sort',     char(string(Args.Sort)); ...
        'mot',      char(string(Args.Mot)); ...
        'tmot',     's'; ...
        'pdes',     'u'; ...
        'ps',       'n'; ...
        'needed',   char(string(Args.Filter)); ...
        'type',     'p'; ...
    };
    Body = urlEncode(Params);

    %% POST to MPC
    if Args.Verbose
        fprintf('Querying Minor Planet Center...\n');
    end

    Options = weboptions( ...
        'MediaType',     'application/x-www-form-urlencoded', ...
        'RequestMethod', 'post', ...
        'Timeout',       60, ...
        'UserAgent',     'mpcChecker/1.0 (MATLAB)' ...
    );

    Html = webwrite(MpcUrl, Body, Options);

    %% Print summary line
    if Args.Verbose
        Summary = extractSummary(Html);
        if ~isempty(Summary)
            fprintf('%s\n', Summary);
        end
    end

    %% Parse results
    DataLines = extractPreLines(Html);
    Results = parseLines(DataLines, RARAD, DecRAD);

    if isempty(Results)
        if ~isempty(regexpi(Html, 'no known minor planet'))
            if Args.Verbose
                fprintf('No known minor planets found in this region.\n');
            end
        else
            if Args.Verbose
                fprintf('No results parsed. Raw lines:\n');
                for I = 1:numel(DataLines)
                    fprintf('%s\n', DataLines{I});
                end
            end
        end
        return
    end

    %% Display table
    if Args.Verbose
        fprintf('\n%-28s %12s %13s %5s  %14s  %12s  %6s  %s\n', ...
            'Designation', 'RA', 'Dec', 'V', 'Offsets', 'Motion', 'Orbit', 'Comment');
        fprintf('%s\n', repmat('-', 1, 120));
        for I = 1:numel(Results)
            R = Results(I);
            fprintf('%-28s %12s %13s %5s  %14s  %12s  %6s  %s\n', ...
                R.designation, R.ra, R.dec, R.V, R.offsets, R.motion, R.orbit, R.comment);
        end
        fprintf('\n%d object(s) found.\n', numel(Results));
    end

    %% Write CSV if requested
    if strlength(string(Args.Csv)) > 0
        writeCsv(char(string(Args.Csv)), Results);
        if Args.Verbose
            fprintf('Results written to %s\n', char(string(Args.Csv)));
        end
    end
end


%% -------------------------------------------------------------------------
function T = parseDate(DateString)

    Formats = {'yyyy-MM-dd HH:mm:ss', 'yyyy-MM-dd HH:mm', 'yyyy-MM-dd'};
    for I = 1:numel(Formats)
        try
            T = datetime(string(DateString), 'InputFormat', Formats{I});
            return
        catch
        end
    end

    error('mpcChecker:badDate', ...
        'Cannot parse date "%s". Use YYYY-MM-DD or YYYY-MM-DD HH:MM:SS', ...
        char(string(DateString)));
end


%% -------------------------------------------------------------------------
function Encoded = urlEncode(Params)
% Build an application/x-www-form-urlencoded string from an Nx2 cell array.

    Parts = cell(size(Params, 1), 1);
    for I = 1:size(Params, 1)
        Key = urlEncodeStr(Params{I,1});
        Value = urlEncodeStr(Params{I,2});
        Parts{I} = [Key '=' Value];
    end
    Encoded = strjoin(Parts, '&');
end


%% -------------------------------------------------------------------------
function EncodedString = urlEncodeStr(InputString)
% Percent-encode a string for use in a URL query string.

    InputString = char(string(InputString));
    OutputString = '';
    SafeChars = [('A':'Z') ('a':'z') ('0':'9') '-' '_' '.' '~' ' '];

    for I = 1:numel(InputString)
        Char1 = InputString(I);
        if Char1 == ' '
            OutputString = [OutputString '+']; %#ok<AGROW>
        elseif any(SafeChars == Char1)
            OutputString = [OutputString Char1]; %#ok<AGROW>
        else
            OutputString = [OutputString sprintf('%%%02X', uint8(Char1))]; %#ok<AGROW>
        end
    end

    EncodedString = OutputString;
end


%% -------------------------------------------------------------------------
function Summary = extractSummary(Html)

    Summary = '';

    Token = regexp(Html, 'The following objects(.*?)<pre>', 'tokens', 'once');
    if ~isempty(Token)
        Raw = ['The following objects' Token{1}];
        Raw = regexprep(Raw, '<[^>]+>', '');
        Raw = regexprep(Raw, '\s+', ' ');
        Summary = strtrim(Raw);
        return
    end

    Token = regexp(Html, '(No known minor planets[^\n]*)', 'tokens', 'once');
    if ~isempty(Token)
        Summary = strtrim(Token{1});
    end
end


%% -------------------------------------------------------------------------
function Lines = extractPreLines(Html)
% Return data lines from the <pre> block (after the column-header row).

    Lines = {};
    Token = regexp(Html, '<pre>(.*?)</pre>', 'tokens', 'once', 'ignorecase');
    if isempty(Token)
        return
    end

    PreBlock = Token{1};
    PreBlock = strrep(PreBlock, '&#176;', char(176));
    PreBlock = strrep(PreBlock, '&amp;', '&');
    PreBlock = strrep(PreBlock, '&lt;', '<');
    PreBlock = strrep(PreBlock, '&gt;', '>');
    PreBlock = regexprep(PreBlock, '<[^>]+>', '');

    RawLines = strsplit(PreBlock, '\n');
    PastHeader = false;

    for I = 1:numel(RawLines)
        Line = RawLines{I};
        if contains(Line, 'h  m  s')
            PastHeader = true;
            continue
        end
        if PastHeader && ~isempty(strtrim(Line))
            Lines{end+1} = Line; %#ok<AGROW>
        end
    end
end


%% -------------------------------------------------------------------------

function Results = parseLines(Lines, RARAD, DecRAD)
% Parse each data line into a struct using a regular expression.

    RAD = 180./pi;
    ARCSEC_DEG = 3600;

    Pattern = [ ...
        '^(.{25})' ...                                  designation
        '(\d{2} \d{2} [\d.]+)\s+' ...                   RA
        '([+-]\d{2} \d{2} \d{2})\s+' ...                Dec
        '([\d.]+)\s+' ...                               V
        '([0-9.]+[EW]\s+[0-9.]+[NS])\s+' ...            offsets
        '([0-9+\-]+\s+[0-9+\-]+)\s+' ...                motion
        '(\S+)\s+' ...                                  orbit/code
        '(.*)$' ...                                     comment
    ];

    Results = struct( ...
        'Designation', {}, 'RA', {}, 'Dec', {}, 'Dist', {}, 'SexRA', {}, 'SexDec', {}, 'MagV', {}, ...
        'Offsets', {}, 'Motion', {}, 'Orbit', {}, 'Comment', {});

    for I = 1:numel(Lines)
        Token = regexp(Lines{I}, Pattern, 'tokens', 'once');
        if isempty(Token)
            continue
        end

        S.Designation = strtrim(Token{1});
        S.SexRA       = strtrim(Token{2});
        S.SexDec      = strtrim(Token{3});
        S.RA          = celestial.coo.convertdms(S.SexRA,'SHb','d');
        S.Dec         = celestial.coo.convertdms(S.SexDec,'SDb','d');
        S.Dist        = celestial.coo.sphere_dist_fast(RARAD, DecRAD, S.RA./RAD, S.Dec./RAD).*RAD.*ARCSEC_DEG;
        S.MagV        = strtrim(Token{4});
        S.Offsets     = strtrim(Token{5});
        S.Motion      = strtrim(Token{6});
        S.Orbit       = strtrim(Token{7});
        S.Comment     = strtrim(Token{8});

        Results(end+1) = S; %#ok<AGROW>
    end
end


function Results = parseLines1(Lines)
% Parse each data line into a struct using a regular expression.

    Pattern = [ ...
        '^(.{25})' ...
        '(\d{2} \d{2} [\d.]+)\s+' ...
        '([+-]\d{2} \d{2} \d{2})\s+' ...
        '([\d.]+)\s+' ...
        '([\d.]+[EW]\s+[\d.]+[NS])\s+' ...
        '([\d-]+\s+[\d+\-]+)\s+' ...
        '(\S+)\s+' ...
        '(.*)' ...
    ];

    Results = struct( ...
        'designation', {}, 'ra', {}, 'dec', {}, 'V', {}, ...
        'offsets', {}, 'motion', {}, 'orbit', {}, 'comment', {});

    for I = 1:numel(Lines)
        Token = regexp(Lines{I}, Pattern, 'tokens', 'once');
        if isempty(Token)
            continue
        end

        S.designation = strtrim(Token{1});
        S.ra          = strtrim(Token{2});
        S.dec         = strtrim(Token{3});
        S.V           = strtrim(Token{4});
        S.offsets     = strtrim(Token{5});
        S.motion      = strtrim(Token{6});
        S.orbit       = strtrim(Token{7});
        S.comment     = strtrim(Token{8});

        Results(end+1) = S; %#ok<AGROW>
    end
end


%% -------------------------------------------------------------------------
function writeCsv(FilePath, Results)

    Fid = fopen(FilePath, 'w');
    if Fid < 0
        error('mpcChecker:csvOpen', 'Cannot open file for writing: %s', FilePath);
    end

    fprintf(Fid, 'designation,ra,dec,V,offsets,motion,orbit,comment\n');
    for I = 1:numel(Results)
        R = Results(I);
        fprintf(Fid, '"%s","%s","%s","%s","%s","%s","%s","%s"\n', ...
            R.designation, R.ra, R.dec, R.V, R.offsets, R.motion, R.orbit, R.comment);
    end

    fclose(Fid);
end


%% -------------------------------------------------------------------------
function mustBeTextScalar(Value)

    if ~(ischar(Value) || (isstring(Value) && isscalar(Value)))
        error('Value must be a char vector or string scalar.');
    end
end