% TopCat - Query online the TopCat catalogs
%
% Download TopCat Stilts jar file:
% https://www.star.bris.ac.uk/~mbt/stilts/?utm_source=chatgpt.com
% best to download using installer:
% In=Installer;
% In.install('TopCatJar');
% 
% Examples:
%
%   Q='SELECT TOP 100 source_id, ra, dec FROM gaiaedr3.gaia_source WHERE phot_g_mean_mag < 12'
%   T = VO.TopCat.queryHttp(Q)
%   Q = "SELECT TOP 50 source_id, ra, dec FROM gaiaedr3.gaia_source WHERE phot_g_mean_mag < 12";
%   T = VO.TopCat.queryStilts(Q);
%
%   Tap = VO.TopCat;
%   T = Tap.query(Q, 'TapUrl',Tap.TapList{1,2})
%
%   Q = 'SELECT top 1 * FROM %s';
%   T = Tap.query(Q, 'Cat','PS1');
%
% Search for VizieR catalogs
%   Tap = VO.TopCat;
%   Q = "SELECT schema_name, table_name, description FROM TAP_SCHEMA.tables WHERE description LIKE '%Ritter%' OR description LIKE '%Cataclysmic%' OR table_name LIKE '%cb%';"
%   Q = "SELECT schema_name, table_name, description FROM TAP_SCHEMA.tables WHERE description LIKE '%Ritter%' AND description LIKE '%Cataclysmic%';"
%   T = Tap.query(Q);
%   Q = 'SELECT * FROM "V/82/catalog"'
%   Tcv = Tap.query(Q);
%   Tcv= tools.table.table_cell2string(Tcv);
%
% Complex queries on SIMBAD - query for all WD which are variable of any kind:
%   Q="SELECT b.main_id,b.ra,b.dec,od.otype_shortname AS otype,b.sp_type FROM basic AS b JOIN otypedef AS od ON b.otype=od.otype WHERE ( ( (od.otype_shortname IN ('WD*','WD?') OR b.sp_type LIKE 'D%' OR b.sp_type LIKE 'PG 1159%') AND (od.otype_shortname LIKE 'V%' OR od.otype_shortname IN ('Pu*','EB*','El*')) ) OR od.otype_shortname IN ('CV*','CV?') ) ORDER BY ra";
%   T = Tap.query(Q);  % select SIMBAD TAP





classdef TopCat < Base
    % OrbitalEl class for storing and manipulating orbital elements

    % Properties
    properties
        Ofmt       = 'csv';                                     % Output format
        TimeoutSec = 600;                                       % For HTTP fallback
        %TapUrl     = 'https://tapvizier.cds.unistra.fr/TAPVizieR/tap';
        TapUrl     = 'https://gea.esac.esa.int/tap-server/tap';  % Default TAP endpoint (Gaia)
    end
    
    
    properties (Constant)
        
        TapList = ["ESA Gaia Archive", "https://gea.esac.esa.int/tap-server/tap";...
                   "ARI Gaia mirror (Heidelberg)", "https://gaia.ari.uni-heidelberg.de/tap";...
                   "VizieR TAP (all VizieR catalogs)", "https://tapvizier.cds.unistra.fr/TAPVizieR/tap";...
                   "SIMBAD TAP", "http://simbad.u-strasbg.fr/simbad/sim-tap";...
                   "CDS X-Match service", "http://cdsxmatch.u-strasbg.fr/xmatch/api/v1/tap";...
                   "ESO Science Archive TAP", "http://archive.eso.org/tap_obs";...
                   "ESO Public Survey Catalogue TAP", "http://archive.eso.org/tap_cat";...
                   "MAST (Mikulski Archive for Space Telescopes)", "https://mast.stsci.edu/tap";...
                   "HEASARC TAP (X-ray, gamma-ray, high-energy missions)", "https://heasarc.gsfc.nasa.gov/xamin/tap";...
                   "SkyMapper (Australia)", "https://skymapper.anu.edu.au/tap";...
                   "UKIDSS / WFAU (Edinburgh)", "http://wfaudata.roe.ac.uk/tap";...
                   "Pan-STARRS (Hawaii)", "https://vao.stsci.edu/PS1DR2/tapservice.aspx";...
                   "SDSS (Sloan Digital Sky Survey)", "http://skyserver.sdss.org/tap";...
                   "LAMOST (China)", "http://dr7.lamost.org/tap";...
                   "EPN-TAP (Europlanet / planetary data)", "http://vespa.obspm.fr/tap"];

                   %How to Discover More TAP Services
                   %IVOA Registry (search for TAP services):
                   %https://registry.ivoa.net
                   %TOPCAT / STILTS:
                   %In TOPCAT: VO → Table Access Protocol (TAP) Query, then “Find Service”.


        % Common/useful catalogs:
        % CommonName, Description, NameInDB, TapUrl
        CommonCat = ["PS1", "Pan-STARRS DR1 catalogue", "II/349/ps1", "https://tapvizier.cds.unistra.fr/TAPVizieR/tap";...
                     "GAIA-DR3", "GAIA DR3", "gaiaedr3.gaia_source", "https://gea.esac.esa.int/tap-server/tap"];
    end
    
    properties (Hidden)
    
        
    end
    
    
    methods % constructor
        
    end
    
    methods  % getters/setters
        function Val=get.TapUrl(Obj)
            % getter for TapUrl

            if isempty(Obj.TapUrl)
                % call GUI selector
                Val = Obj.selectTapServer;
            else
                Val = Obj.TapUrl;
            end
            
        end
    end
    
    methods
        function T=query(Obj, Query, Args)
            % Execute TAP query using Java or Http
            % Input  : - self.
            %          - Query.
            %          * ...,key,val,...
            %            'Cat' - An optional short cut for common catalogs.
            %                   In order to activate this option, this
            %                   parameter should be not-empty AND the query
            %                   string should contain a '%s' after the FROM
            %                   clause.
            %                   This option will also automatically set the
            %                   TapUrl string.
            %                   Available catalogs are:
            %                   'PS1'
            %                   'GAIA-DR3'
            %                   Default is [].
            %            'Method' - 'java'|'http'. ('java' is faster).
            %                   Java requires installing jar file:
            %                   In=Installer; In.install('TopCatJar');
            %                   Default is 'java'.
            %            'TapUrl' - If empty, then use object TapUrl.
            %                   If 'select', then call GUI selector.
            %                   Default is [].
            %            'Ofmt' - Format. Default is 'csv'.
            %            'TimeoutSec' - Timeout in sec. Default is 600.
            %            'JarFile' - Jar file full path.
            %                   Default is VO.TopCat.getStiltsJarPath()
            % Output : - A table with results.
            % Author : Eran Ofek (Aug 2025)
            % Example: Q='SELECT TOP 100 source_id, ra, dec FROM gaiaedr3.gaia_source WHERE phot_g_mean_mag < 12';
            %          Tap = VO.TopCat;
            %          T = Tap.query(Q);
            %
            %          Q = 'SELECT top 1 * FROM %s';
            %          T = Tap.query(Q, 'Cat','PS1');

            arguments
                Obj
                Query
                Args.Cat        = [];
                Args.Method     = 'java';  %'http'|'java'
                Args.TapUrl     = [];   % []|'select' | or url
                Args.Ofmt       = 'csv';
                Args.TimeoutSec = 600;
                Args.JarFile    = VO.TopCat.getStiltsJarPath();
            end


            if ~isempty(Args.Cat)
                % replace %s in query with catalog name from CommonCat
                IndCat = find(strcmpi(Obj.CommonCat(:,1), Args.Cat));
                if isempty(IndCat)
                    error('Catalog %s was not found in VO.TopCat.CommonCat property (first column)', Args.Cat);
                end
                if numel(IndCat)>1
                    error('Catalog %s - Multiple entries found', Args.Cat);
                end

                Args.TapUrl = Obj.CommonCat(IndCat,4);
                TableName   = Obj.CommonCat(IndCat,3);
                TableName   = sprintf('"%s"', TableName);    
                Query = sprintf(Query, TableName);
                
            end

            if isempty(Args.TapUrl)
                Obj.selectTapServer;
                Args.TapUrl = Obj.TapUrl;
            end

            if strcmpi(Args.TapUrl, 'select')
                Obj.selectTapServer;
                Args.TapUrl = Obj.TapUrl;
            end

            if strcmpi(Args.Method, 'java') && ~isfile(Args.JarFile)
                Args.Method = 'http';
                fprintf('Jar File %s not found - switching to http method\n', Args.JarFile);
            end
            

            switch lower(Args.Method)
                case 'java'
                    T = VO.TopCat.queryStilts(Query, 'TapUrl',Obj.TapUrl, 'Ofmt',Args.Ofmt, 'TimeoutSec',Args.TimeoutSec);
                case 'http'
                    T = VO.TopCat.queryHttp(Query, 'TapUrl',Obj.TapUrl, 'Ofmt',Args.Ofmt, 'TimeoutSec',Args.TimeoutSec);
                otherwise
                    error('Unknown Method option');
            end

        end

        function Val=selectTapServer(Obj)
            % Select Tap Server using GUI
            % Input  : - self.
            % Output : - URL of Tap server AND Updated object with TapUrl
            % Author : Eran Ofek (Aug 2025)
            % Example: Tap=VO.TopCat; Tap.selectTapServer;

            Idx = tools.gui.selectStringGUI(Obj.TapList(:,1));
            Val = Obj.TapList(Idx, 2);
            Obj.TapUrl = Val;


        end


        function Result = searchCatalog(Obj, SearchString, Args)
            % Search catalogs/tables in a specific TAP service
            % Input  : - self.
            %          - A string array of strings to search.
            %            E.g., ["Ritter", "Cataclysmic"]
            %            E.g., "Pan-STARRS DR1"
            %            Default is [""].
            %          * ...,key,val,...
            %            'TapName' - Name of TAP service. Will search this
            %                   string, and will use the first service
            %                   found.
            %                   Default is 'VizieR'.
            %            'TapUrl' - Tap URL. If given, then TapName will be
            %                   ignored. Default is [].
            %            'Operator' - Operator betwewm searched strings.
            %                   'OR' | 'AND'.
            %                   Default is 'AND'.
            % Output : - Table of all found ttables.
            % Author : Eran Ofek (Sep 2025)
            % Example: Tap=VO.TopCat;
            %          Tap.searchCatalog(["Ritter", "Cataclysmic"])
            %          Tap.searchCatalog(["Pan-STARRS DR1"])

            arguments
                Obj
                SearchString   = [""];
                Args.TapName   = 'VizieR';
                Args.TapUrl    = [];
                Args.Operator  = 'AND';
            end

            if isempty(Args.TapUrl)
                % user TapName
                if isempty(Args.TapName)
                    % use GUI
                    Args.TapUrl = Obj.selectTapServer;
                else
                    % search TapName in Obj.TapList
                    IndUrl = find(contains(Obj.TapList(:,1), Args.TapName));
                    if numel(IndUrl)==0
                        error('Server name %s was not found in TapList', Args.TapName);
                    else
                        if numel(IndUrl)>1
                            ListTaps = tools.cell.sprintf_concatCell(", ", Obj.TapList(IndUrl,1));
                            fprintf('More than one server was found in TapList: %s\n', ListTaps);
                            fprintf('Server selected: %s\n', Obj.TapList(IndUrl,1));
                        end
                        IndUrl      = IndUrl(1);
                        Args.TapUrl = Obj.TapList(IndUrl,2);
                    end
                end
            end

            %Q = "SELECT schema_name, table_name, description FROM TAP_SCHEMA.tables WHERE description LIKE '%Ritter%' AND description LIKE '%Cataclysmic%';"

            Nss = numel(SearchString);
            WhereDescription = "";
            for Iss=1:1:Nss-1
                WhereDescription =  sprintf("%s description LIKE '%%%s%%' %s",WhereDescription, SearchString(Iss), Args.Operator);
            end
            WhereDescription =  sprintf("%s description LIKE '%%%s%%';",WhereDescription, SearchString(Nss));

            ListTpas = tools.cell.sprintf_concatCell(", ", Obj.TapList(IndUrl,1));
            Q = sprintf("SELECT schema_name, table_name, description FROM TAP_SCHEMA.tables WHERE %s", WhereDescription);
            Result = Obj.query(Q, 'TapUrl',Args.TapUrl);

        end

    end


    methods (Static)
        function Query=constructQueryAllTables()
            % Construct a query to get all table names
            % Input  : null
            % Output : Query string.
            % Author : Eran Ofek (Aug 2025)
            % Example: Query = VO.TopCat.constructQueryAllTables;

            Query = 'SELECT schema_name, table_name, description FROM TAP_SCHEMA.tables';
        end

        function Query = constructColumnNamesQuery(TableSchema, TableName)

            %VOCOLUMNSQUERYFROMTABLES Map TAP_SCHEMA.tables identifiers to a columns query.
            % Inputs (exactly as returned by TAP_SCHEMA.tables):
            %   SchemaNameFromTables : e.g. 'J_AA'  (VizieR) or 'gaiadr3' (Gaia)
            %   TableNameFromTables  : e.g. 'J/A+A/635/A13/table1' (VizieR) or 'gaia_source' (Gaia)
            %
            % Outputs:
            %   TableSchema : value to use in TAP_SCHEMA.columns.table_schema
            %   TableName   : value to use in TAP_SCHEMA.columns.table_name
            %   Query       : ADQL string to fetch the column metadata

            % Example: Query = VO.TopCat.constructQueryAllTables;
            %          Tbl=VO.TopCat.queryHttp(Query);
            %          Query=VO.TopCat.constructColumnNamesQuery(Tbl.schema_name{1}, Tbl.table_name{1})
            %          T=VO.TopCat.queryHttp(Query);
        
            TS = strrep(string(TableSchema), "'", "''");   % escape inner quotes
            TN = strrep(string(TableName),   "'", "''");
            Query  = "SELECT column_name, datatype, ucd, unit, description " + ...
                 "FROM TAP_SCHEMA.columns " + ...
                 "WHERE table_schema = '" + TS + "' AND table_name = '" + TN + "'";

            %SELECT c.column_name, c.datatype, c.ucd, c.unit, c.description
            %FROM TAP_SCHEMA.columns AS c
            %WHERE (c.schema_name = 'III_spectro' OR c.table_schema = 'III_spectro')
            %AND c.table_name = 'III/198/hyades'

            Query = sprintf("SELECT c.column_name, c.datatype, c.ucd, c.unit, c.description FROM TAP_SCHEMA.columns AS c WHERE (c.schema_name = '%s' OR c.table_schema = '%s') AND c.table_name = '%s'",...
                    TableSchema, TableSchema, TableName);


        end
                 
        function S = protectForUrlEncoded(Sin)
            % Protect '+' and '%' inside an x-www-form-urlencoded value
            % so servers don't turn '+' into spaces or double-interpret '%'.
            
            S = char(string(Sin));
            S = strrep(S, '%', '%25');   % encode '%' first
            S = strrep(S, '+', '%2B');   % then encode '+'
        end


        function SafeName = convertTableName(TableName, SchemaName)
            %CONVERTTABLENAME Quote a TAP/VizieR table (and optional schema) for ADQL.
            % Usage:
            %   Safe = convertTableName('J/A+A/635/A13/table1');
            %   Safe = convertTableName('gaiadr3');                       % simple
            %   Safe = convertTableName('J/A+A/635/A13/table1','J_AA');   % -> "J_AA"."J/A+A/635/A13/table1"
            %
            % Notes:
            % - ADQL identifiers with special chars (/ + etc.) MUST be double-quoted.
            % - We escape any embedded double quotes by doubling them per SQL rules.
            % - Returns a char vector (works on older MATLAB releases, too).
            
            if nargin < 2
                SchemaName = '';
            end
        
            % Coerce to char
            TN = char(TableName);
            SN = char(SchemaName);
        
            % Strip surrounding double quotes if already quoted
            if ~isempty(TN) && TN(1) == '"' && TN(end) == '"'
                TN = TN(2:end-1);
            end
            if ~isempty(SN) && SN(1) == '"' && SN(end) == '"'
                SN = SN(2:end-1);
            end
        
            % Escape embedded double quotes by doubling them
            TN = strrep(TN, '"', '""');
            SN = strrep(SN, '"', '""');
        
            if isempty(SN)
                SafeName = ['"' TN '"'];
            else
                SafeName = ['"' SN '"."' TN '"'];
            end
        end

        function Qfixed = normalizeVizierIdentifiers(Qin)
            % Rewrite common VizieR shorthand identifiers to their canonical form.
            % Current rule: "J/AA/..."  -> "J/A+A/..."
            Qfixed = char(string(Qin));
            % Only touch inside double-quoted identifiers (not SQL keywords)
            % "J/AA/..."  -> "J/A+A/..."
            Qfixed = regexprep(Qfixed, '"J/AA/', '"J/A+A/');
            % Also fix plain string literals if used in WHERE table_name comparisons
            Qfixed = regexprep(Qfixed, '''J/AA/', '''J/A+A/');
        end
              
        function s = escapeForShellDoubleQuotes(adql)
            % Convert to char
            s = char(string(adql));
            % Collapse newlines/tabs to spaces (TAP doesn’t need formatting)
            s = regexprep(s, '[\r\n\t]+', ' ');
            % Escape backslashes and double-quotes for a double-quoted shell argument
            s = regexprep(s, '(\\|")', '\\$1');
        end


    end

    methods (Static)

        function T = queryHttp(Query, Args) 
            % Execute TopCat query via http
            % Input  : - A query string.
            %          * ...,key,val,...
            %            'TapUrl' - Default is VO.TopCat.TapUrl
            %            'Ofmt' - Format. Default is 'csv'.
            %            'TimeoutSec' - Timeout in sec. Default is 600.
            % Output : - A table with results.
            % Author : ChatGPT, Eran Ofek (Aug 2025)
            % Example: Q='SELECT TOP 100 source_id, ra, dec FROM gaiaedr3.gaia_source WHERE phot_g_mean_mag < 12'
            %          T = VO.TopCat.queryHttp(Q)
                    
            arguments
                Query
                Args.TapUrl     = VO.TopCat.TapList{1,2};
                Args.Ofmt       = 'csv';
                Args.TimeoutSec = 600;
            end

            TapUrl     = char(Args.TapUrl);
            Ofmt       = Args.Ofmt;
            TimeoutSec = Args.TimeoutSec;

            % Normalize TapUrl (strip trailing slash)
            if endsWith(TapUrl,"/"), TapUrl = extractBefore(TapUrl, strlength(TapUrl)); end
        

            IsVizier = contains(lower(TapUrl), 'tapvizier');
            if IsVizier
                Query = VO.TopCat.normalizeVizierIdentifiers(Query);
            end

            % =========================
            % 1) /sync (simple & fast)
            % =========================
            try
                Params = struct('REQUEST','doQuery','LANG','ADQL','FORMAT',upper(Ofmt),'QUERY',VO.TopCat.protectForUrlEncoded(Query));
                Opts   = weboptions('Timeout', TimeoutSec, 'CharacterEncoding','auto');  % struct => form-encoded
                Data   = webwrite([TapUrl '/sync'], Params, Opts);
                T      = localreadcsvtotable(Data);
                return
            catch ME1
                SyncMsg = ME1.message;  %#ok<NASGU>
            end
        
            % =========================
            % 2) /async (robust UWS)
            % =========================
            import matlab.net.*
            import matlab.net.http.*
            import matlab.net.http.field.*
            import matlab.net.http.io.*
        
            try
                UriAsync = URI(char([TapUrl '/async']));  % scalar
                HttpOpts = HTTPOptions('ConnectTimeout',TimeoutSec,'ResponseTimeout',TimeoutSec,'MaxRedirects',0);
                HdrForm  = [ContentTypeField('application/x-www-form-urlencoded')];
        
                % --- Try single-shot create (parameters in create) ---
                Form1 = FormProvider('REQUEST','doQuery','LANG','ADQL','FORMAT',upper(Ofmt),'QUERY',VO.TopCat.protectForUrlEncoded(Query));
                Req1  = RequestMessage('post', HdrForm, Form1);
                Resp1 = Req1.send(UriAsync, HttpOpts);
        
                JobUrl = localgetjoburl(Resp1, TapUrl);  % scalar "" if not found
        
                % --- If no JobUrl, do two-step UWS: create -> parameters -> run ---
                if strlength(JobUrl) == 0
                    % Step A: create empty job (no body)
                    ReqA  = RequestMessage('post');
                    RespA = ReqA.send(UriAsync, HttpOpts);
                    JobUrl = localgetjoburl(RespA, TapUrl);
                    if strlength(JobUrl) == 0
                        error('runwithhttp:NoJobURL', 'Failed to obtain async job URL after two-step create.');
                    end
        
                    % Step B: set parameters
                    FormB = FormProvider('REQUEST','doQuery','LANG','ADQL','FORMAT',upper(Ofmt),'QUERY',VO.TopCat.protectForUrlEncoded(Query));
                    ReqB  = RequestMessage('post', HdrForm, FormB);
                    ReqB.send(URI(char(JobUrl) + "/parameters"), HttpOpts);
                end
        
                % Try to start the job (some services auto-run)
                try
                    RunReq = RequestMessage('post', HdrForm, FormProvider('PHASE','RUN'));
                    RunReq.send(URI(char(JobUrl) + "/phase"), HttpOpts);
                catch
                end
        
                % Poll until COMPLETED
                T0 = tic;
                while true
                    PhaseResp = RequestMessage('get').send(URI(char(JobUrl) + "/phase"), HttpOpts);
                    PhaseTxt  = strtrim(char(PhaseResp.Body.string));
                    if strcmpi(PhaseTxt,'COMPLETED')
                        break
                    elseif any(strcmpi(PhaseTxt, {'ERROR','ABORTED'}))
                        ErrMsg = '';
                        try
                            ErrResp = RequestMessage('get').send(URI(char(JobUrl) + "/error"), HttpOpts);
                            ErrMsg  = strtrim(char(ErrResp.Body.string));
                        catch
                        end
                        error('runwithhttp:AsyncError', 'TAP async job failed: %s', ErrMsg);
                    end
                    if toc(T0) > TimeoutSec
                        error('runwithhttp:Timeout', 'TAP async job timed out.');
                    end
                    pause(0.6);
                end
        
                % Discover result id (not always 'result')
                IdxResp = RequestMessage('get').send(URI(char(JobUrl) + "/results"), HttpOpts);
                IdxTxt  = char(IdxResp.Body.string);
                TokRes  = regexp(IdxTxt, '/results/([^"''>/\s]+)', 'tokens', 'once');
                if ~isempty(TokRes)
                    ResultId = string(TokRes{1});
                else
                    ResultId = "result"; % fallback
                end
        
                % Download the result
                ResResp = RequestMessage('get').send(URI(char(JobUrl) + "/results/" + char(ResultId)), HttpOpts);
                CsvText = ResResp.Body.string;
                T       = localreadcsvtotable(CsvText);
                return
        
            catch ME2
                if exist('SyncMsg','var')
                    error('VO:TopCat:HTTP', 'TAP HTTP query failed.\n/sync error: %s\n/async error: %s', ...
                          SyncMsg, ME2.message);
                else
                    error('VO:TopCat:HTTP', 'TAP HTTP query failed (async): %s', ME2.message);
                end
            end
        
            % ===== Nested helpers (always in scope) =====
            function JobUrl = localgetjoburl(Resp, TapUrl0)
                % Normalize base
                if endsWith(TapUrl0,"/"), TapUrl0 = extractBefore(TapUrl0, strlength(TapUrl0)); end
                JobUrl = "";
        
                % 1) Location header
                L = Resp.getFields('Location');
                if ~isempty(L), JobUrl = string(L(1).Value); end
        
                % 2) Content-Location header
                if strlength(JobUrl)==0
                    CL = Resp.getFields('Content-Location');
                    if ~isempty(CL), JobUrl = string(CL(1).Value); end
                end
        
                % 3) Body scraping (URL, HTML href, UWS XML)
                if strlength(JobUrl)==0
                    try
                        Txt = char(Resp.Body.string);
        
                        Tok = regexp(Txt,'https?://[^\s"''<>]+/async/[^\s"''<>]+','match','once');
                        if ~isempty(Tok), JobUrl = string(Tok); end
        
                        if strlength(JobUrl)==0
                            Tok = regexp(Txt,'href="([^"]*/async/[^"]+)"','tokens','once');
                            if ~isempty(Tok), JobUrl = string(Tok{1}); end
                        end
        
                        if strlength(JobUrl)==0
                            Tok = regexp(Txt,'<\s*jobId\s*>\s*([^<\s]+)\s*<\s*/\s*jobId\s*>','tokens','once');
                            if ~isempty(Tok), JobUrl = string(TapUrl0 + "/async/" + string(Tok{1})); end
                        end
        
                        if strlength(JobUrl)==0
                            Tok = regexp(Txt,'<\s*(?:uws:)?job[^>]*\sid="([^"]+)"','tokens','once');
                            if ~isempty(Tok), JobUrl = string(TapUrl0 + "/async/" + string(Tok{1})); end
                        end
                    catch
                    end
                end
        
                % If relative, make absolute
                if strlength(JobUrl) > 0 && startsWith(JobUrl,"/")
                    JobUrl = string(TapUrl0 + string(JobUrl));
                end
        
                % Force scalar string
                if strlength(JobUrl) > 0, JobUrl = string(JobUrl(1)); end
            end
        
            function Ttab = localreadcsvtotable(Data)
                if isa(Data,'string') || ischar(Data)
                    Buf = char(Data);
                    Outfile = [tempname,'.csv'];
                    Fid = fopen(Outfile,'w');  assert(Fid>0, 'Failed to create temp CSV.');
                    fwrite(Fid, Buf);          fclose(Fid);
                    Ttab = readtable(Outfile, 'FileType','text');
                    try, delete(Outfile); end
                elseif isa(Data,'uint8') || isa(Data,'int8')
                    Outfile = [tempname,'.csv'];
                    Fid = fopen(Outfile,'w');  assert(Fid>0, 'Failed to create temp CSV.');
                    fwrite(Fid, Data, 'uint8'); fclose(Fid);
                    Ttab = readtable(Outfile, 'FileType','text');
                    try, delete(Outfile); end
                else
                    Tmp = [tempname,'.csv'];
                    writematrix(Data, Tmp);
                    Ttab = readtable(Tmp, 'FileType','text');
                    try, delete(Tmp); end
                end
            end
        end
    
       

        function Result = getStiltsJarPath()
            % Get Stilt7s Jar file path
            % Input  : null
            % Output : Stilts Jar path
            % Author : Eran Ofek (Aug 2025)
            % Example: VO.TopCat.getStiltsJarPath()

            Result = fullfile(Installer.dataDir, 'Java', 'TopCat', 'stilts.jar');
        end

        function T = queryStilts(Query, Args)
            % Execute a TAP/ADQL query using Java STILTS (tapquery).
            % Input  : - Query.
            %          * ...,key,val,...
            %            'TapUrl' - TAP URL. Default: VO.TopCat.TapUrl
            %            'Ofmt' - (string) 'csv'|'tsv'|'fits'...  Default: 'csv'
            %            'StiltsCmd' -  (string) e.g. "stilts" (uses PATH) if StiltsJar not given
            %            'StiltsJar' -  (string) full path to stilts.jar (java -Xmx1g -jar "<jar>")
            %                   To download file use installer.
            %                   In = installer; In.install('TopCatJar');
            %            'TimeoutSec' - best-effort timeout (sec). Default: 600
            %            'WorkDir' - (string) directory for temp files. Default: tempdir
            % Output : T - table with query results (csv/tsv parsed via readtable)
            % Author : ChatGPT + Eran Ofek (Aug 2025)
            % Example: Q = "SELECT TOP 50 source_id, ra, dec FROM gaiaedr3.gaia_source WHERE phot_g_mean_mag < 12";
            %          T = VO.TopCat.queryStilts(Q);


            arguments
                Query
                Args.TapUrl     string = VO.TopCat.TapList{1,2};
                Args.Ofmt       string = "csv";
                Args.StiltsCmd  string = ""
                Args.StiltsJar  string = VO.TopCat.getStiltsJarPath();
                Args.TimeoutSec double = 600
                Args.WorkDir    string = string(tempdir)
            end
        
            TapUrl  = char(Args.TapUrl);
            Ofmt    = char(lower(Args.Ofmt));
            WorkDir = char(Args.WorkDir);
        
            % Normalize trailing slash
            if endsWith(TapUrl,"/"), TapUrl = extractBefore(TapUrl, strlength(TapUrl)); end
        
            % For VizieR endpoints, normalize "J/AA/..." -> "J/A+A/..." within quoted identifiers
            if contains(lower(TapUrl), 'tapvizier')
                Query = VO.TopCat.normalizeVizierIdentifiers(Query);
            end
        
            % Ensure work dir exists; prepare output temp file
            if ~exist(WorkDir,'dir'), mkdir(WorkDir); end
            outfile = [tempname(WorkDir) '_res.' Ofmt];
        
            % Build STILTS launcher (expand ~ in StiltsJar)
            base = "";
            if strlength(Args.StiltsJar) > 0
                jar = char(Args.StiltsJar);
                if ~isempty(jar) && jar(1)=='~'
                    home = getenv('HOME');
                    if isempty(home), home = char(java.lang.System.getProperty('user.home')); end
                    if strlength(jar) >= 2 && (jar(2)=='/' || jar(2)=='\')
                        jar = fullfile(home, jar(3:end));
                    end
                end
                base = sprintf('java -Xmx1g -jar "%s"', jar);
            elseif strlength(Args.StiltsCmd) > 0
                base = char(Args.StiltsCmd);
            else
                base = 'stilts';   % rely on PATH
            end
        
            % Escape ADQL for safe double-quoted shell argument
            AdqlEsc = VO.TopCat.escapeForShellDoubleQuotes(Query);
        
            % Assemble tapquery command (inline ADQL; force sync; capture stderr)
            cmdParts = {
                base, 'tapquery', ...
                ['tapurl="' TapUrl '"'], ...
                'language=ADQL', ...
                ['adql="' AdqlEsc '"'], ...
                'omode=out', ...
                ['ofmt=' Ofmt], ...
                ['out="' outfile '"'], ...
                'sync=true'
            };
            cmd = strjoin(cmdParts, ' ');


        
            % Run with best-effort timeout on Unix if available; always capture stderr
            if ispc
                [status, outTxt] = system(cmd + " 2>&1");
            else
                [hasTimeout,~] = system('command -v timeout >/dev/null 2>&1');
                if hasTimeout==0
                    [status, outTxt] = system(sprintf('timeout %ds %s 2>&1', round(Args.TimeoutSec), cmd));
                else
                    [status, outTxt] = system(cmd + " 2>&1");
                end
            end
        
            % Verify result / bubble up detailed error
            if status~=0 || ~exist(outfile,'file')
                if exist(outfile,'file')==2
                    try
                        delete(outfile);
                    end
                end %#ok<TRYNC>
                if status==124 || status==137
                    error('VO:TopCat:Stilts','STILTS timed out.\nCommand:\n%s\nOutput:\n%s', cmd, outTxt);
                else
                    error('VO:TopCat:Stilts','STILTS failed.\nCommand:\n%s\nOutput:\n%s', cmd, outTxt);
                end
            end
        
            % Load to table
            switch Ofmt
                case 'csv'
                    T = readtable(outfile, 'FileType','text', 'Delimiter','comma');
                case 'tsv'
                    T = readtable(outfile, 'FileType','text', 'Delimiter','tab');
<<<<<<< HEAD
                case 'fits'
                    T = AstroCatalog(outfile);
=======

                case 'fits'
                    % Read a FITS binary table with typed columns (int64/uint64 preserved)
                    % Requires the matlab.io.fits package (R2013b+)
                    if exist('matlab.io.fits.openFile','file') ~= 2
                        error(['FITS I/O not found. Your MATLAB lacks the matlab.io.fits package. ' ...
                               'Try a newer MATLAB or use Ofmt="votable" as a fallback.']);
                    end
                
                    % Locate a binary table HDU using fitsinfo
                    finfo = fitsinfo(outfile);  % returns struct
                    % Find the first binary table HDU
                    hIdx = [];
                    for k = 1:numel(finfo.Contents)
                        % In modern MATLAB, Type is like 'Binary table' or 'Image'
                        if isfield(finfo.Contents(k), 'Type') && strcmpi(finfo.Contents(k).Type,'Binary table')
                            hIdx = k; break
                        end
                        % Back-compat: some releases use Class or XTension fields
                        if isempty(hIdx)
                            if (isfield(finfo.Contents(k),'Class') && strcmpi(finfo.Contents(k).Class,'BinaryTableHDU')) || ...
                               (isfield(finfo.Contents(k),'XTension') && strcmpi(finfo.Contents(k).XTension,'BINTABLE'))
                                hIdx = k; break
                            end
                        end
                    end
                    if isempty(hIdx)
                        % STILTS usually writes primary image + ext#2 as table; fall back to HDU 2
                        hIdx = 2;
                    end
                
                    % Extract column metadata from fitsinfo
                    colsMeta = [];
                    if isfield(finfo.Contents(hIdx),'Fields')
                        colsMeta = finfo.Contents(hIdx).Fields;  % struct array with Name, etc.
                    end
                
                    % Open with fully-qualified FITS API
                    fptr = matlab.io.fits.openFile(outfile, 'READONLY');
                    cobj = onCleanup(@() matlab.io.fits.closeFile(fptr));
                    matlab.io.fits.movAbsHDU(fptr, hIdx);
                
                    % Determine number of columns (prefer info; otherwise query header)
                    if ~isempty(colsMeta)
                        ncol = numel(colsMeta);
                        varnames = string({colsMeta.Name});
                    else
                        % Query TFIELDS from header if fields are missing in this MATLAB version
                        ncol = double(matlab.io.fits.getNumCols(fptr));
                        varnames = strings(1, ncol);
                        for c = 1:ncol
                            try
                                varnames(c) = string(matlab.io.fits.getColName(fptr, c));
                            catch
                                varnames(c) = "Col"+c;
                            end
                        end
                    end
                
                    % Read all columns preserving their native integer types (incl. int64)
                    data = cell(1, ncol);
                    for c = 1:ncol
                        colvec = matlab.io.fits.readCol(fptr, c);
                        if isrow(colvec), colvec = colvec.'; end
                        data{c} = colvec;
                    end
                
                    % Build table (keeps int64/uint64 classes as-is)
                    T = table(data{:}, 'VariableNames', cellstr(varnames));


               

>>>>>>> 71e8fbe292391cb3d9bda1ac783d0af267fad66d
                otherwise
                    try
                        T = readtable(outfile, 'FileType','text');
                    catch
                        try, delete(outfile); end %#ok<TRYNC>
                        error('VO:TopCat:Stilts','Unsupported Ofmt for automatic parsing: %s', Ofmt);
                    end
            end
        
            % Cleanup
            try, delete(outfile); end %#ok<TRYNC>
        end
        



    end




    methods(Static) % Unit test

        Result = unitTest()
            % unitTest for OrbitalEl class
    end

end
