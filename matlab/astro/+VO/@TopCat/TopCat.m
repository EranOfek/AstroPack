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
%
% Simbad name resolver:
% Q="SELECT b.main_id, b.ra, b.dec FROM ident AS i JOIN basic AS b ON i.oidref=b.oid WHERE i.id = 'M 31'"
% T=Tap.query(Q, 'TapName','SIMBAD TAP')
%
% Example for cone search:
% SELECT * FROM swiftmastr WHERE 1 = CONTAINS( POINT('ICRS', RA, Dec), CIRCLE('ICRS', {RAdeg}, {Decdeg}, {SRdeg}) )
%Q = 'SELECT TOP 10 * FROM swiftmastr';
%T1 = VO.TopCat.queryStilts(Q, ...
%     'TapUrl','https://heasarc.gsfc.nasa.gov/xamin/vo/tap', ...
%     'Ofmt','csv','TimeoutSec',120);
%
% SDSS cone search
%   Q   = "SELECT * FROM sdss_dr16.specobjall WHERE snmedian>10 AND subClass LIKE '%WD%'"
%   T   = Tap.query(Q,'TapUrl','https://datalab.noirlab.edu/tap','Ofmt','csv','TimeoutSec',120);
%
% VLA archive cone search
%   Q = "SELECT TOP 5000 * FROM tap_schema.obscore WHERE 1=CONTAINS(POINT('ICRS',s_ra,s_dec),CIRCLE('ICRS',82.995,33.148,0.1)) AND t_min > 51000 AND t_min < 61000 ORDER BY t_min";  
%   T = Tap.query(Q,'TapUrl','https://data-query.nrao.edu/tap','Ofmt','csv','TimeoutSec',120);         




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
            %            'TapName' - If argument is not empty, then will use searchTapList
            %                   to search for Tap URL.
            %                   Default is [].
            %            'Ofmt' - Format. Default is 'csv'.
            %            'WorkDir' - (string) directory for temp files. Default: tempdir
            %            'TimeoutSec' - Timeout in sec. Default is 600.
            %            'SyncMode' - 'sync' (default) | 'async'.
            %            'JavaOpts' - (string) extra JVM flags. Default: ''
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
                Args.TapName    = [];
                Args.Ofmt       = 'csv';
                Args.WorkDir string = string(tempdir);
                Args.TimeoutSec = 600;
                Args.SyncMode   = 'sync';
                Args.JavaOpts   = '';
                Args.JarFile    = VO.TopCat.getStiltsJarPath();
            end

            if ~isempty(Args.TapName) && isempty(Args.TapUrl)
                Args.TapUrl = VO.TopCat.searchTapList(Args.TapName);
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
                %Obj.selectTapServer;
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
                    T = VO.TopCat.queryStilts(Query, 'TapUrl',Args.TapUrl, 'Ofmt',Args.Ofmt, 'TimeoutSec',Args.TimeoutSec, 'SyncMode',Args.SyncMode, 'JavaOpts',Args.JavaOpts, 'WorkDir',Args.WorkDir);
                case 'http'
                    T = VO.TopCat.queryHttp(Query, 'TapUrl',Args.TapUrl, 'Ofmt',Args.Ofmt, 'TimeoutSec',Args.TimeoutSec); % , 'WorkDir',Args.WorkDir);
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

        function Url=searchTapList(Name)
            % Search Tap URL by its name
            % Input  : - Tap server name.
            %          - Tap server URL.
            % Author : Eran Ofek (Sep 2025)
            % Example: Url = VO.TopCat.searchTapList('SIMBAD TAP');
            
            I=find(contains(Name,VO.TopCat.TapList(:,1)));
            if isempty(I)
                Url = [];
            else
                Url = VO.TopCat.TapList(I(1),2);
            end

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
            %            'SyncMode' - (string) 'sync' (default) | 'async'.
            %            'JavaOpts' - (string) extra JVM flags, e.g.
            %                   '-Dhttps.proxyHost=proxy -Dhttps.proxyPort=8080'
            %                   or '-Djava.net.preferIPv4Stack=true'. Default: ''
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
                Args.SyncMode   string = "sync"
                Args.JavaOpts   string = ""
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
                javaOpts = strtrim(char(Args.JavaOpts));
                if isempty(javaOpts)
                    base = sprintf('java -Xmx1g -jar "%s"', jar);
                else
                    base = sprintf('java -Xmx1g %s -jar "%s"', javaOpts, jar);
                end
            elseif strlength(Args.StiltsCmd) > 0
                base = char(Args.StiltsCmd);
            else
                base = 'stilts';   % rely on PATH
            end
        
            % Escape ADQL for safe double-quoted shell argument
            AdqlEsc = VO.TopCat.escapeForShellDoubleQuotes(Query);
        
            % Assemble tapquery command (inline ADQL; capture stderr)
            if strcmpi(Args.SyncMode, 'sync')
                SyncStr = 'true';
            else
                SyncStr = 'false';
            end
            cmdParts = {
                base, 'tapquery', ...
                ['tapurl="' TapUrl '"'], ...
                'language=ADQL', ...
                ['adql="' AdqlEsc '"'], ...
                'omode=out', ...
                ['ofmt=' Ofmt], ...
                ['out="' outfile '"'], ...
                ['sync=' SyncStr]
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
                case 'fits'
                    T = AstroCatalog(outfile);              
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


        function Result = splittedQuery(BaseQuery, Args)
            % Download large TAP query by splitting the sky into chunks
            % Package: @VO.TopCat
            % Description: Splits a large ADQL query into spatial chunks
            %              (declination bands, HealPix tiles, etc.) and
            %              downloads each chunk as a separate file using
            %              STILTS or HTTP. Supports resume, retries,
            %              sync/async modes, and multiple output formats.
            %              After downloading, chunks can be merged into a
            %              single file.
            % Input  : - Base ADQL query string containing a WHERE clause.
            %            The spatial constraint for each chunk is appended
            %            automatically.
            %          * ...,key,val,...
            %            --- Subdivision ---
            %            'Method'    - Sky subdivision method:
            %                          'DecBands' - declination bands (default)
            %                          'HealPix'  - HealPix tiles (placeholder)
            %            'DecEdges'  - Vector of declination band edges [deg].
            %                          Used when Method='DecBands'.
            %                          Default: -90:5:90 (36 bands of 5 deg).
            %            'DecColumn' - Name of the Dec column in ADQL table.
            %                          Default: 'dec'.
            %            'RAColumn'  - Name of the RA column in ADQL table.
            %                          Default: 'ra'.
            %            'HealPixNside' - HealPix Nside parameter (placeholder).
            %                          Default: 8.
            %            'ChunkIndices' - Vector of chunk indices to download.
            %                          Default: [] (all chunks).
            %            'DryRun'     - If true, list all chunks and return
            %                           without downloading. Default: false.
            %            --- Output ---
            %            'OutputDir'  - Directory for output files. Default: pwd.
            %            'FilePrefix' - Prefix for output filenames.
            %                           Default: 'tap_chunk'.
            %            'Ofmt'       - Output format: 'fits'|'csv'|'tsv'|'votable'.
            %                           Default: 'fits'.
            %            --- TAP connection ---
            %            'TapUrl'     - TAP service URL.
            %                           Default: Gaia ESA TAP.
            %            'SyncMode'   - 'async' (default) | 'sync'.
            %            'Backend'    - 'stilts' (default) | 'http'.
            %            'StiltsJar'  - Path to stilts.jar.
            %            'TimeoutSec' - Timeout per chunk [sec]. Default: 3600.
            %            'MaxRetries' - Retries per chunk. Default: 3.
            %            --- Post-processing ---
            %            'Merge'      - 'none' (default) | 'fits' | 'table'.
            %            'MergeFile'  - Output filename for merged result.
            %            'DeleteChunks' - Delete chunks after merge. Default: false.
            %            'Verbose'    - Print progress. Default: true.
            % Output : - Struct with fields:
            %            .Files    - Cell array of output file paths.
            %            .Status   - Vector (0=success, NaN=not attempted).
            %            .Nchunks  - Number of chunks.
            %            .Chunks   - Nx3 cell: {Label, ADQLConstraint, FileSuffix}.
            %            .Queries  - Cell array of full ADQL queries.
            %            .MergedFile - Path to merged file (if Merge~='none').
            % Author : Dana Kovaleva (Feb 2026)
            % Example:
            %   Q = ['SELECT * FROM gaiaedr3.gaia_source ', ...
            %        'WHERE parallax > 1 AND parallax_over_error > 5'];
            %   Result = VO.TopCat.splittedQuery(Q, 'DryRun', true);
            %   Result = VO.TopCat.splittedQuery(Q, ...
            %       'OutputDir', '/data/gaia', 'FilePrefix', 'gaia_plx');
            %   Result = VO.TopCat.splittedQuery(Q, 'ChunkIndices', 5:10);

            arguments
                BaseQuery
                Args.Method         = 'DecBands'
                Args.DecEdges       = -90:5:90
                Args.DecColumn      = 'dec'
                Args.RAColumn       = 'ra'
                Args.HealPixNside   = 8
                Args.ChunkIndices   = []
                Args.DryRun logical = false
                Args.OutputDir      = pwd
                Args.FilePrefix     = 'tap_chunk'
                Args.Ofmt           = 'fits'
                Args.TapUrl         = 'https://gea.esac.esa.int/tap-server/tap'
                Args.SyncMode       = 'async'
                Args.Backend        = 'stilts'
                Args.StiltsJar      = VO.TopCat.getStiltsJarPath()
                Args.TimeoutSec     = 3600
                Args.MaxRetries     = 3
                Args.Merge          = 'none'
                Args.MergeFile      = ''
                Args.DeleteChunks   = false
                Args.Verbose        = true
            end

            BaseQuery = strtrim(char(BaseQuery));
            if endsWith(BaseQuery, ';'), BaseQuery = BaseQuery(1:end-1); end

            TapUrl = char(Args.TapUrl);
            if endsWith(TapUrl, '/'), TapUrl = TapUrl(1:end-1); end

            Ofmt = lower(char(Args.Ofmt));

            % Generate chunk definitions
            switch lower(Args.Method)
                case 'decbands'
                    Chunks = localGenerateDecBandChunks(Args.DecEdges, Args.DecColumn);
                case 'healpix'
                    Chunks = localGenerateHealPixChunks(Args.HealPixNside, ...
                                 Args.RAColumn, Args.DecColumn);
                otherwise
                    error('VO:TopCat:splittedQuery', ...
                        'Unknown Method ''%s''. Supported: DecBands, HealPix.', ...
                        Args.Method);
            end

            Nchunks = size(Chunks, 1);

            % DryRun: list chunks and return
            if Args.DryRun
                fprintf('Chunk list (%d chunks, Method=%s):\n', Nchunks, Args.Method);
                fprintf('  %5s  %-30s  %s\n', 'Index', 'Label', 'Output file');
                fprintf('  %5s  %-30s  %s\n', '-----', '-----', '-----------');
                for Ic = 1:Nchunks
                    OutFile = fullfile(Args.OutputDir, ...
                        sprintf('%s_%s.%s', Args.FilePrefix, Chunks{Ic, 3}, Ofmt));
                    if exist(OutFile, 'file') == 2
                        Mark = ' [exists]';
                    else
                        Mark = '';
                    end
                    fprintf('  %5d  %-30s  %s%s\n', Ic, Chunks{Ic, 1}, OutFile, Mark);
                end
                Result.Files   = cell(Nchunks, 1);
                Result.Status  = nan(Nchunks, 1);
                Result.Nchunks = Nchunks;
                Result.Chunks  = Chunks;
                Result.Queries = cell(Nchunks, 1);
                Result.MergedFile = '';
                for Ic = 1:Nchunks
                    Result.Queries{Ic} = sprintf('%s AND %s', BaseQuery, Chunks{Ic, 2});
                    Result.Files{Ic}   = fullfile(Args.OutputDir, ...
                        sprintf('%s_%s.%s', Args.FilePrefix, Chunks{Ic, 3}, Ofmt));
                end
                return;
            end

            % Select subset of chunks
            if ~isempty(Args.ChunkIndices)
                ValidIdx = Args.ChunkIndices(Args.ChunkIndices >= 1 & ...
                                             Args.ChunkIndices <= Nchunks);
                if isempty(ValidIdx)
                    error('VO:TopCat:splittedQuery', ...
                        'No valid ChunkIndices in range [1, %d].', Nchunks);
                end
            else
                ValidIdx = 1:Nchunks;
            end

            if ~exist(Args.OutputDir, 'dir')
                mkdir(Args.OutputDir);
            end

            % Build queries and filenames
            Result.Files   = cell(Nchunks, 1);
            Result.Status  = nan(Nchunks, 1);
            Result.Nchunks = Nchunks;
            Result.Chunks  = Chunks;
            Result.Queries = cell(Nchunks, 1);
            Result.MergedFile = '';

            for Ic = 1:Nchunks
                Result.Queries{Ic} = sprintf('%s AND %s', BaseQuery, Chunks{Ic, 2});
                Result.Files{Ic}   = fullfile(Args.OutputDir, ...
                    sprintf('%s_%s.%s', Args.FilePrefix, Chunks{Ic, 3}, Ofmt));
            end

            % Download loop
            TotalTic = tic;

            for Ii = 1:numel(ValidIdx)
                Ic = ValidIdx(Ii);
                Label   = Chunks{Ic, 1};
                Query   = Result.Queries{Ic};
                OutFile = Result.Files{Ic};

                % Resume: skip existing files
                if exist(OutFile, 'file') == 2
                    Info = dir(OutFile);
                    if Info.bytes > 0
                        Result.Status(Ic) = 0;
                        if Args.Verbose
                            fprintf('[%d/%d] SKIP %s — exists (%.1f MB)\n', ...
                                Ii, numel(ValidIdx), Label, Info.bytes/1e6);
                        end
                        continue;
                    end
                end

                Result.Status(Ic) = 1;
                for Itry = 1:Args.MaxRetries
                    if Args.Verbose
                        fprintf('[%d/%d] %s attempt %d/%d ...', ...
                            Ii, numel(ValidIdx), Label, Itry, Args.MaxRetries);
                    end
                    ChunkTic = tic;

                    try
                        switch lower(Args.Backend)
                            case 'stilts'
                                [Status, OutTxt] = localExecuteStilts(Query, OutFile, ...
                                    TapUrl, Ofmt, Args.SyncMode, ...
                                    Args.StiltsJar, Args.TimeoutSec);
                            case 'http'
                                Status = localExecuteHttp(Query, OutFile, ...
                                    TapUrl, Ofmt, Args.TimeoutSec);
                                OutTxt = '';
                            otherwise
                                error('Unknown Backend: %s', Args.Backend);
                        end
                    catch ME
                        Status = -1;
                        OutTxt = ME.message;
                    end

                    Elapsed = toc(ChunkTic);

                    if Status == 0 && exist(OutFile, 'file') == 2
                        Info = dir(OutFile);
                        if Args.Verbose
                            fprintf(' OK (%.1f s, %.1f MB)\n', Elapsed, Info.bytes/1e6);
                        end
                        Result.Status(Ic) = 0;
                        break;
                    else
                        if Args.Verbose
                            fprintf(' FAILED (status=%d, %.1f s)\n', Status, Elapsed);
                            if ~isempty(OutTxt)
                                fprintf('  %s\n', strtrim(OutTxt));
                            end
                        end
                        if exist(OutFile, 'file') == 2
                            delete(OutFile);
                        end
                        if Itry < Args.MaxRetries
                            pause(5 * Itry);
                        end
                    end
                end

                if Result.Status(Ic) ~= 0
                    fprintf('  WARNING: chunk %d (%s) failed after %d retries\n', ...
                        Ic, Label, Args.MaxRetries);
                end
            end

            % Post-processing: merge
            if ~strcmpi(Args.Merge, 'none')
                SuccessIdx = find(Result.Status == 0);
                SuccessFiles = Result.Files(SuccessIdx);

                if isempty(SuccessFiles)
                    fprintf('No successful downloads to merge.\n');
                else
                    if isempty(Args.MergeFile)
                        Args.MergeFile = fullfile(Args.OutputDir, ...
                            sprintf('%s_merged.%s', Args.FilePrefix, Ofmt));
                    end
                    if Args.Verbose
                        fprintf('Merging %d chunks into %s ...\n', ...
                            numel(SuccessFiles), Args.MergeFile);
                    end
                    localMergeChunks(SuccessFiles, Args.MergeFile, Ofmt);
                    Result.MergedFile = Args.MergeFile;

                    if Args.DeleteChunks
                        for If = 1:numel(SuccessFiles)
                            delete(SuccessFiles{If});
                        end
                        if Args.Verbose
                            fprintf('Deleted %d chunk files.\n', numel(SuccessFiles));
                        end
                    end
                end
            end

            % Summary
            Nattempted = sum(~isnan(Result.Status));
            Nsuccess   = sum(Result.Status == 0);
            if Args.Verbose
                fprintf('\nDone: %d/%d chunks downloaded (%.1f min total)\n', ...
                    Nsuccess, Nattempted, toc(TotalTic)/60);
                if Nattempted < Nchunks
                    fprintf('  (%d chunks were not attempted)\n', Nchunks - Nattempted);
                end
                FailIdx = find(Result.Status == 1);
                if ~isempty(FailIdx)
                    fprintf('Failed chunks:\n');
                    for If = 1:numel(FailIdx)
                        fprintf('  %s\n', Chunks{FailIdx(If), 1});
                    end
                end
            end

            % ===== Nested helpers =====

            function Chunks = localGenerateDecBandChunks(DecEdges, DecColumn)
                DecEdges = DecEdges(:)';
                Nbands   = numel(DecEdges) - 1;
                Chunks   = cell(Nbands, 3);
                for Ib = 1:Nbands
                    DecLo = DecEdges(Ib);
                    DecHi = DecEdges(Ib + 1);
                    Chunks{Ib, 1} = sprintf('Dec [%+.1f, %+.1f]', DecLo, DecHi);
                    Chunks{Ib, 2} = sprintf('%s BETWEEN %.6f AND %.6f', ...
                                            DecColumn, DecLo, DecHi);
                    LoStr = strrep(sprintf('%+.0f', DecLo), '+', 'p');
                    LoStr = strrep(LoStr, '-', 'm');
                    HiStr = strrep(sprintf('%+.0f', DecHi), '+', 'p');
                    HiStr = strrep(HiStr, '-', 'm');
                    Chunks{Ib, 3} = sprintf('dec_%s_%s', LoStr, HiStr);
                end
            end

            function Chunks = localGenerateHealPixChunks(Nside, RAColumn, DecColumn) %#ok<INUSD>
                Npix = 12 * Nside^2;
                error('VO:TopCat:splittedQuery:NotImplemented', ...
                    ['HealPix subdivision is not yet implemented. ', ...
                     'Npix would be %d for Nside=%d.'], Npix, Nside);
            end

            function [Status, OutTxt] = localExecuteStilts(Query, OutFile, ...
                    TapUrl0, Ofmt0, SyncMode, StiltsJar, TimeoutSec)
                Jar = char(StiltsJar);
                if ~isempty(Jar) && Jar(1) == '~'
                    Home = getenv('HOME');
                    if strlength(Jar) >= 2 && (Jar(2) == '/' || Jar(2) == '\')
                        Jar = fullfile(Home, Jar(3:end));
                    end
                end
                StiltsBase = sprintf('java -Xmx2g -jar "%s"', Jar);
                AdqlEsc = strrep(char(Query), '"', '\"');
                if strcmpi(SyncMode, 'sync')
                    SyncStr = 'true';
                else
                    SyncStr = 'false';
                end
                Cmd = sprintf('%s tapquery tapurl="%s" language=ADQL adql="%s" omode=out ofmt=%s out="%s" sync=%s 2>&1', ...
                    StiltsBase, TapUrl0, AdqlEsc, Ofmt0, OutFile, SyncStr);
                if ispc
                    [Status, OutTxt] = system(Cmd);
                else
                    [HasTimeout, ~] = system('command -v timeout >/dev/null 2>&1');
                    if HasTimeout == 0
                        [Status, OutTxt] = system(sprintf('timeout %ds %s', ...
                            round(TimeoutSec), Cmd));
                    else
                        [Status, OutTxt] = system(Cmd);
                    end
                end
            end

            function Status = localExecuteHttp(Query, OutFile, TapUrl0, Ofmt0, TimeoutSec)
                try
                    T = VO.TopCat.queryHttp(char(Query), ...
                        'TapUrl', TapUrl0, 'Ofmt', Ofmt0, 'TimeoutSec', TimeoutSec);
                    if strcmpi(Ofmt0, 'fits')
                        T.write1(OutFile);
                    else
                        writetable(T, OutFile);
                    end
                    Status = 0;
                catch
                    Status = 1;
                end
            end

            function localMergeChunks(ChunkFiles, MergeFile, Ofmt0)
                try
                    MergeJar = char(VO.TopCat.getStiltsJarPath());
                    MergeBase = sprintf('java -Xmx4g -jar "%s"', MergeJar);
                    InArgs = '';
                    for Jf = 1:numel(ChunkFiles)
                        InArgs = sprintf('%s in%d="%s" ifmt%d=%s', ...
                            InArgs, Jf, ChunkFiles{Jf}, Jf, Ofmt0);
                    end
                    Cmd = sprintf('%s tcat%s omode=out ofmt=%s out="%s" 2>&1', ...
                        MergeBase, InArgs, Ofmt0, MergeFile);
                    [Status, OutTxt] = system(Cmd);
                    if Status ~= 0
                        error('STILTS tcat failed: %s', OutTxt);
                    end
                catch ME
                    fprintf('STILTS merge failed (%s), falling back to MATLAB.\n', ME.message);
                    AllData = [];
                    for Jf = 1:numel(ChunkFiles)
                        switch lower(Ofmt0)
                            case 'fits'
                                T = FITS.readTable1(ChunkFiles{Jf}, 'OutClass', []);
                            otherwise
                                T = readtable(ChunkFiles{Jf});
                        end
                        if isempty(AllData)
                            AllData = T;
                        else
                            AllData = [AllData; T]; %#ok<AGROW>
                        end
                        fprintf('  Merged chunk %d/%d\n', Jf, numel(ChunkFiles));
                    end
                    switch lower(Ofmt0)
                        case 'fits'
                            AC = AstroCatalog;
                            AC.Catalog = AllData;
                            AC.write1(MergeFile);
                        otherwise
                            writetable(AllData, MergeFile);
                    end
                end
            end

        end


    end




    methods(Static) % Unit test

        Result = unitTest()
            % unitTest for OrbitalEl class
    end

end
