function [Result] = insertArchiveSourceCatalogs2DB(RootDir, Args)
    % Insert the source catalogues of archived LAST v1 visits into the src_visit / src_proc tables
    %     This is post-processing, not intended to be used in real time within the pipeline.
    %     Goes from visit to visit (proc/*v1 directories), reads the coadd and
    %     epoch catalogues, keeps and converts the columns selected in the
    %     table-design discussion #1411, adds the computed columns (id_coadd /
    %     id_proc, id_visit_src / id_proc_src, jd, bjd, baryvel,
    %     ingestion_time_jd, poisson_bias_mag), inserts the rows and stamps the
    %     visit .status file. The logic follows
    %     pipeline.last.insertDB.insertArchiveImageHeaders2DB.
    %     - src_visit: the coadd catalogues (*coadd_Cat_1.fits), header from HDU 3
    %     - src_proc : the epoch catalogues, from the visit's *proc_Cat_1.mat and
    %                  *proc_Head_1.mat if there are such (issue #1417), else from
    %                  the *proc_Cat_1.fits files (header from HDU 3).
    %     bjd and baryvel are computed for every source from jd, ra and dec
    %     (celestial.time.barycentricJD, jd in UTC, geocentric, as in
    %     imProc.db.insertCatalog); rows without ra/dec keep NaN.
    % Input  : - Root directory from where to inject the data.
    %          * ...,key,val,...
    %            'ProcDirTemplate' - Template of the visit dirs under RootDir. Default is '/proc/*'.
    %            'MinVersion'      - Visit dirs are named HHMMSSv<N>; only the latest reduction
    %                                of each visit is used, if N >= MinVersion. Default is 1.
    %            'ExcludeFolder'   - Skip visit dirs whose folder contains it. Default is '_re'.
    %            'Tables'          - Tables to fill. Default is ["src_visit", "src_proc"].
    %            'Spec'            - Discussion number or markdown file of the two tables.
    %                                For production runs pass the frozen .md file the tables
    %                                were created from. Default is 1411.
    %            'VisitTemplate'   - File template of the coadd catalogues. Default is 'LAST*coadd_Cat_1.fits'.
    %            'ProcTemplate'    - File template of the epoch catalogues. Default is 'LAST*proc_Cat_1.fits'.
    %            'HeadHDU'         - HDU of the image header in the catalogue FITS files. Default is 3.
    %            'ProcCatMat'      - File template of the epoch catalogue MAT file. Default is '*_sci_proc_Cat_1.mat'.
    %            'ProcHeadMat'     - File template of the epoch header MAT file. Default is '*_sci_proc_Head_1.mat'.
    %            'Db*'             - Database parameters. DbName default is 'last_test'.
    %            'PassKey'         - Passwords.yml entry of the DB password. Default is 'last'.
    %            'StatusStamp*'    - .status stamps of the two tables.
    %            'RemoteUser'      - User with write permission to the archive dirs (stamps are
    %                                written with su - RemoteUser). If empty, write directly.
    %                                Default is 'euclid'.
    %            'StampStatus'     - If false, insert without stamping the .status files (for tests on
    %                                real archive dirs; a later run inserts the visits again). Default is true.
    %            'DryRun'          - If true, build the tables but do not insert or stamp. Default is false.
    %            'MaxVisits'       - Stop after this number of ingested visits. Default is Inf.
    % Output : - Structure array, one element per processed visit: .Dir, .Nvisit, .Nproc
    %            (numbers of rows), .Status, .Time (seconds per stage: ReadVisit, ReadProc,
    %            ConvertVisit, ConvertProc, BjdVisit, BjdProc, InsertVisit, InsertProc, Stamp),
    %            and with DryRun also .Visit and .Proc (the tables built).
    % Author : A.M. Krassilchtchikov (2026 Oct)
    % Example: R = pipeline.last.insertDB.insertArchiveSourceCatalogs2DB('/bigdata3/projects/last/data/LAST.01.04.02/2026/10/20/', 'DryRun',true, 'MaxVisits',1);
    %          pipeline.last.insertDB.insertArchiveSourceCatalogs2DB(RootDir, 'Spec','src_tables_1411.md', 'StampStatus',false)

    arguments
        RootDir                = '/mnt/marvin/LAST.01*/';
        Args.ProcDirTemplate   = '/proc/*';
        Args.MinVersion        = 1;
        Args.ExcludeFolder     = '_re';
        Args.Tables            = ["src_visit", "src_proc"];

        Args.Spec              = 1411;
        Args.VisitTemplate     = 'LAST*coadd_Cat_1.fits';
        Args.ProcTemplate      = 'LAST*proc_Cat_1.fits';
        Args.HeadHDU           = 3;
        Args.ProcCatMat        = '*_sci_proc_Cat_1.mat';
        Args.ProcHeadMat       = '*_sci_proc_Head_1.mat';

        Args.DbHost            = 'euclid';
        Args.DbName            = 'last_test';
        Args.DbUser            = 'default';
        Args.DbPort            = 9000;
        Args.PassKey           = 'last';
        Args.AstroDBPassFile   = '~/.astropack/Passwords.yml';
        Args.ConnectorOpts     = struct('compression', db.mex.Compression.ZSTD);

        Args.StatusStampVisit  = "Injected into the src_visit table";
        Args.StatusStampProc   = "Injected into the src_proc table";
        Args.RemoteUser        = 'euclid';
        Args.StampStatus logical = true;
        Args.DryRun logical    = false;
        Args.MaxVisits         = Inf;
    end

    DoVisit = ismember("src_visit", Args.Tables);
    DoProc  = ismember("src_proc",  Args.Tables);
    Known   = ["id_coadd", "id_proc", "id_visit_src", "id_proc_src", "jd", "bjd", "baryvel", ...
               "ingestion_time_jd", "poisson_bias_mag"];

    % column specifications from the discussion text
    if DoVisit
        [~, SpecVisit, InfoVisit] = pipeline.last.insertDB.discussion2sql(Args.Spec, 'DbName',Args.DbName, ...
                                        'TableName','src_visit', 'TableTag',"visit");
        checkComputed(SpecVisit, Known);
    end
    if DoProc
        [~, SpecProc, InfoProc] = pipeline.last.insertDB.discussion2sql(Args.Spec, 'DbName',Args.DbName, ...
                                        'TableName','src_proc', 'TableTag',"proc");
        checkComputed(SpecProc, Known);
    end

    % ephemeris for the barycentric correction, loaded once
    IP = celestial.INPOP;
    IP.populateTables('Ear', 'FileData','pos');
    IP.populateTables('Ear', 'FileData','vel');
    IP.populateTables('TT');

    % connect to the DB
    if ~Args.DryRun
        Configuration.getSingleton().loadFile(Args.AstroDBPassFile); % tell the PM where to look for passwords
        PM  = PasswordsManager;
        Pwd = PM.search(Args.PassKey).Pass;
        DB  = db.mex.ClickHouseClient(Args.DbHost, Args.DbPort, Args.DbUser, Pwd, Args.ConnectorOpts);
        DB.query(sprintf('use %s', Args.DbName));
        if DoVisit
            SchemaVisit = DB.describe(InfoVisit.TableName);
            checkSchema(SpecVisit, SchemaVisit, InfoVisit.TableName);
        end
        if DoProc
            SchemaProc  = DB.describe(InfoProc.TableName);
            checkSchema(SpecProc, SchemaProc, InfoProc.TableName);
        end
    end

    FID = fopen('source_catalogs_no_status_dir.txt', 'a');

    % find all the visit directories according to the template
    D    = dir(fullfile(RootDir, Args.ProcDirTemplate));
    Dirs = D([D.isdir]);
    if ~isempty(Args.ExcludeFolder)
        Dirs = Dirs(~contains({Dirs.folder}, Args.ExcludeFolder));
    end
    Dirs = latestReduction(Dirs, Args.MinVersion);

    Result = struct('Dir',{}, 'Nvisit',{}, 'Nproc',{}, 'Status',{}, 'Time',{}, 'Visit',{}, 'Proc',{});
    Ndone  = 0;
    tic
    for Idir = 1:numel(Dirs)
        if Ndone >= Args.MaxVisits
            break
        end
        DataDir = fullfile(Dirs(Idir).folder, Dirs(Idir).name);

        % skip visits without .status, or already injected into all the requested tables
        try
            StatusText = fileread(fullfile(DataDir, '.status'));
        catch
            fprintf(FID, '%s \n', DataDir);
            continue
        end
        NeedVisit = DoVisit && ~contains(StatusText, Args.StatusStampVisit);
        NeedProc  = DoProc  && ~contains(StatusText, Args.StatusStampProc);
        if ~NeedVisit && ~NeedProc
            continue
        end

        Time = struct('ReadVisit',0, 'ReadProc',0, 'ConvertVisit',0, 'ConvertProc',0, 'BjdVisit',0, 'BjdProc',0, ...
                      'InsertVisit',0, 'InsertProc',0, 'Stamp',0);
        R = struct('Dir',DataDir, 'Nvisit',0, 'Nproc',0, 'Status',"", 'Time',Time, 'Visit',[], 'Proc',[]);
        fprintf('Injecting from %s ..', DataDir);
        try
            JDnow = posixtime(datetime('now', 'TimeZone','UTC'))./86400 + 2440587.5;
            if NeedVisit
                T0 = tic;
                [CatV, HeadV] = readFitsCatalogs(DataDir, Args.VisitTemplate, Args.HeadHDU);
                R.Time.ReadVisit = toc(T0);
                if isempty(CatV)
                    error('insertArchiveSourceCatalogs2DB:noCoadd', 'no coadd catalogues');
                end
                T0 = tic;
                TV = catalogs2columns(CatV, HeadV, SpecVisit, "id_coadd", "ID_COADD", "id_visit_src", JDnow);
                R.Time.ConvertVisit = toc(T0);
                T0 = tic;
                TV = addBarycentric(TV, SpecVisit, IP);
                R.Time.BjdVisit = toc(T0);
                R.Nvisit = numel(TV.ingestion_time_jd);
            end
            if NeedProc
                T0 = tic;
                [CatP, HeadP] = readEpochCatalogs(DataDir, Args);
                R.Time.ReadProc = toc(T0);
                if isempty(CatP)
                    error('insertArchiveSourceCatalogs2DB:noProc', 'no epoch catalogues');
                end
                T0 = tic;
                TP = catalogs2columns(CatP, HeadP, SpecProc, "id_proc", "ID_PROC", "id_proc_src", JDnow);
                R.Time.ConvertProc = toc(T0);
                T0 = tic;
                TP = addBarycentric(TP, SpecProc, IP);
                R.Time.BjdProc = toc(T0);
                R.Nproc = numel(TP.ingestion_time_jd);
            end
        catch ME
            Where = '';
            if ~isempty(ME.stack)
                Where = sprintf(' (%s, line %d)', ME.stack(1).name, ME.stack(1).line);
            end
            fprintf(' ..skipped: %s%s\n', ME.message, Where);
            R.Status = "read error: " + ME.message + Where;
            Result(end+1) = R; %#ok<AGROW>
            continue
        end

        if Args.DryRun
            if NeedVisit, R.Visit = TV; end
            if NeedProc,  R.Proc  = TP; end
            R.Status = "dry run";
        else
            % a DB error stops the run, as in insertArchiveImages2DB
            if NeedVisit
                T0 = tic;
                DB.insert(InfoVisit.TableName, TV, SchemaVisit);
                R.Time.InsertVisit = toc(T0);
                if Args.StampStatus
                    T0 = tic;
                    appendStatus(DataDir, Args.StatusStampVisit, Args.RemoteUser);
                    R.Time.Stamp = toc(T0);
                end
            end
            if NeedProc
                T0 = tic;
                DB.insert(InfoProc.TableName, TP, SchemaProc);
                R.Time.InsertProc = toc(T0);
                if Args.StampStatus
                    T0 = tic;
                    appendStatus(DataDir, Args.StatusStampProc, Args.RemoteUser);
                    R.Time.Stamp = R.Time.Stamp + toc(T0);
                end
            end
            R.Status = "injected";
        end
        fprintf([' ..done (%d coadd, %d epoch rows; read %.2f+%.2f s, convert %.2f+%.2f s, bjd %.2f+%.2f s, ' ...
                 'insert %.2f+%.2f s, stamp %.2f s)\n'], R.Nvisit, R.Nproc, R.Time.ReadVisit, R.Time.ReadProc, ...
                R.Time.ConvertVisit, R.Time.ConvertProc, R.Time.BjdVisit, R.Time.BjdProc, ...
                R.Time.InsertVisit, R.Time.InsertProc, R.Time.Stamp);
        Result(end+1) = R; %#ok<AGROW>
        Ndone = Ndone + 1;
    end
    toc
    fclose(FID);

    if ~Args.DryRun
        DB.delete;
    end
end

% internal functions

function Dirs = latestReduction(Dirs, MinVersion)
    % Keep the latest reduction (largest N of HHMMSSv<N>) of every visit, if N >= MinVersion
    Tok  = regexp({Dirs.name}, '^(\d{6})v(\d+)$', 'tokens', 'once');
    Good = ~cellfun(@isempty, Tok);
    Dirs = Dirs(Good);
    Tok  = vertcat(Tok{Good});
    if isempty(Dirs)
        return
    end
    Visit = strcat({Dirs.folder}', '/', Tok(:,1));
    Ver   = str2double(Tok(:,2));
    [~, ~, Ig] = unique(Visit);
    Keep = false(numel(Dirs), 1);
    for I = 1:max(Ig)
        In = find(Ig == I);
        [MaxVer, Im] = max(Ver(In));
        Keep(In(Im)) = MaxVer >= MinVersion;
    end
    Dirs = Dirs(Keep);
end

function checkSchema(Spec, Schema, TableName)
    % Error if the columns of the discussion text differ from those of the DB table
    InDb   = string(Schema.name);
    Extra  = setdiff(Spec.Column, InDb);
    Absent = setdiff(InDb, Spec.Column);
    if ~isempty(Extra) || ~isempty(Absent)
        error('insertArchiveSourceCatalogs2DB:schema', 'Table %s does not match the discussion: not in table: %s; not in discussion: %s', ...
              TableName, strjoin(Extra, ', '), strjoin(Absent, ', '));
    end
end

function checkComputed(Spec, Known)
    % Error if the discussion asks for a computed column this function does not compute
    Computed = Spec.Column(Spec.Kind == "computed");
    Unknown  = setdiff(Computed, Known);
    if ~isempty(Unknown)
        error('insertArchiveSourceCatalogs2DB:computed', 'No rule to compute column(s): %s', strjoin(Unknown, ', '));
    end
end

function [Cat, Head] = readFitsCatalogs(DataDir, Template, HDU)
    % Read the catalogues and their image headers of all files matching the template in a visit dir
    List = dir(fullfile(DataDir, Template));
    if isempty(List)
        Cat  = AstroCatalog.empty;
        Head = AstroHeader.empty;
        return
    end
    Files = fullfile(DataDir, {List.name});
    Cat   = AstroCatalog(Files);
    Head  = AstroHeader(Files, HDU);
end

function [Cat, Head] = readEpochCatalogs(DataDir, Args)
    % Read the epoch catalogues and headers of a visit: from the proc_Cat/proc_Head
    % MAT files if there are such, else from the proc_Cat FITS files
    if isempty(dir(fullfile(DataDir, Args.ProcHeadMat))) || isempty(dir(fullfile(DataDir, Args.ProcCatMat)))
        [Cat, Head] = readFitsCatalogs(DataDir, Args.ProcTemplate, Args.HeadHDU);
    else
        [Cat, Head] = pipeline.last.load.loadEpochCatMat(DataDir, 'Read',"both", ...
                                                         'CatTemplate',Args.ProcCatMat, 'HeadTemplate',Args.ProcHeadMat);
        Cat  = reshape(Cat.',  1, []);   % order of the FITS file names: epoch, then crop
        Head = reshape(Head.', 1, []);
    end
end

function T = catalogs2columns(Cat, Head, Spec, IdCol, IdKey, RowCol, JDnow)
    % Concatenate the catalogues into a struct of column arrays, as selected and typed in Spec,
    % and add the computed columns (except bjd and baryvel)
    Ncat  = numel(Cat);
    Nrow  = arrayfun(@(C) size(C.Catalog, 1), Cat(:));
    Ntot  = sum(Nrow);
    IsCat = find(Spec.Kind == "catalog")';
    T     = struct();
    for Icol = 1:height(Spec)
        T.(Spec.Column(Icol)) = emptyColumn(Spec.Type(Icol), Ntot);
    end
    ID  = zeros(Ncat, 1, 'uint64');
    JD  = nan(Ncat, 1);
    Pos = 0;
    for Ic = 1:Ncat
        if Nrow(Ic) == 0
            continue
        end
        Rows = Pos + (1:Nrow(Ic))';
        Data = Cat(Ic).Catalog;
        if istable(Data)
            Data = table2array(Data);
        end
        [Found, Ind] = ismember(lower(Spec.Keyword(IsCat)), lower(string(Cat(Ic).ColNames)));
        for Ik = find(Found(:))'
            Icol = IsCat(Ik);
            T.(Spec.Column(Icol))(Rows) = castColumn(Data(:, Ind(Ik)), Spec.Type(Icol));
        end
        ID(Ic) = idValue(keyValue(Head(Ic), IdKey));
        JD(Ic) = numValue(keyValue(Head(Ic), 'MIDJD'));
        if isfield(T, RowCol)
            T.(RowCol)(Rows) = (1:Nrow(Ic))';
        end
        Pos = Pos + Nrow(Ic);
    end
    if isfield(T, IdCol)
        T.(IdCol) = repelem(ID, Nrow);
    end
    if isfield(T, 'jd')
        T.jd = repelem(JD, Nrow);
    end
    if isfield(T, 'ingestion_time_jd')
        T.ingestion_time_jd(:) = JDnow;
    end
    if isfield(T, 'poisson_bias_mag')
        SN = double(T.sn);
        Bias = nan(Ntot, 1);
        Bias(SN > 0) = 1.0857 ./ SN(SN > 0).^2;
        T.poisson_bias_mag = castColumn(Bias, Spec.Type(Spec.Column == "poisson_bias_mag"));
    end
end

function T = addBarycentric(T, Spec, IP)
    % bjd and baryvel of every source with finite jd, ra, dec (jd in UTC, geocentric, as imProc.db.insertCatalog)
    if ~isfield(T, 'bjd') && ~isfield(T, 'baryvel')
        return
    end
    Good = isfinite(T.jd) & isfinite(T.ra) & isfinite(T.dec);
    if ~any(Good)
        return
    end
    [BJD, BVel] = celestial.time.barycentricJD(T.jd(Good), double(T.ra(Good)), double(T.dec(Good)), 'INPOP',IP, ...
                                               'GeoPos',[], 'InTimeScale','UTC', 'CooUnits','deg', 'VelOutUnits','cm/s');
    if isfield(T, 'bjd')
        T.bjd(Good) = castColumn(BJD, Spec.Type(Spec.Column == "bjd"));
    end
    if isfield(T, 'baryvel')
        T.baryvel(Good) = castColumn(BVel, Spec.Type(Spec.Column == "baryvel"));
    end
end

function Col = emptyColumn(Type, N)
    % Column of N default values with the MATLAB class of the ClickHouse type
    switch char(Type)
        case 'Float64'
            Col = nan(N, 1);
        case 'Float32'
            Col = nan(N, 1, 'single');
        case {'UInt8','UInt16','UInt32','UInt64','Int8','Int16','Int32','Int64'}
            Col = zeros(N, 1, lower(char(Type)));
        otherwise
            error('insertArchiveSourceCatalogs2DB:type', 'Unsupported column type %s', Type);
    end
end

function V = castColumn(V, Type)
    % Catalogue values -> MATLAB class of the ClickHouse type (NaN -> 0 for integer types)
    switch char(Type)
        case 'Float64'
            V = double(V);
        case 'Float32'
            V = single(V);
        otherwise
            V(isnan(V)) = 0;
            V = cast(round(V), lower(char(Type)));   % cast saturates at the type limits
    end
end

function V = numValue(Val)
    % Header value -> double (NaN if blank or not a number)
    if isnumeric(Val) || islogical(Val)
        if isempty(Val)
            V = NaN;
        else
            V = double(Val(1));
        end
    elseif ischar(Val) || isstring(Val)
        V = str2double(Val);
    else
        V = NaN;
    end
end

function Val = keyValue(H, Key)
    % Raw value of a header keyword ([] if missing)
    I = find(strcmp(H.Data(:,1), Key), 1);
    if isempty(I)
        Val = [];
    else
        Val = H.Data{I,2};
    end
end

function V = idValue(Val)
    % ID header value (text) -> uint64, 0 if missing
    V = uint64(0);
    if ischar(Val) || isstring(Val)
        T = sscanf(char(Val), '%lu');
        if ~isempty(T)
            V = T;
        end
    elseif isnumeric(Val) && ~isempty(Val) && ~isnan(Val)
        V = uint64(Val);
    end
end

function appendStatus(DataDir, Stamp, RemoteUser)
    % Append a time-stamped line to the visit .status file
    Line = sprintf('%s %s', tools.timeStamp.getTimeStamp, Stamp);
    if isempty(RemoteUser)
        FID = fopen(fullfile(DataDir, '.status'), 'a');
        if FID < 0
            error('insertArchiveSourceCatalogs2DB:status', 'Rows inserted but .status not stamped in %s', DataDir);
        end
        fprintf(FID, '%s\n', Line);
        fclose(FID);
    else
        Cmd = sprintf('su - %s -c "echo ''%s'' >> %s/.status"', RemoteUser, Line, DataDir);
        [Status, Out] = system(Cmd);
        if Status ~= 0
            % stop: with MergeTree an unstamped visit would be inserted again by the next run
            error('insertArchiveSourceCatalogs2DB:status', 'Rows inserted but .status not stamped in %s: %s', DataDir, Out);
        end
    end
end
