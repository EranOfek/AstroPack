function [Result] = insertArchiveImageHeaders2DB(RootDir, Args)
    % Insert the headers of archived LAST v1 coadd and epoch images into the image_visit / image_proc tables
    %     This is post-processing, not intended to be used in real time within the pipeline.
    %     Goes from visit to visit (proc/*v1 directories), reads the headers,
    %     keeps and converts the columns selected in the table-design
    %     discussions (#1409 image_visit, #1413 image_proc), adds the computed
    %     columns (ingestion_time_jd, id_coadd), inserts the rows and stamps
    %     the visit .status file. The logic follows
    %     pipeline.last.insertDB.insertArchiveImages2DB.
    %     - image_visit: header of the coadd images (HDU 1 of *coadd_Image_1.fits)
    %     - image_proc : image header of the epoch catalogues (from the visit's
    %                    *proc_Head_1.mat if there is one (issue #1417), else
    %                    HDU 3 of *proc_Cat_1.fits);
    %                    id_coadd is the ID_COADD of the coadd with the same CROPID
    %                    whose ID_PROCF..ID_PROCL range contains the epoch ID_PROC.
    % Input  : - Root directory from where to inject the data.
    %          * ...,key,val,...
    %            'ProcDirTemplate' - Template of the visit dirs under RootDir. Default is '/proc/*'.
    %            'MinVersion'      - Visit dirs are named HHMMSSv<N>, where N counts the reductions
    %                                (0 = DemonLAST, from 1 = PipelineDemon). Only the latest
    %                                reduction of each visit is used, if N >= MinVersion. Default is 1.
    %            'ExcludeFolder'   - Skip visit dirs whose folder contains it. Default is '_re'.
    %            'Tables'          - Tables to fill. Default is ["image_visit", "image_proc"].
    %            'VisitSpec'       - Discussion number or markdown file of image_visit. Default is 1409.
    %            'ProcSpec'        - Discussion number or markdown file of image_proc. Default is 1413.
    %                                For production runs pass the frozen .md file the table was
    %                                created from; the columns must match the DB table.
    %            'VisitTemplate'   - File template of the coadd images. Default is 'LAST*coadd_Image_1.fits'.
    %            'VisitHDU'        - HDU of the coadd header. Default is 1.
    %            'ProcTemplate'    - File template of the epoch catalogues. Default is 'LAST*proc_Cat_1.fits'.
    %            'ProcHDU'         - HDU of the epoch image header. Default is 3.
    %            'ProcHeadMat'     - File template of the epoch header MAT file written by
    %                                pipelineI instead of the proc_Cat FITS files.
    %                                Default is '*_sci_proc_Head_1.mat'.
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
    %            'Logger'          - A MsgLogger (pipeline.last.insertDB.ingestLogger) for the run
    %                                start and end, each visit and each error. If empty, create one:
    %                                ~/matlab/log/last/insertDB/<yyyy>/<yyyy-mm-dd>/insertDB_tables.log.
    %                                Default is [].
    % Output : - Structure array, one element per processed visit: .Dir, .Nvisit, .Nproc,
    %            .Status, .Time (seconds per stage: ReadCoadd, ReadEpoch, ConvertVisit,
    %            ConvertProc (incl. id_coadd), InsertVisit, InsertProc, Stamp),
    %            and with DryRun also .Visit and .Proc (the tables built).
    % Author : A.M. Krassilchtchikov (2026 Oct)
    % Example: R = pipeline.last.insertDB.insertArchiveImageHeaders2DB('/last02e/data1/archive/LAST.01.02.01/2026/04/16/', 'DryRun',true, 'MaxVisits',1);
    %          pipeline.last.insertDB.insertArchiveImageHeaders2DB(RootDir, 'VisitSpec','image_visit_1409.md', 'ProcSpec','image_proc_1413.md')
    %          pipeline.last.insertDB.insertArchiveImageHeaders2DB('/mnt/marvin/', 'ProcDirTemplate','LAST*/2026/*/*/proc/*')

    arguments
        RootDir                = '/mnt/marvin/LAST.01*/';
        Args.ProcDirTemplate   = '/proc/*';
        Args.MinVersion        = 1;
        Args.ExcludeFolder     = '_re';
        Args.Tables            = ["image_visit", "image_proc"];

        Args.VisitSpec         = 1409;
        Args.ProcSpec          = 1413;
        Args.VisitTemplate     = 'LAST*coadd_Image_1.fits';
        Args.VisitHDU          = 1;
        Args.ProcTemplate      = 'LAST*proc_Cat_1.fits';
        Args.ProcHDU           = 3;
        Args.ProcHeadMat       = '*_sci_proc_Head_1.mat';

        Args.DbHost            = 'euclid';
        Args.DbName            = 'last_test';
        Args.DbUser            = 'default';
        Args.DbPort            = 9000;
        Args.PassKey           = 'last';
        Args.AstroDBPassFile   = '~/.astropack/Passwords.yml';
        Args.ConnectorOpts     = struct('compression', db.mex.Compression.ZSTD);

        Args.StatusStampVisit  = "Injected into the image_visit table";
        Args.StatusStampProc   = "Injected into the image_proc table";
        Args.RemoteUser        = 'euclid';
        Args.StampStatus logical = true;
        Args.DryRun logical    = false;
        Args.MaxVisits         = Inf;
        Args.Logger            = [];
    end

    DoVisit = ismember("image_visit", Args.Tables);
    DoProc  = ismember("image_proc",  Args.Tables);

    if isempty(Args.Logger)
        Args.Logger = pipeline.last.insertDB.ingestLogger;
    end
    Log  = @(Level, Fmt, varargin) pipeline.last.insertDB.ingestLog(Args.Logger, Level, ['insertArchiveImageHeaders2DB ' Fmt], varargin{:});
    TRun = tic;
    Log(LogLevel.Info, 'start on %s: RootDir %s, ProcDirTemplate %s; tables %s in %s:%d/%s; specs %s, %s; DryRun %d, StampStatus %d, MaxVisits %g', ...
        hostName, RootDir, Args.ProcDirTemplate, strjoin(Args.Tables, ','), Args.DbHost, Args.DbPort, Args.DbName, ...
        string(Args.VisitSpec), string(Args.ProcSpec), Args.DryRun, Args.StampStatus, Args.MaxVisits);

    try
        % column specifications from the discussion texts
        if DoVisit
            [~, SpecVisit, InfoVisit] = pipeline.last.insertDB.discussion2sql(Args.VisitSpec, 'DbName',Args.DbName);
        end
        if DoProc
            [~, SpecProc, InfoProc] = pipeline.last.insertDB.discussion2sql(Args.ProcSpec, 'DbName',Args.DbName);
        end
        % only these computed columns are known to this function
        if DoVisit
            checkComputed(SpecVisit, "ingestion_time_jd");
        end
        if DoProc
            checkComputed(SpecProc, ["ingestion_time_jd", "id_coadd"]);
        end

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
    catch ME
        Log(LogLevel.Error, 'stopped before the first visit: %s%s', strtrim(ME.message), errWhere(ME));
        rethrow(ME)
    end

    % find all the visit directories according to the template
    D    = dir(fullfile(RootDir, Args.ProcDirTemplate));
    Dirs = D([D.isdir]);
    if ~isempty(Args.ExcludeFolder)
        Dirs = Dirs(~contains({Dirs.folder}, Args.ExcludeFolder));
    end
    Dirs = latestReduction(Dirs, Args.MinVersion);
    Log(LogLevel.Info, '%d visit dirs (latest reduction, v%d or later)', numel(Dirs), Args.MinVersion);

    Result = struct('Dir',{}, 'Nvisit',{}, 'Nproc',{}, 'Status',{}, 'Time',{}, 'Visit',{}, 'Proc',{});
    Ndone  = 0;
    Count  = struct('NoStatus',0, 'Already',0);
    for Idir = 1:numel(Dirs)
        if Ndone >= Args.MaxVisits
            break
        end
        DataDir = fullfile(Dirs(Idir).folder, Dirs(Idir).name);

        % skip visits without .status, or already injected into all the requested tables
        try
            StatusText = fileread(fullfile(DataDir, '.status'));
        catch
            Log(LogLevel.Warning, 'skipped %s: no .status', DataDir);
            Count.NoStatus = Count.NoStatus + 1;
            continue
        end
        NeedVisit = DoVisit && ~contains(StatusText, Args.StatusStampVisit);
        NeedProc  = DoProc  && ~contains(StatusText, Args.StatusStampProc);
        if ~NeedVisit && ~NeedProc
            Count.Already = Count.Already + 1;
            continue
        end

        Time = struct('ReadCoadd',0, 'ReadEpoch',0, 'ConvertVisit',0, 'ConvertProc',0, 'InsertVisit',0, 'InsertProc',0, 'Stamp',0);
        R = struct('Dir',DataDir, 'Nvisit',0, 'Nproc',0, 'Status',"", 'Time',Time, 'Visit',[], 'Proc',[]);
        try
            % coadd headers: needed for image_visit and for id_coadd of image_proc
            T0 = tic;
            HC = readHeaders(DataDir, Args.VisitTemplate, Args.VisitHDU);
            R.Time.ReadCoadd = toc(T0);
            if isempty(HC)
                error('insertArchiveImageHeaders2DB:noCoadd', 'no coadd images');
            end
            JDnow = posixtime(datetime('now', 'TimeZone','UTC'))./86400 + 2440587.5;

            if NeedVisit
                T0 = tic;
                TV = headers2columns(HC, SpecVisit);
                TV.ingestion_time_jd(:) = JDnow;
                R.Time.ConvertVisit = toc(T0);
                R.Nvisit = numel(HC);
            end
            if NeedProc
                T0 = tic;
                HP = readEpochHeaders(DataDir, Args);
                R.Time.ReadEpoch = toc(T0);
                if isempty(HP)
                    error('insertArchiveImageHeaders2DB:noProc', 'no epoch catalogues');
                end
                T0 = tic;
                TP = headers2columns(HP, SpecProc);
                TP.ingestion_time_jd(:) = JDnow;
                TP.id_coadd = matchCoaddID(HP, HC);
                R.Time.ConvertProc = toc(T0);
                R.Nproc = numel(HP);
            end
        catch ME
            Log(LogLevel.Warning, 'skipped %s: %s%s', DataDir, strtrim(ME.message), errWhere(ME));
            R.Status = "read error: " + strtrim(ME.message) + errWhere(ME);
            Result(end+1) = R; %#ok<AGROW>
            continue
        end

        if Args.DryRun
            if NeedVisit, R.Visit = TV; end
            if NeedProc,  R.Proc  = TP; end
            R.Status = "dry run";
        else
            % a DB error stops the run, as in insertArchiveImages2DB
            try
                if NeedVisit
                    T0 = tic;
                    DB.insert(InfoVisit.TableName, dropEmptyText(TV), SchemaVisit);
                    R.Time.InsertVisit = toc(T0);
                    if Args.StampStatus
                        T0 = tic;
                        appendStatus(DataDir, Args.StatusStampVisit, Args.RemoteUser);
                        R.Time.Stamp = toc(T0);
                        Log(LogLevel.Verbose, 'stamped %s: %s', DataDir, Args.StatusStampVisit);
                    end
                end
                if NeedProc
                    T0 = tic;
                    DB.insert(InfoProc.TableName, dropEmptyText(TP), SchemaProc);
                    R.Time.InsertProc = toc(T0);
                    if Args.StampStatus
                        T0 = tic;
                        appendStatus(DataDir, Args.StatusStampProc, Args.RemoteUser);
                        R.Time.Stamp = R.Time.Stamp + toc(T0);
                        Log(LogLevel.Verbose, 'stamped %s: %s', DataDir, Args.StatusStampProc);
                    end
                end
            catch ME
                Log(LogLevel.Error, 'stopped at %s: %s%s', DataDir, strtrim(ME.message), errWhere(ME));
                logSummary(Log, Result, Count, toc(TRun));
                rethrow(ME)
            end
            R.Status = "injected";
        end
        Log(LogLevel.Info, '%s %s: %d coadd, %d epoch rows; read %.2f+%.2f s, convert %.2f+%.2f s, insert %.2f+%.2f s, stamp %.2f s', ...
            R.Status, DataDir, R.Nvisit, R.Nproc, R.Time.ReadCoadd, R.Time.ReadEpoch, R.Time.ConvertVisit, R.Time.ConvertProc, ...
            R.Time.InsertVisit, R.Time.InsertProc, R.Time.Stamp);
        Result(end+1) = R; %#ok<AGROW>
        Ndone = Ndone + 1;
    end
    logSummary(Log, Result, Count, toc(TRun));

    if ~Args.DryRun
        DB.delete;
    end
end

% internal functions

function logSummary(Log, Result, Count, Seconds)
    % Log the totals of the run
    St = string({Result.Status});
    Ok = ismember(St, ["injected", "dry run"]);
    Log(LogLevel.Info, 'end: %d visits injected, %d dry run, %d skipped (read errors), %d without .status, %d done before; %d coadd + %d epoch rows; %.1f s', ...
        sum(St == "injected"), sum(St == "dry run"), sum(startsWith(St, "read error")), Count.NoStatus, Count.Already, ...
        sum([Result(Ok).Nvisit]), sum([Result(Ok).Nproc]), Seconds);
end

function Where = errWhere(ME)
    % Function and line where an error happened, as ' (name, line N)'
    Where = '';
    if ~isempty(ME.stack)
        Where = sprintf(' (%s, line %d)', ME.stack(1).name, ME.stack(1).line);
    end
end

function Name = hostName
    % Host name of this computer
    [~, Name] = system('hostname');
    Name = strtrim(Name);
end

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
        error('insertArchiveImageHeaders2DB:schema', 'Table %s does not match the discussion: not in table: %s; not in discussion: %s', ...
              TableName, strjoin(Extra, ', '), strjoin(Absent, ', '));
    end
end

function checkComputed(Spec, Known)
    % Error if the discussion asks for a computed column this function does not compute
    Computed = Spec.Column(Spec.Kind == "computed");
    Unknown  = setdiff(Computed, Known);
    if ~isempty(Unknown)
        error('insertArchiveImageHeaders2DB:computed', 'No rule to compute column(s): %s', strjoin(Unknown, ', '));
    end
end

function H = readHeaders(DataDir, Template, HDU)
    % Read the headers of all files matching the template in a visit dir
    List = dir(fullfile(DataDir, Template));
    if isempty(List)
        H = AstroHeader.empty;
        return
    end
    H = AstroHeader(fullfile(DataDir, {List.name}), HDU);
end

function H = readEpochHeaders(DataDir, Args)
    % Read the epoch image headers of a visit: from the proc_Head MAT file if
    % there is one, else from the proc_Cat FITS files
    if isempty(dir(fullfile(DataDir, Args.ProcHeadMat)))
        H = readHeaders(DataDir, Args.ProcTemplate, Args.ProcHDU);
    else
        [~, H] = pipeline.last.load.loadEpochCatMat(DataDir, 'Read',"head", 'HeadTemplate',Args.ProcHeadMat);
        H = reshape(H.', 1, []);   % order of the FITS file names: epoch, then crop
    end
end

function T = headers2columns(H, Spec)
    % Convert an array of headers to a struct of column arrays, as selected and typed in Spec
    Nh   = numel(H);
    Ncol = height(Spec);
    T    = struct();
    for Icol = 1:Ncol
        T.(Spec.Column(Icol)) = emptyColumn(Spec.Type(Icol), Spec.Default(Icol), Nh);
    end
    IsHdr = Spec.Kind ~= "computed";
    for Ih = 1:Nh
        Keys = H(Ih).Data(:,1);
        [Found, Ind] = ismember(Spec.Keyword, string(Keys));
        for Icol = find(IsHdr & Found)'
            Val = H(Ih).Data{Ind(Icol), 2};
            Col = Spec.Column(Icol);
            if Spec.Kind(Icol) == "sec4"
                Sec = secValue(Val);
                T.(Col)(Ih) = Sec(Spec.SecPart(Icol));
            elseif Spec.Keyword(Icol) == "FIELDID"
                T.(Col){Ih} = regexprep(textValue(Val), '^(\d+)\.$', '$1');   % 1214. -> 1214
            else
                T.(Col) = setValue(T.(Col), Ih, Val, Spec.Type(Icol));
            end
        end
    end
end

function T = dropEmptyText(T)
    % Leave out the text columns that are empty in every row: the DB fills them
    % with the column default (''), and the ClickHouse mex cannot insert a column
    % of only empty char cells (it takes it for an array column)
    F = fieldnames(T);
    for I = 1:numel(F)
        if iscell(T.(F{I})) && all(cellfun(@isempty, T.(F{I})))
            T = rmfield(T, F{I});
        end
    end
end

function Col = emptyColumn(Type, Default, N)
    % Column of N default values with the MATLAB class of the ClickHouse type
    switch char(Type)
        case {'String', 'LowCardinality(String)'}
            Col = repmat({''}, N, 1);
        case 'Float64'
            Col = nan(N, 1);
        case 'Float32'
            Col = nan(N, 1, 'single');
        case {'UInt8','UInt16','UInt32','UInt64','Int8','Int16','Int32','Int64'}
            Col = zeros(N, 1, lower(char(Type)));
        case "DateTime64(3,'UTC')"
            Col = zeros(N, 1);    % POSIX seconds
        otherwise
            error('insertArchiveImageHeaders2DB:type', 'Unsupported column type %s', Type);
    end
    if Default ~= "nan" && Default ~= "''" && Default ~= "0"
        error('insertArchiveImageHeaders2DB:default', 'Unsupported default %s', Default);
    end
end

function Col = setValue(Col, I, Val, Type)
    % Convert one header value to the column type; blank/NaN keeps the default
    switch char(Type)
        case {'String', 'LowCardinality(String)'}
            Col{I} = textValue(Val);
        case {'Float64', 'Float32'}
            V = numValue(Val);
            if ~isnan(V)
                Col(I) = V;
            end
        case 'UInt64'
            if ischar(Val) || isstring(Val)
                V = sscanf(char(Val), '%lu');       % exact: never through a double
                if ~isempty(V)
                    Col(I) = V;
                end
            elseif isnumeric(Val) && ~isempty(Val) && ~isnan(Val)
                Col(I) = uint64(Val);
            end
        case {'UInt8','UInt16','UInt32','Int8','Int16','Int32','Int64'}
            V = numValue(Val);
            if ~isnan(V)
                Col(I) = round(V);                  % cast saturates at the type limits
            end
        case "DateTime64(3,'UTC')"
            if ischar(Val) || isstring(Val)
                Col(I) = isoPosix(char(Val));
            end
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

function S = textValue(Val)
    % Header value -> char (numbers without a trailing point; NaN -> '')
    if ischar(Val) || isstring(Val)
        S = strtrim(char(Val));
    elseif isnumeric(Val) && ~isempty(Val) && ~isnan(Val(1))
        if Val(1) == round(Val(1))
            S = sprintf('%d', Val(1));
        else
            S = sprintf('%.15g', Val(1));
        end
    else
        S = '';
    end
end

function Sec = secValue(Val)
    % '[xmin xmax ymin ymax]' -> int16 1x4 (zeros if missing)
    Sec = zeros(1, 4, 'int16');
    if ischar(Val) || isstring(Val)
        V = imUtil.ccdsec.ccdsecStr2num(char(Val));
    else
        V = Val;
    end
    if isnumeric(V) && numel(V) == 4 && all(isfinite(V))
        Sec = int16(V(:)');
    end
end

function P = isoPosix(Str)
    % ISO date-time text (UTC) -> POSIX seconds, 0 if it cannot be parsed
    P = 0;
    Formats = {'yyyy-MM-dd''T''HH:mm:ss.SSS', 'yyyy-MM-dd''T''HH:mm:ss'};
    for If = 1:numel(Formats)
        try
            P = posixtime(datetime(Str, 'InputFormat',Formats{If}, 'TimeZone','UTC'));
            return
        catch
        end
    end
end

function ID = matchCoaddID(HP, HC)
    % id_coadd of every epoch header: the coadd with the same CROPID whose
    % ID_PROCF..ID_PROCL range contains the epoch ID_PROC (0 if none)
    Nc = numel(HC);
    CropC = zeros(Nc,1); First = zeros(Nc,1,'uint64'); Last = zeros(Nc,1,'uint64'); IDC = zeros(Nc,1,'uint64');
    for Ic = 1:Nc
        CropC(Ic) = numValue(keyValue(HC(Ic), 'CROPID'));
        First(Ic) = idValue(keyValue(HC(Ic), 'ID_PROCF'));
        Last(Ic)  = idValue(keyValue(HC(Ic), 'ID_PROCL'));
        IDC(Ic)   = idValue(keyValue(HC(Ic), 'ID_COADD'));
    end
    Np = numel(HP);
    ID = zeros(Np, 1, 'uint64');
    for Ip = 1:Np
        Crop = numValue(keyValue(HP(Ip), 'CROPID'));
        IDP  = idValue(keyValue(HP(Ip), 'ID_PROC'));
        Ic   = find(CropC == Crop & First <= IDP & IDP <= Last & IDP > 0, 1);
        if ~isempty(Ic)
            ID(Ip) = IDC(Ic);
        end
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
            error('insertArchiveImageHeaders2DB:status', 'Rows inserted but .status not stamped in %s', DataDir);
        end
        fprintf(FID, '%s\n', Line);
        fclose(FID);
    else
        Cmd = sprintf('su - %s -c "echo ''%s'' >> %s/.status"', RemoteUser, Line, DataDir);
        [Status, Out] = system(Cmd);
        if Status ~= 0
            % stop: with MergeTree an unstamped visit would be inserted again by the next run
            error('insertArchiveImageHeaders2DB:status', 'Rows inserted but .status not stamped in %s: %s', DataDir, Out);
        end
    end
end
