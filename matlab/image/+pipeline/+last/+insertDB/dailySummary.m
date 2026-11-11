function [Summary, HtmlFile, MdFile] = dailySummary(Args)
    % Summary of one day of ingestion into the image_visit / image_proc tables (issue #1398)
    %     Counts and histograms of the rows added to the DB in a time window
    %     (by ingestion_time_jd), by default the 24 h before the call. All
    %     numbers come from DB queries, so the summary shows what is in the
    %     tables and can be rebuilt for any past window.
    %     - Counts: visits, coadd images (image_visit rows), epoch images
    %       (image_proc rows), raw images (distinct ID_RAW of the epoch images,
    %       i.e. raw images that gave an epoch image) and, once the tables exist,
    %       coadd and epoch sources; in total and per observing night and telescope.
    %       The observing night is the UTC date of DATE-OBS minus 12 h.
    %     - Histograms of LIMMAG, FWHM, PT_ARMS and AST_ARMS of the coadd and the
    %       epoch images (the bins are counted in the DB), with the 5, 50 and
    %       95% quantiles (approximate, ClickHouse quantile).
    %     - Medians of the same metrics per telescope; a median further than
    %       'OutlierZ' robust sigma (1.4826*MAD across telescopes) from the median
    %       of all telescopes is flagged.
    %     Writes a self-contained HTML file (figures embedded as PNG) and a
    %     Markdown file with the counts and the flagged telescopes (for a
    %     GitHub comment), and one line to the ingestion log.
    % Input  : * ...,key,val,...
    %            'Start'     - Start of the window: JD, or a datetime (UTC).
    %                          Default is [] (End - 1 day).
    %            'End'       - End of the window: JD, or a datetime (UTC).
    %                          Default is [] (now).
    %            'Tables'    - Coadd and epoch image tables. Default is ["image_visit", "image_proc"].
    %            'SrcTables' - Coadd and epoch source tables, counted by ingestion_time_jd;
    %                          "" = not counted (n/a). Default is ["", ""] (not created yet).
    %            'Metrics'   - Columns shown as histograms and per-telescope medians.
    %                          Default is ["limmag", "fwhm", "pt_arms", "ast_arms"].
    %            'Edges'     - Bin edges of the histograms, a struct with one field per metric;
    %                          values below the first edge or above the last are counted
    %                          in the end bins. Default is limmag 14:0.1:22, fwhm 1:0.2:20,
    %                          pt_arms logspace(-4,0,81), ast_arms logspace(-2.5,0.5,61).
    %            'LogX'      - Metrics with a log x axis. Default is ["pt_arms", "ast_arms"].
    %            'OutlierZ'  - Robust sigma of a flagged telescope median. Default is 3.
    %            'MaxRows'   - Maximum number of night/telescope rows in the HTML table
    %                          and of night rows in the Markdown table. Default is 2000 and 40.
    %            'OutDir'    - Folder of the output files, written as
    %                          <OutDir>/<yyyy>/ingest_summary_<yyyy-mm-dd>.html and .md, the
    %                          date of End (UTC). '' = do not write files.
    %                          Default is '~/matlab/log/last/insertDB/summary'.
    %            'Db*'       - Database parameters. DbName default is 'last1'.
    %            'PassKey'   - Passwords.yml entry of the DB password. Default is 'last'.
    %            'Logger'    - A MsgLogger (pipeline.last.insertDB.ingestLogger). Default is [] (a new one).
    % Output : - A struct with the window (Start, End as JD), Totals (one-row table),
    %            ByNight (table: night, tel, visits, coadd, epoch, raw), Hist
    %            (per table and metric: Edges, Counts, Nnan, Q05, Q50, Q95) and
    %            PerTel (per table: a table of telescope medians and Flag columns).
    %          - The HTML file name ('' if not written).
    %          - The Markdown file name ('' if not written).
    % Author : A.M. Krassilchtchikov (2026 Nov)
    % Example: S = pipeline.last.insertDB.dailySummary;
    %          S = pipeline.last.insertDB.dailySummary('Start',datetime(2026,11,10), 'End',datetime(2026,11,11), 'OutDir','');

    arguments
        Args.Start             = [];
        Args.End               = [];
        Args.Tables            = ["image_visit", "image_proc"];
        Args.SrcTables         = ["", ""];
        Args.Metrics           = ["limmag", "fwhm", "pt_arms", "ast_arms"];
        Args.Edges             = struct('limmag',14:0.1:22, 'fwhm',1:0.2:20, ...
                                        'pt_arms',logspace(-4,0,81), 'ast_arms',logspace(-2.5,0.5,61));
        Args.LogX              = ["pt_arms", "ast_arms"];
        Args.OutlierZ          = 3;
        Args.MaxRows           = 2000;
        Args.MaxMdRows         = 40;
        Args.OutDir            = '~/matlab/log/last/insertDB/summary';

        Args.DbHost            = 'euclid';
        Args.DbName            = 'last1';
        Args.DbUser            = 'default';
        Args.DbPort            = 9000;
        Args.PassKey           = 'last';
        Args.AstroDBPassFile   = '~/.astropack/Passwords.yml';
        Args.ConnectorOpts     = struct('compression', db.mex.Compression.ZSTD);
        Args.Logger            = [];
    end

    if isempty(Args.Logger)
        Args.Logger = pipeline.last.insertDB.ingestLogger;
    end
    Log = @(Level, Fmt, varargin) pipeline.last.insertDB.ingestLog(Args.Logger, Level, ['dailySummary ' Fmt], varargin{:});

    % the window
    JDnow = posixtime(datetime('now', 'TimeZone','UTC'))./86400 + 2440587.5;
    End   = toJD(Args.End, JDnow);
    Start = toJD(Args.Start, End - 1);
    Where = sprintf('ingestion_time_jd >= %.6f AND ingestion_time_jd < %.6f', Start, End);
    Night = 'toString(toDate(date_obs - INTERVAL 12 HOUR))';
    Tel   = ['concat(leftPad(toString(nodenumb),2,''0''), ''.'', leftPad(toString(mountnum),2,''0''), ''.'', ' ...
             'leftPad(toString(camnum),2,''0''))'];
    TabV  = sprintf('%s.%s', Args.DbName, Args.Tables(1));
    TabP  = sprintf('%s.%s', Args.DbName, Args.Tables(2));

    Configuration.getSingleton().loadFile(Args.AstroDBPassFile); % tell the PM where to look for passwords
    PM = PasswordsManager;
    DB = db.mex.ClickHouseClient(Args.DbHost, Args.DbPort, Args.DbUser, PM.search(Args.PassKey).Pass, Args.ConnectorOpts);

    Summary = struct('Start',Start, 'End',End);

    % counts per night and telescope
    V = query(DB, sprintf(['SELECT %s AS night, %s AS tel, uniqExact(date_obs) AS visits, count() AS coadd ' ...
                           'FROM %s WHERE %s GROUP BY night, tel'], Night, Tel, TabV, Where), ...
              ["night", "tel", "visits", "coadd"]);
    P = query(DB, sprintf(['SELECT %s AS night, %s AS tel, count() AS epoch, uniqExact(id_raw) AS raw ' ...
                           'FROM %s WHERE %s GROUP BY night, tel'], Night, Tel, TabP, Where), ...
              ["night", "tel", "epoch", "raw"]);
    B = outerjoin(V, P, 'Keys',{'night','tel'}, 'MergeKeys',true);
    for C = ["visits", "coadd", "epoch", "raw"]
        B.(C)(isnan(B.(C))) = 0;
    end
    B = sortrows(B, {'night','tel'}, {'descend','ascend'});
    Summary.ByNight = B;

    Src = nan(1, 2);
    for I = 1:2
        if strlength(Args.SrcTables(I)) > 0
            S = query(DB, sprintf('SELECT count() AS n FROM %s.%s WHERE %s', Args.DbName, Args.SrcTables(I), Where), "n");
            Src(I) = S.n;
        end
    end
    Summary.Totals = table(numel(unique(B.night)), numel(unique(B.tel)), sum(B.visits), sum(B.coadd), sum(B.epoch), sum(B.raw), Src(1), Src(2), ...
        'VariableNames',{'nights','telescopes','visits','coadd','epoch','raw','coadd_src','epoch_src'});

    % histograms and per-telescope medians
    Name = ["coadd", "epoch"];
    Tab  = [string(TabV), string(TabP)];
    for It = 1:2
        for M = Args.Metrics
            E = Args.Edges.(M);
            H = query(DB, sprintf(['SELECT arraySum(arrayMap(e -> %s >= e, [%s])) AS b, count() AS n ' ...
                                   'FROM %s WHERE %s AND isFinite(%s) GROUP BY b'], M, strjoin(compose('%.10g', E), ','), Tab(It), Where, M), ...
                      ["b", "n"]);
            Q = query(DB, sprintf(['SELECT countIf(NOT isFinite(%1$s)) AS nnan, quantileIf(0.05)(%1$s, isFinite(%1$s)) AS q05, ' ...
                                   'quantileIf(0.5)(%1$s, isFinite(%1$s)) AS q50, quantileIf(0.95)(%1$s, isFinite(%1$s)) AS q95 ' ...
                                   'FROM %2$s WHERE %3$s'], M, Tab(It), Where), ["nnan", "q05", "q50", "q95"]);
            % b = 0: below the first edge, numel(E): at or above the last edge
            Counts = zeros(1, numel(E) + 1);
            Counts(H.b + 1) = H.n;
            Hist = struct('Edges',E, 'Counts',Counts, 'Nnan',0, 'Q05',NaN, 'Q50',NaN, 'Q95',NaN);
            if height(Q) > 0
                Hist.Nnan = Q.nnan;
                if sum(Counts) > 0
                    Hist.Q05 = Q.q05; Hist.Q50 = Q.q50; Hist.Q95 = Q.q95;
                end
            end
            Summary.Hist.(Name(It)).(M) = Hist;
        end
        Med = strjoin(compose('medianIf(%s, isFinite(%s)) AS %s', Args.Metrics(:), Args.Metrics(:), Args.Metrics(:)), ', ');
        T = query(DB, sprintf('SELECT %s AS tel, count() AS n, %s FROM %s WHERE %s GROUP BY tel ORDER BY tel', Tel, Med, Tab(It), Where), ...
                  ["tel", "n", Args.Metrics]);
        for M = Args.Metrics
            X  = T.(M);
            Mx = median(X, 'omitnan');
            Sx = 1.4826 * median(abs(X - Mx), 'omitnan');
            T.("flag_" + M) = Sx > 0 & abs(X - Mx) > Args.OutlierZ * Sx;
        end
        Summary.PerTel.(Name(It)) = T;
    end

    Tot = Summary.Totals;
    Log(LogLevel.Info, 'ingestion JD %.4f-%.4f in %s: %d visits, %d coadd, %d epoch, %d raw images; %d nights, %d telescopes; %d flagged telescope medians', ...
        Start, End, Args.DbName, Tot.visits, Tot.coadd, Tot.epoch, Tot.raw, Tot.nights, Tot.telescopes, ...
        nnz(table2array(Summary.PerTel.coadd(:, startsWith(Summary.PerTel.coadd.Properties.VariableNames, 'flag_')))) + ...
        nnz(table2array(Summary.PerTel.epoch(:, startsWith(Summary.PerTel.epoch.Properties.VariableNames, 'flag_')))));

    % output files
    HtmlFile = '';
    MdFile   = '';
    if ~isempty(Args.OutDir)
        OutDir = char(Args.OutDir);
        if startsWith(OutDir, '~')
            OutDir = [getenv('HOME'), OutDir(2:end)];
        end
        Day    = datetime((End - 2440587.5)*86400, 'ConvertFrom','posixtime', 'TimeZone','UTC');
        OutDir = fullfile(OutDir, char(string(Day, 'yyyy')));
        if ~isfolder(OutDir)
            mkdir(OutDir);
        end
        Base     = fullfile(OutDir, ['ingest_summary_' char(string(Day, 'yyyy-MM-dd'))]);
        HtmlFile = [Base '.html'];
        MdFile   = [Base '.md'];
        writeHtml(HtmlFile, Summary, Args);
        writeMd(MdFile, Summary, Args, HtmlFile);
        Log(LogLevel.Info, 'wrote %s, %s', HtmlFile, MdFile);
    end
end

% ----------------------------------------------------------------------
function JD = toJD(Val, Default)
    % JD from a JD number or a datetime (UTC if no time zone); Default if empty
    if isempty(Val)
        JD = Default;
    elseif isdatetime(Val)
        if isempty(Val.TimeZone)
            Val.TimeZone = 'UTC';
        end
        JD = posixtime(Val)./86400 + 2440587.5;
    else
        JD = Val;
    end
end

function T = query(DB, Sql, Cols)
    % Run a query; an empty result becomes an empty table with the given columns
    T = DB.query(Sql);
    if ~istable(T) || height(T) == 0
        T = table('Size',[0 numel(Cols)], 'VariableTypes',repmat("double", 1, numel(Cols)), 'VariableNames',Cols);
        Text = ismember(Cols, ["night", "tel"]);
        for C = Cols(Text)
            T.(C) = strings(0, 1);
        end
    end
end

function Str = jdStr(JD)
    % JD as 'yyyy-mm-dd HH:MM' UTC
    Str = char(string(datetime((JD - 2440587.5)*86400, 'ConvertFrom','posixtime', 'TimeZone','UTC'), 'yyyy-MM-dd HH:mm'));
end

function Str = num(X)
    % Count or value as text: n/a for NaN, integers with thousands separators
    if isnan(X)
        Str = 'n/a';
    elseif X == round(X)
        Str = char(regexprep(sprintf('%d', X), '(\d)(?=(\d{3})+$)', '$1,'));
    else
        Str = sprintf('%.3g', X);
    end
end

function writeHtml(File, S, Args)
    % Self-contained HTML: counts, histograms (embedded PNG), telescope medians, night/telescope table
    Fid = fopen(File, 'w');
    if Fid < 0
        error('dailySummary:file', 'Cannot write %s', File);
    end
    Cleanup = onCleanup(@() fclose(Fid));
    Title = sprintf('LAST DB ingestion %s to %s UTC', jdStr(S.Start), jdStr(S.End));
    fprintf(Fid, ['<!doctype html><html><head><meta charset="utf-8"><title>%s</title><style>' ...
                  'body{font-family:sans-serif;margin:16px} table{border-collapse:collapse;margin:8px 0} ' ...
                  'td,th{border:1px solid #bbb;padding:2px 6px;text-align:right} th{background:#eee} ' ...
                  '.flag{background:#f8c8c8} img{max-width:100%%}</style></head><body>\n'], Title);
    fprintf(Fid, '<h2>%s</h2>\n<p>Database %s; rows by ingestion_time_jd %.5f to %.5f.</p>\n', Title, Args.DbName, S.Start, S.End);

    T = S.Totals;
    fprintf(Fid, '<h3>Counts</h3>\n<table><tr><th>nights</th><th>telescopes</th><th>visits</th><th>coadd images</th><th>epoch images</th><th>raw images</th><th>coadd sources</th><th>epoch sources</th></tr>\n');
    fprintf(Fid, '<tr>%s</tr></table>\n', strjoin(cellfun(@(x) ['<td>' num(x) '</td>'], num2cell(table2array(T)), 'UniformOutput',false), ''));
    fprintf(Fid, '<p>Raw images = distinct ID_RAW of the epoch images. n/a = source table not counted.</p>\n');

    if T.coadd + T.epoch > 0
        fprintf(Fid, '<h3>Histograms</h3>\n<img src="data:image/png;base64,%s">\n', histFigure(S, Args));
    end

    fprintf(Fid, '<h3>Medians per telescope</h3>\n<p>Flagged (red): more than %g robust sigma from the median of all telescopes.</p>\n<div style="display:flex;gap:24px;flex-wrap:wrap">\n', Args.OutlierZ);
    for Name = ["coadd", "epoch"]
        P = S.PerTel.(Name);
        fprintf(Fid, '<div><b>%s images</b><table><tr><th>telescope</th><th>images</th>%s</tr>\n', Name, ...
            strjoin(compose('<th>%s</th>', upper(Args.Metrics)), ''));
        for I = 1:height(P)
            Cells = '';
            for M = Args.Metrics
                Cls = '';
                if P.("flag_" + M)(I), Cls = ' class="flag"'; end
                Cells = [Cells sprintf('<td%s>%.3g</td>', Cls, P.(M)(I))]; %#ok<AGROW>
            end
            fprintf(Fid, '<tr><td>%s</td><td>%s</td>%s</tr>\n', P.tel(I), num(P.n(I)), Cells);
        end
        fprintf(Fid, '</table></div>\n');
    end
    fprintf(Fid, '</div>\n');

    B = S.ByNight;
    fprintf(Fid, '<h3>Per night and telescope</h3>\n');
    if height(B) > Args.MaxRows
        fprintf(Fid, '<p>First %d of %d rows (newest nights first).</p>\n', Args.MaxRows, height(B));
    end
    fprintf(Fid, '<table><tr><th>night</th><th>telescope</th><th>visits</th><th>coadd</th><th>epoch</th><th>raw</th></tr>\n');
    for I = 1:min(height(B), Args.MaxRows)
        fprintf(Fid, '<tr><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td></tr>\n', ...
            B.night(I), B.tel(I), num(B.visits(I)), num(B.coadd(I)), num(B.epoch(I)), num(B.raw(I)));
    end
    fprintf(Fid, '</table>\n<p>Made by pipeline.last.insertDB.dailySummary on %s at %s UTC.</p></body></html>\n', ...
        hostName, char(string(datetime('now', 'TimeZone','UTC'), 'yyyy-MM-dd HH:mm')));
end

function B64 = histFigure(S, Args)
    % One figure: a row per metric, coadd and epoch side by side; returns the PNG as base64
    Nm  = numel(Args.Metrics);
    Fig = figure('Visible','off', 'Position',[0 0 1100 260*Nm]);
    Tl  = tiledlayout(Fig, Nm, 2, 'TileSpacing','compact', 'Padding','compact');
    for Im = 1:Nm
        M = Args.Metrics(Im);
        for Name = ["coadd", "epoch"]
            H  = S.Hist.(Name).(M);
            Ax = nexttile(Tl);
            % the end bins hold the values outside the edges: show them in the title
            Inner = H.Counts(2:end-1);
            if any(Inner)
                histogram(Ax, 'BinEdges',H.Edges, 'BinCounts',Inner, 'EdgeColor','none');
            end
            if ismember(M, Args.LogX)
                set(Ax, 'XScale','log', 'XTick',10.^(floor(log10(H.Edges(1))):ceil(log10(H.Edges(end)))));
            end
            set(Ax, 'YScale','log');
            xlim(Ax, H.Edges([1 end]));
            if any(Inner)
                ylim(Ax, [0.8 2*max(Inner)]);
                set(Ax, 'YTick',10.^(0:ceil(log10(2*max(Inner)))));
            end
            if ~isnan(H.Q50)
                xline(Ax, [H.Q05 H.Q50 H.Q95], '-', {'5%', '50%', '95%'}, 'Color',[0.8 0.2 0.2]);
            end
            title(Ax, sprintf('%s %s: N %s, median %.3g; %s below, %s above, %s NaN', Name, upper(M), num(sum(H.Counts)), H.Q50, ...
                num(H.Counts(1)), num(H.Counts(end)), num(H.Nnan)), 'FontWeight','normal', 'FontSize',9, 'Interpreter','none');
        end
    end
    Tmp = [tempname '.png'];
    exportgraphics(Fig, Tmp, 'Resolution',90);
    close(Fig);
    Fid  = fopen(Tmp, 'rb');
    Byte = fread(Fid, Inf, '*uint8');
    fclose(Fid);
    delete(Tmp);
    B64 = char(matlab.net.base64encode(Byte));
end

function writeMd(File, S, Args, HtmlFile)
    % Markdown for a GitHub comment: counts, nights, flagged telescopes, quantiles
    Fid = fopen(File, 'w');
    if Fid < 0
        error('dailySummary:file', 'Cannot write %s', File);
    end
    Cleanup = onCleanup(@() fclose(Fid));
    T = S.Totals;
    fprintf(Fid, '**DB ingestion %s to %s UTC** (`%s`)\n\n', jdStr(S.Start), jdStr(S.End), Args.DbName);
    fprintf(Fid, '| nights | telescopes | visits | coadd images | epoch images | raw images | coadd sources | epoch sources |\n|---|---|---|---|---|---|---|---|\n');
    fprintf(Fid, '|%s|\n\n', strjoin(cellfun(@num, num2cell(table2array(T)), 'UniformOutput',false), '|'));

    B = S.ByNight;
    if height(B) > 0
        G = groupsummary(B, 'night', 'sum', {'visits','coadd','epoch','raw'});
        Ntel = groupsummary(B, 'night');
        G = sortrows(G, 'night', 'descend');
        Ntel = sortrows(Ntel, 'night', 'descend');
        fprintf(Fid, '| night | telescopes | visits | coadd | epoch | raw |\n|---|---|---|---|---|---|\n');
        for I = 1:min(height(G), Args.MaxMdRows)
            fprintf(Fid, '| %s | %d | %s | %s | %s | %s |\n', G.night(I), Ntel.GroupCount(I), num(G.sum_visits(I)), ...
                num(G.sum_coadd(I)), num(G.sum_epoch(I)), num(G.sum_raw(I)));
        end
        if height(G) > Args.MaxMdRows
            fprintf(Fid, '\n%d more nights in the HTML file.\n', height(G) - Args.MaxMdRows);
        end
        fprintf(Fid, '\n');

        fprintf(Fid, '| metric | coadd 5–50–95%% | epoch 5–50–95%% |\n|---|---|---|\n');
        for M = Args.Metrics
            C = S.Hist.coadd.(M);
            E = S.Hist.epoch.(M);
            fprintf(Fid, '| %s | %.3g – %.3g – %.3g | %.3g – %.3g – %.3g |\n', upper(M), C.Q05, C.Q50, C.Q95, E.Q05, E.Q50, E.Q95);
        end
        fprintf(Fid, '\n');

        Lines = strings(0, 1);
        for Name = ["coadd", "epoch"]
            P = S.PerTel.(Name);
            for M = Args.Metrics
                F = find(P.("flag_" + M));
                for I = F'
                    Lines(end+1) = sprintf('%s %s %s median %.3g (all telescopes %.3g)', P.tel(I), Name, upper(M), ...
                        P.(M)(I), median(P.(M), 'omitnan')); %#ok<AGROW>
                end
            end
        end
        if isempty(Lines)
            fprintf(Fid, 'No telescope median further than %g robust sigma from the others.\n\n', Args.OutlierZ);
        else
            fprintf(Fid, 'Telescope medians further than %g robust sigma from the others:\n', Args.OutlierZ);
            fprintf(Fid, '- %s\n', Lines);
            fprintf(Fid, '\n');
        end
    end
    fprintf(Fid, 'Histograms and the full tables: `%s` on %s.\n', HtmlFile, hostName);
end

function Name = hostName
    % Host name of this machine
    [~, Name] = system('hostname -f');
    Name = strtrim(Name);
end
