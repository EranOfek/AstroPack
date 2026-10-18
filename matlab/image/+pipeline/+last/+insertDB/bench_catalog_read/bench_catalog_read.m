function Results = bench_catalog_read(Args)
% bench_catalog_read  Benchmark loading ~480 LAST proc_Cat FITS tables into memory (MATLAB side)
% Cache states (this storage is a spinning RAID, ~6 ms/file when really cold):
%   warm  - same visit, page cache hot            -> pure CPU / parse cost
%   evict - same visit, page cache dropped (posix_fadvise DONTNEED) before each run;
%           RAID controller cache may still be warm -> optimistic "cold"
%   cold  - every trial reads a FRESH visit from ColdPool never read before (ColdState file is
%           shared with bench_catalog_read.py so no visit is reused) -> real first-touch cost
% Input  : * ...,Key,Val,...
%            'Visit'     - visit dir (warm/evict states), e.g. .../proc/214135v0
%            'States'    - cellstr subset of {'warm','evict','cold'} (default {'warm','evict'})
%            'Configs'   - cellstr 'method' or 'method:N' (N = threads/workers), methods:
%                          raw          fopen+fread whole files, serial
%                          astrocatalog PRODUCTION: AstroCatalog(List,'UseMex',true), serial
%                          mexloop      io.fits.mex.read_catalog + struct2table + table2array per file
%                          matlab       readCatalogMatlab (pure MATLAB, no MEX), serial
%                          matlab_proc  readCatalogMatlab in parfor on a process pool of N workers
%                          matlab_thr   readCatalogMatlab in parfor on a thread pool of N workers
%                          batch        read_catalog_batch MEX, N threads, WILLNEED prefetch
%                          batch_nopf   same without prefetch
%            'Reps'      - repetitions (warm/evict) or rounds (cold), default 3
%            'ColdPool'  - cellstr of proc/ dirs whose *v0 visits feed the cold state
%            'ColdState' - file listing already used visits (default cold_used_visits.txt next to this file)
%            'Csv'       - output csv (appended); default bench_ml_<timestamp>.csv in pwd
%            'Verify'    - compare against io.fits.mex.read_catalog (default true)
%            'Seed'      - rng seed for cold config order (default 1)
% Output : - Results table (one row per trial)
% Example: bench_catalog_read('Visit',V,'Configs',{'astrocatalog','batch:16'},'States',{'warm'});

    arguments
        Args.Visit (1,:) char
        Args.States cell = {'warm','evict'}
        Args.Configs cell = {'raw','astrocatalog','mexloop','matlab','matlab_proc:16','matlab_thr:16','batch:1','batch:4','batch:16','batch:64','batch_nopf:16'}
        Args.Reps (1,1) double {mustBePositive, mustBeInteger} = 3
        Args.ColdPool cell = {}
        Args.ColdState (1,:) char = ''
        Args.Csv (1,:) char = ''
        Args.Verify (1,1) logical = true
        Args.Seed (1,1) double = 1
        Args.Template (1,:) char = 'LAST*proc_Cat_1.fits'
    end

    Here = fileparts(mfilename('fullpath'));
    if isempty(Args.ColdState), Args.ColdState = fullfile(Here, 'cold_used_visits.txt'); end
    if isempty(Args.Csv), Args.Csv = fullfile(pwd, sprintf('bench_ml_%s.csv', datestr(now, 'yyyymmdd_HHMMSS'))); end
    addpath(Here);

    Cfgs = parseConfigs_(Args.Configs);
    Rows = {};
    RefCache = containers.Map();

    if any(ismember(Args.States, {'warm','evict'}))
        [Files, SizeMB] = visitFiles_(Args.Visit, Args.Template);
        fprintf('visit %s: %d files %.1f MB; %d configs\n', Args.Visit, numel(Files), SizeMB, numel(Cfgs));
        for Is = 1:numel(Args.States)
            State = Args.States{Is};
            if ~ismember(State, {'warm','evict'}), continue, end
            for Ic = 1:numel(Cfgs)
                Cfg = Cfgs(Ic);
                prepare_(Cfg, Files);
                Times = zeros(1, Args.Reps);
                Ok = '-';
                for Rep = 0:Args.Reps
                    if strcmp(State, 'evict'), evictVisit_(Args.Visit, Args.Template); end
                    T = tic;
                    Res = runConfig_(Cfg, Files);
                    Sec = toc(T);
                    if Rep == 0   % untimed first run: JIT / mex load warm-up, plus verification
                        if Args.Verify, Ok = verify_(Res, Files, Args.Visit, RefCache, true); end
                        continue
                    end
                    Times(Rep) = Sec;
                end
                for Sec = Times
                    Rows(end+1, :) = record_(State, Cfg, Args.Visit, numel(Files), SizeMB, Sec, Ok); %#ok<AGROW>
                end
            end
        end
    end

    if ismember('cold', Args.States)
        if isempty(Args.ColdPool), error('bench_catalog_read:pool', 'ColdPool required for state cold'); end
        Pool = coldPool_(Args.ColdPool, Args.ColdState, Args.Template);
        Need = numel(Cfgs) * Args.Reps;
        if numel(Pool) < Need, error('bench_catalog_read:pool', 'cold pool has %d unused visits, need %d', numel(Pool), Need); end
        rng(Args.Seed);
        Pool = Pool(randperm(numel(Pool)));   % spread trials over nights / times of day
        Ip = 0;
        for Rnd = 1:Args.Reps
            Order = randperm(numel(Cfgs));
            for Ic = Order
                Cfg = Cfgs(Ic);
                Ip = Ip + 1;
                Visit = Pool{Ip};
                [Files, SizeMB] = visitFiles_(Visit, Args.Template);
                prepare_(Cfg, Files(1:2));       % create pool / load mex before the clock starts (touches 2 files only)
                fid = fopen(Args.ColdState, 'a'); fprintf(fid, '%s\n', Visit); fclose(fid);
                evictVisit_(Visit, Args.Template);
                T = tic;
                Res = runConfig_(Cfg, Files);
                Sec = toc(T);
                Ok = '-';
                if Args.Verify, Ok = verify_(Res, Files, Visit, RefCache, false); end
                Rows(end+1, :) = record_('cold', Cfg, Visit, numel(Files), SizeMB, Sec, Ok); %#ok<AGROW>
            end
        end
    end

    Results = cell2table(Rows, 'VariableNames', {'lang','state','method','threads','visit','n','sizeMB','sec','MBps','ok'});
    writetable(Results, Args.Csv, 'WriteMode', 'append');
    fprintf('csv: %s\n', Args.Csv);
end

% ------------------------------------------------------------------------------------------ configs
function Cfgs = parseConfigs_(List)
    Cfgs = struct('name', {}, 'method', {}, 'n', {});
    for I = 1:numel(List)
        Tok = strsplit(List{I}, ':');
        N = 1;
        if numel(Tok) > 1, N = str2double(Tok{2}); end
        Cfgs(I).name = List{I}; Cfgs(I).method = Tok{1}; Cfgs(I).n = N; %#ok<AGROW>
    end
end

function prepare_(Cfg, Files)
% Create pools / load code outside the timed region.
    switch Cfg.method
        case 'matlab_proc'
            ensurePool_('Processes', Cfg.n);
            runConfig_(Cfg, Files(1:min(2, numel(Files))));
        case 'matlab_thr'
            ensurePool_('Threads', Cfg.n);
            runConfig_(Cfg, Files(1:min(2, numel(Files))));
    end
end

function ensurePool_(Profile, N)
    P = gcp('nocreate');
    if ~isempty(P) && (P.NumWorkers ~= N || ~strcmp(class(P), iff_(strcmp(Profile, 'Threads'), 'parallel.ThreadPool', 'parallel.ProcessPool')))
        delete(P); P = [];
    end
    if isempty(P)
        if strcmp(Profile, 'Threads')
            parpool('Threads', N);
        else
            parpool('Processes', N);
            Here = fileparts(mfilename('fullpath'));
            pctRunOnAll(sprintf('addpath(''%s'')', Here));
        end
    end
end

function V = iff_(C, A, B)
    if C, V = A; else, V = B; end
end

% ------------------------------------------------------------------------------------------ methods
function Res = runConfig_(Cfg, Files)
    N = numel(Files);
    switch Cfg.method
        case 'raw'
            Res = cell(N, 1);
            for I = 1:N
                Fid = fopen(Files{I}, 'rb'); Res{I} = fread(Fid, inf, '*uint8'); fclose(Fid);
            end
        case 'astrocatalog'
            AC = AstroCatalog(Files, 'UseMex', true);
            Res = {AC.Catalog}';
        case 'mexloop'
            Res = cell(N, 1);
            for I = 1:N
                S = io.fits.mex.read_catalog(Files{I}, 0);
                Res{I} = table2array(struct2table(S));
            end
        case 'matlab'
            Res = cell(N, 1);
            for I = 1:N, Res{I} = readCatalogMatlab(Files{I}); end
        case {'matlab_proc', 'matlab_thr'}
            Res = cell(N, 1);
            parfor I = 1:N
                Res{I} = readCatalogMatlab(Files{I});
            end
        case 'batch'
            Res = read_catalog_batch(Files, Cfg.n, 1);
        case 'batch_nopf'
            Res = read_catalog_batch(Files, Cfg.n, 0);
        otherwise
            error('bench_catalog_read:method', 'unknown method %s', Cfg.method);
    end
end

function Ok = verify_(Res, Files, Visit, RefCache, Keep)
    if isa(Res{1}, 'uint8')
        Ok = '-'; return
    end
    if ~isKey(RefCache, Visit)
        Ref = cell(numel(Files), 1);
        for I = 1:numel(Files)
            S = io.fits.mex.read_catalog(Files{I}, 0);
            Ref{I} = table2array(struct2table(S));
        end
        RefCache(Visit) = Ref; %#ok<NASGU>
    end
    Ref = RefCache(Visit);
    if isequaln(Res, Ref), Ok = 'OK'; else, Ok = 'MISMATCH'; end
    if ~Keep, RefCache.remove(Visit); end   % 85 MB per visit: keep only for the reused warm/evict visit
end

% ------------------------------------------------------------------------------------------ files / cache
function [Files, SizeMB] = visitFiles_(Visit, Template)
    L = dir(fullfile(Visit, Template));
    Files = fullfile({L.folder}, {L.name})';
    SizeMB = sum([L.bytes]) / 2^20;
end

function evictVisit_(Visit, Template)
    Py = fullfile(fileparts(mfilename('fullpath')), 'evict_cache.py');
    [St, Out] = system(sprintf('python3 -I "%s" "%s" "%s"', Py, Visit, Template));
    if St ~= 0, error('bench_catalog_read:evict', 'evict failed: %s', Out); end
end

function Pool = coldPool_(PoolDirs, StateFile, Template)
    Used = {};
    if isfile(StateFile), Used = strsplit(strtrim(fileread(StateFile)), newline); end
    Pool = {};
    for I = 1:numel(PoolDirs)
        D = dir(PoolDirs{I});
        D = D([D.isdir] & endsWith({D.name}, 'v0'));
        [~, Ord] = sort({D.name}); D = D(Ord);
        for K = 1:numel(D)
            V = fullfile(D(K).folder, D(K).name);
            if ~ismember(V, Used) && numel(dir(fullfile(V, Template))) == 480
                Pool{end+1} = V; %#ok<AGROW>
            end
        end
    end
end

function Row = record_(State, Cfg, Visit, N, SizeMB, Sec, Ok)
    Mbps = SizeMB / Sec;
    Row = {'matlab', State, Cfg.method, Cfg.n, Visit, N, round(SizeMB, 2), round(Sec, 4), round(Mbps, 1), Ok};
    fprintf('%-5s %-13s thr=%3d  %7.3fs  %8.1f MB/s  ok=%s  %s\n', State, Cfg.method, Cfg.n, Sec, Mbps, Ok, Visit);
end
