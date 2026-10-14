function Results = bench_catalog_read(Args)
% bench_catalog_read  Thread-sweep benchmark for proc Cat FITS read I/O
% Input  : * ...,Key,Val,...
%            'Parent'     - proc/ directory containing *v0 visit subdirs (required)
%            'Threads'    - Worker counts to try, e.g. [1 2 4 8] (default 1)
%            'Mode'       - 'raw' | 'astro' | 'header' | 'all' (default 'all')
%            'MaxVisits'  - Max visit folders to benchmark (default 1)
%            'Template'   - Cat file template inside each visit (default LAST*proc_Cat_1.fits*)
%            'LogFile'    - Append log path; default bench_catalog_read_<timestamp>.log in pwd
% Output : - Results struct array (one row per visit x threads x mode)
% Example: bench_catalog_read('Parent','/bigdata2/.../proc','Threads',[1 4 8],'Mode','all');

    arguments
        Args.Parent (1,:) char
        Args.Threads (1,:) double {mustBePositive, mustBeInteger} = 1
        Args.Mode (1,:) char {mustBeMember(Args.Mode, {'raw','astro','header','all'})} = 'all'
        Args.MaxVisits (1,1) double {mustBePositive, mustBeInteger} = 1
        Args.Template (1,:) char = 'LAST*proc_Cat_1.fits*'
        Args.LogFile (1,:) char = ''
    end

    Modes = resolveModes_(Args.Mode);
    Visits = discoverVisits_(Args.Parent, Args.MaxVisits);
    if isempty(Visits)
        error('bench_catalog_read:NoVisits', 'No visit folders under %s', Args.Parent);
    end

    if isempty(Args.LogFile)
        Args.LogFile = fullfile(pwd, sprintf('bench_catalog_read_%s.log', datestr(now, 'yyyymmdd_HHMMSS')));
    end
    Fid = fopen(Args.LogFile, 'a');
    if Fid < 0
        error('bench_catalog_read:LogOpen', 'Cannot open log: %s', Args.LogFile);
    end
    LogCloser = onCleanup(@() fclose(Fid)); %#ok<NASGU>

    logLine_(Fid, sprintf('START Parent=%s Modes=%s Threads=%s MaxVisits=%d Template=%s', ...
        Args.Parent, strjoin(Modes, ','), mat2str(Args.Threads), Args.MaxVisits, Args.Template));
    logLine_(Fid, sprintf('HOST %s MATLAB %s', getenv('HOSTNAME'), version));

    Results = struct([]);
    Ires = 0;
    for Iv = 1:numel(Visits)
        VisitDir = Visits{Iv};
        List = io.files.filelist(fullfile(VisitDir, Args.Template), 'AddPath', true);
        if isempty(List)
            logLine_(Fid, sprintf('SKIP visit=%s reason=noFiles', VisitDir));
            continue
        end
        [Nfiles, SizeMB] = fileStats_(List);
        logLine_(Fid, sprintf('visit=%s nFiles=%d sizeMB=%.2f', VisitDir, Nfiles, SizeMB));

        for Im = 1:numel(Modes)
            Mode = Modes{Im};
            for It = 1:numel(Args.Threads)
                Nthreads = Args.Threads(It);
                Sec = benchOneMode_(Mode, List, Nthreads, VisitDir, Args.Template);
                Mbps = mbPerSec_(SizeMB, Sec);
                Ires = Ires + 1;
                Results(Ires).VisitDir = VisitDir; %#ok<AGROW>
                Results(Ires).Mode = Mode;
                Results(Ires).Threads = Nthreads;
                Results(Ires).Nfiles = Nfiles;
                Results(Ires).SizeMB = SizeMB;
                Results(Ires).Seconds = Sec;
                Results(Ires).MBps = Mbps;
                logLine_(Fid, sprintf('RESULT visit=%s mode=%s threads=%d n=%d sizeMB=%.2f sec=%.3f MBps=%.2f', ...
                    VisitDir, Mode, Nthreads, Nfiles, SizeMB, Sec, Mbps));
                fprintf('bench_catalog_read %s threads=%d n=%d sizeMB=%.2f sec=%.3f MBps=%.2f %s\n', ...
                    Mode, Nthreads, Nfiles, SizeMB, Sec, Mbps, VisitDir);
            end
        end
    end

    printSummary_(Fid, Results);
    logLine_(Fid, 'END');
end

function Modes = resolveModes_(Mode)
    if strcmp(Mode, 'all')
        Modes = {'raw', 'astro', 'header'};
    else
        Modes = {Mode};
    end
end

function Visits = discoverVisits_(Parent, MaxVisits)
    D = dir(Parent);
    D = D([D.isdir]);
    D = D(~ismember({D.name}, {'.', '..'}));
    D = D(contains({D.name}, 'v0'));
    D = D(~contains({D.folder}, 're'));
    [~, Ord] = sort({D.name});
    D = D(Ord);
    N = min(numel(D), MaxVisits);
    Visits = cell(N, 1);
    for I = 1:N
        Visits{I} = fullfile(D(I).folder, D(I).name);
    end
end

function [Nfiles, SizeMB] = fileStats_(List)
    Nfiles = numel(List);
    TotalBytes = 0;
    for I = 1:Nfiles
        Info = dir(List{I});
        if ~isempty(Info)
            TotalBytes = TotalBytes + Info.bytes;
        end
    end
    SizeMB = TotalBytes / (1024^2);
end

function Sec = benchOneMode_(Mode, List, Nthreads, VisitDir, Template)
    switch Mode
        case 'raw'
            Sec = benchRaw_(List, Nthreads);
        case 'astro'
            Sec = benchAstro_(List, Nthreads, VisitDir, Template);
        case 'header'
            Sec = benchHeader_(List, Nthreads, VisitDir, Template);
        otherwise
            error('bench_catalog_read:BadMode', '%s', Mode);
    end
end

function Sec = benchRaw_(List, Nthreads)
    N = numel(List);
    Tic = tic;
    if Nthreads <= 1
        for I = 1:N
            readFileBytes_(List{I});
        end
    else
        ensureParpool_(Nthreads);
        parfor I = 1:N
            readFileBytes_(List{I});
        end
    end
    Sec = toc(Tic);
end

function Sec = benchAstro_(List, Nthreads, VisitDir, Template)
    Tic = tic;
    if Nthreads <= 1
        if ~isempty(List)
            AstroCatalog(List, 'UseMex', true);
        else
            oldDir = pwd;
            c = onCleanup(@() cd(oldDir)); %#ok<NASGU>
            cd(VisitDir);
            AstroCatalog(Template, 'UseMex', true);
        end
    else
        ensureParpool_(Nthreads);
        N = numel(List);
        DataCell = cell(N, 1);
        parfor I = 1:N
            DataCell{I} = readCatalogArray_(List{I});
        end
        AstroCatalog(DataCell);
    end
    Sec = toc(Tic);
end

function Sec = benchHeader_(List, Nthreads, VisitDir, Template)
    Tic = tic;
    if Nthreads <= 1
        if ~isempty(List)
            AstroHeader(List, 3, 'UseMex', true);
        else
            oldDir = pwd;
            c = onCleanup(@() cd(oldDir)); %#ok<NASGU>
            cd(VisitDir);
            AstroHeader(Template, 3, 'UseMex', true);
        end
    else
        ensureParpool_(Nthreads);
        N = numel(List);
        parfor I = 1:N
            io.fits.mex.read_header(List{I}, 3);
        end
    end
    Sec = toc(Tic);
end

function Arr = readCatalogArray_(FileName)
    Data = ImageIO.read1(FileName, 'IsTable', true, 'UseMex', true);
    if istable(Data)
        Arr = table2array(Data);
    else
        Arr = Data;
    end
end

function readFileBytes_(FileName)
    Fid = fopen(FileName, 'rb');
    if Fid < 0
        error('bench_catalog_read:CannotOpen', '%s', FileName);
    end
    Cleaner = onCleanup(@() fclose(Fid)); %#ok<NASGU>
    fread(Fid, inf, '*uint8');
end

function ensureParpool_(Nworkers)
    Pool = gcp('nocreate');
    if isempty(Pool)
        parpool('local', Nworkers);
        return
    end
    if Pool.NumWorkers ~= Nworkers
        delete(Pool);
        parpool('local', Nworkers);
    end
end

function Mbps = mbPerSec_(SizeMB, Sec)
    if Sec > 0 && SizeMB > 0
        Mbps = SizeMB / Sec;
    else
        Mbps = NaN;
    end
end

function logLine_(Fid, Msg)
    Line = sprintf('%s %s\n', datestr(now, 'yyyy-mm-dd HH:MM:SS'), Msg);
    fprintf(Fid, '%s', Line);
end

function printSummary_(Fid, Results)
    if isempty(Results)
        logLine_(Fid, 'SUMMARY empty');
        return
    end
    Modes = unique({Results.Mode}, 'stable');
    Threads = unique([Results.Threads], 'stable');
    for Im = 1:numel(Modes)
        Mode = Modes{Im};
        for It = 1:numel(Threads)
            Nth = Threads(It);
            Mask = strcmp({Results.Mode}, Mode) & [Results.Threads] == Nth;
            Sub = Results(Mask);
            if isempty(Sub)
                continue
            end
            Mbps = [Sub.MBps];
            logLine_(Fid, sprintf('SUMMARY mode=%s threads=%d nRuns=%d MBps_mean=%.2f MBps_median=%.2f', ...
                Mode, Nth, numel(Sub), mean(Mbps, 'omitnan'), median(Mbps, 'omitnan')));
        end
    end
end
