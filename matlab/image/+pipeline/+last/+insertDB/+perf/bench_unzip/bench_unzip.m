function Results = bench_unzip(Args)
% bench_unzip  Benchmark MATLAB unzip() extract + rm into tmpfs (/dev/shm)
% Input  : * ...,Key,Val,...
%            'ZipDir'     - Directory with *.zip (required unless ZipFile set)
%            'ZipFile'    - Single zip path (optional)
%            'DestRoot'   - Extract root (default '/dev/shm')
%            'Niter'      - Timed iterations per zip (default 1)
%            'Warmup'     - Warmup extract+delete cycles per zip (default 0)
%            'LogFile'    - Append log; default bench_unzip_<timestamp>.log in pwd
% Output : - Results struct array (one row per zip x iter)
% Example: bench_unzip('ZipDir','/bigdata2/.../proc/_zip','DestRoot','/dev/shm');

    arguments
        Args.ZipDir (1,:) char = ''
        Args.ZipFile (1,:) char = ''
        Args.DestRoot (1,:) char = '/dev/shm'
        Args.Niter (1,1) double {mustBePositive, mustBeInteger} = 1
        Args.Warmup (1,1) double {mustBeNonnegative, mustBeInteger} = 0
        Args.LogFile (1,:) char = ''
    end

    Zips = discoverZips_(Args.ZipDir, Args.ZipFile);
    if isempty(Zips)
        error('bench_unzip:NoZips', 'No zip files found');
    end
    if ~isfolder(Args.DestRoot)
        error('bench_unzip:DestRoot', 'DestRoot not found: %s', Args.DestRoot);
    end

    if isempty(Args.LogFile)
        Args.LogFile = fullfile(pwd, sprintf('bench_unzip_%s.log', datestr(now, 'yyyymmdd_HHMMSS')));
    end
    Fid = fopen(Args.LogFile, 'a');
    if Fid < 0
        error('bench_unzip:LogOpen', 'Cannot open log: %s', Args.LogFile);
    end
    LogCloser = onCleanup(@() fclose(Fid)); %#ok<NASGU>

    method = 'matlab';
    logLine_(Fid, sprintf('START method=%s ZipDir=%s nZip=%d DestRoot=%s Niter=%d Warmup=%d', ...
        method, Args.ZipDir, numel(Zips), Args.DestRoot, Args.Niter, Args.Warmup));
    [~, HostName] = system('hostname');
    logLine_(Fid, sprintf('HOST %s MATLAB %s', strtrim(HostName), version));

    Results = struct([]);
    Ires = 0;
    Pid = feature('getpid');

    for Iz = 1:numel(Zips)
        ZipPath = Zips{Iz};
        [~, BaseName, ~] = fileparts(ZipPath);
        [ZipMB, UnpackMB, Nfiles] = zipMetadata_(ZipPath);

        for W = 0:(Args.Warmup - 1)
            Dest = fullfile(Args.DestRoot, sprintf('unzip_bench_%d_%s_%s_w%d', Pid, method, BaseName, W));
            try
                timedCycleMatlab_(ZipPath, Dest);
            catch ME
                rmDest_(Dest);
                logLine_(Fid, sprintf('ZIP method=%s file=%s ERROR=warmup %s', method, [BaseName '.zip'], ME.message));
            end
        end

        for It = 1:Args.Niter
            Dest = fullfile(Args.DestRoot, sprintf('unzip_bench_%d_%s_%s_i%d', Pid, method, BaseName, It));
            try
                [UnzipSec, RmSec, CycleSec] = timedCycleMatlab_(ZipPath, Dest);
            catch ME
                rmDest_(Dest);
                logLine_(Fid, sprintf('ZIP method=%s file=%s iter=%d ERROR=%s', method, [BaseName '.zip'], It, ME.message));
                continue
            end
            Mbps = UnpackMB / UnzipSec;
            if UnzipSec <= 0
                Mbps = NaN;
            end
            Ires = Ires + 1;
            Results(Ires).Method = method; %#ok<AGROW>
            Results(Ires).File = [BaseName '.zip'];
            Results(Ires).Iter = It;
            Results(Ires).ZipMB = ZipMB;
            Results(Ires).UnpackMB = UnpackMB;
            Results(Ires).Nfiles = Nfiles;
            Results(Ires).UnzipSec = UnzipSec;
            Results(Ires).RmSec = RmSec;
            Results(Ires).CycleSec = CycleSec;
            Results(Ires).UnzipMBps = Mbps;
            msg = sprintf(['ZIP method=%s file=%s iter=%d zipMB=%.2f unpackMB=%.2f nFiles=%d ' ...
                'unzipSec=%.3f rmSec=%.3f cycleSec=%.3f unzipMBps=%.1f'], ...
                method, Results(Ires).File, It, ZipMB, UnpackMB, Nfiles, UnzipSec, RmSec, CycleSec, Mbps);
            logLine_(Fid, msg);
        end
    end

    printAvg_(Fid, Results, method);
    logLine_(Fid, 'END');
end

function Zips = discoverZips_(ZipDir, ZipFile)
    if ~isempty(ZipFile)
        if ~isfile(ZipFile)
            error('bench_unzip:ZipFile', 'Zip not found: %s', ZipFile);
        end
        Zips = {ZipFile};
        return
    end
    if isempty(ZipDir)
        error('bench_unzip:ZipDir', 'ZipDir or ZipFile required');
    end
    if ~isfolder(ZipDir)
        error('bench_unzip:ZipDir', 'ZipDir not found: %s', ZipDir);
    end
    D = dir(fullfile(ZipDir, '*.zip'));
    if isempty(D)
        error('bench_unzip:NoZips', 'No *.zip under %s', ZipDir);
    end
    Names = sort({D.name});
    Zips = cell(numel(Names), 1);
    for K = 1:numel(Names)
        Zips{K} = fullfile(ZipDir, Names{K});
    end
end

function [ZipMB, UnpackMB, Nfiles] = zipMetadata_(ZipPath)
    Info = dir(ZipPath);
    ZipMB = Info.bytes / (1024^2);
    Z = java.util.zip.ZipFile(java.io.File(ZipPath));
    C = onCleanup(@() Z.close()); %#ok<NASGU>
    Entries = Z.entries();
    Unpack = 0;
    Nfiles = 0;
    while Entries.hasMoreElements()
        E = Entries.nextElement();
        Unpack = Unpack + E.getSize();
        Nfiles = Nfiles + 1;
    end
    UnpackMB = Unpack / (1024^2);
end

function [UnzipSec, RmSec, CycleSec] = timedCycleMatlab_(ZipPath, Dest)
    rmDest_(Dest);
    if ~isfolder(Dest)
        mkdir(Dest);
    end
    T0 = tic;
    unzip(ZipPath, Dest);
    UnzipSec = toc(T0);
    T1 = tic;
    rmDest_(Dest);
    RmSec = toc(T1);
    CycleSec = UnzipSec + RmSec;
end

function rmDest_(Dest)
    if isfolder(Dest)
        if ispc
            rmdir(Dest, 's');
        else
            status = system(sprintf('rm -rf %s', Dest));
            if status ~= 0
                error('bench_unzip:RmDest', 'rm -rf failed for %s', Dest);
            end
        end
    end
end

function printAvg_(Fid, Results, method)
    if isempty(Results)
        return
    end
    UnzipSecs = [Results.UnzipSec];
    RmSecs = [Results.RmSec];
    CycleSecs = [Results.CycleSec];
    ZipMBs = [Results.ZipMB];
    Files = {Results.File};
    Nzip = numel(unique(Files));
    TotalUnpackGB = sum([Results.UnpackMB]) / 1024;
    msg = sprintf(['AVG method=%s nZip=%d nRec=%d unzipSec mean=%.3f med=%.3f ' ...
        'rmSec mean=%.3f med=%.3f cycleSec mean=%.3f med=%.3f zipMB mean=%.2f totalUnpackGB=%.2f'], ...
        method, Nzip, numel(Results), mean(UnzipSecs), median(UnzipSecs), ...
        mean(RmSecs), median(RmSecs), mean(CycleSecs), median(CycleSecs), ...
        mean(ZipMBs), TotalUnpackGB);
    logLine_(Fid, msg);
end

function logLine_(Fid, Msg)
    fprintf('%s %s\n', datestr(now, 'yyyy-mm-dd HH:MM:SS'), Msg);
    fprintf(Fid, '%s %s\n', datestr(now, 'yyyy-mm-dd HH:MM:SS'), Msg);
end
