function writeArchiveProcCatMat(Args)
% writeArchiveProcCatMat Batch-build proc Cat/Head MAT files in each visit folder.
% Delegates FITS read and MAT write to insertArchiveCatalogs2DB_perf (ProcCatMat='write'),
% routes perfLog to insertDB_proc_mat_build.log for the perf pass, writes error/visit sidecar logs,
% and optionally verifies the first N visits against FITS.
% Input  : * ...,key,val,...
%            'RootDir', 'ProcDirTemplate', 'ProcDirList', 'FileNameTemplate'
%            'OverWrite', 'VerifyFirstN', 'VerifyStrict'
%            'ExpectedProcCatFiles', 'CatalogAstroPerf', 'VisitIdleSec'
% Example: pipeline.last.insertDB.create_proc_mat.writeArchiveProcCatMat('RootDir','/bigdata2/projects/last/data/');

    arguments
        Args.RootDir (1,:) char = '/bigdata2/projects/last/data/';
        Args.ProcDirTemplate (1,:) char = 'LAST.*/20*/*/*/proc/*';
        Args.ProcDirList = [];
        Args.FileNameTemplate (1,:) char = 'LAST*proc_Cat_1.fits*';
        Args.OverWrite (1,1) logical = false;
        Args.VerifyFirstN (1,1) double {mustBeInteger, mustBeNonnegative} = 50;
        Args.VerifyStrict (1,1) logical = false;
        Args.ExpectedProcCatFiles (1,1) double {mustBeInteger, mustBePositive} = 480;
        Args.CatalogAstroPerf (1,1) logical = true;
        Args.VisitIdleSec (1,1) double {mustBeNonnegative} = 10;
    end

    FnName = 'writeArchiveProcCatMat';
    RunT0 = tic;
    StartDir = pwd;
    LogCtx = procMatBuildLogInit_();
    fprintf(1, '%s logs: %s\n', FnName, LogCtx.LogDir);

    pipeline.last.insertDB.create_proc_mat.procMatBuildSidecarLogs('reset', LogCtx);
    procMatBuildLogFull_(LogCtx, sprintf('%s RUN_START RootDir=%s ProcDirTemplate=%s OverWrite=%d VerifyFirstN=%d VerifyStrict=%d ExpectedProcCatFiles=%d VisitIdleSec=%.1f', ...
        FnName, Args.RootDir, Args.ProcDirTemplate, Args.OverWrite, Args.VerifyFirstN, Args.VerifyStrict, Args.ExpectedProcCatFiles, Args.VisitIdleSec));

    BuildLogOffset = procMatBuildFileSize_(LogCtx.BuildPath);

    CntVerifyOk = 0;
    CntVerifyFail = 0;
    CntProcFileWarn = 0;

    try
        pipeline.last.insertDB.perf.perfLogAlternateBaseName('set', 'insertDB_proc_mat_build.log');
        PerfLogCleanup = onCleanup(@() pipeline.last.insertDB.perf.perfLogAlternateBaseName('clear'));
        pipeline.last.insertDB.perf.insertArchiveCatalogs2DB_perf(Args.RootDir, Args.FileNameTemplate, ...
            'ProcDirTemplate', Args.ProcDirTemplate, ...
            'ProcDirList', Args.ProcDirList, ...
            'ProcCatMat', 'write', ...
            'ProcCatMatOverWrite', Args.OverWrite, ...
            'UpdateStatus', false, ...
            'IgnoreStatus', true, ...
            'CatalogFileInventory', false, ...
            'CatalogRawFileRead', false, ...
            'CatalogAstroPerf', Args.CatalogAstroPerf, ...
            'VisitIdleSec', Args.VisitIdleSec, ...
            'ProcMatSidecarLogCtx', LogCtx);
    catch ME
        procMatBuildLogError_(LogCtx, sprintf('%s perf EXCEPTION %s', FnName, ME.message));
        procMatBuildLogFull_(LogCtx, sprintf('%s RUN_ABORT perf EXCEPTION %s', FnName, ME.message));
        cd(StartDir);
        rethrow(ME);
    end

    PerfLogEndOffset = procMatBuildFileSize_(LogCtx.BuildPath);
    PerfLines = procMatBuildReadLinesRange_(LogCtx.BuildPath, BuildLogOffset, PerfLogEndOffset);
    procMatBuildScanPerfErrors_(LogCtx, PerfLines);

    Slice = procMatBuildParsePerfSlice_(PerfLines);
    VisitPaths = unique(Slice.VisitPaths, 'stable');
    RowByPath = Slice.RowByPath;

    for Iv = 1:numel(VisitPaths)
        DataDir = VisitPaths{Iv};
        Nproc = procMatBuildCountProcCatFiles_(DataDir, Args.FileNameTemplate);
        if Nproc ~= Args.ExpectedProcCatFiles
            CntProcFileWarn = CntProcFileWarn + 1;
            Msg = sprintf('WARNING procCatFileCount=%d expected=%d path=%s', Nproc, Args.ExpectedProcCatFiles, DataDir);
            procMatBuildLogWarn_(LogCtx, Msg);
            procMatBuildLogFull_(LogCtx, sprintf('%s %s', FnName, Msg));
        end
        if ~isKey(RowByPath, DataDir)
            Nrows = procMatBuildSumRowsFromMat_(DataDir);
            if isnan(Nrows)
                RowByPath(DataDir) = -1;
            else
                RowByPath(DataDir) = Nrows;
            end
        end
    end

    pipeline.last.insertDB.create_proc_mat.procMatBuildSidecarLogs('mergeRowsMap', LogCtx, RowByPath);
    pipeline.last.insertDB.create_proc_mat.procMatBuildSidecarLogs('flush', LogCtx);

    if Args.VerifyFirstN > 0
        Dirs = pipeline.last.insertDB.perf.perfDiscoverProcVisitDirs( ...
            Args.RootDir, Args.ProcDirTemplate, Args.ProcDirList);
        Ndir = numel(Dirs);
        Nverify = min(Args.VerifyFirstN, Ndir);
        procMatBuildLogFull_(LogCtx, sprintf('%s verifyStart Nverify=%d discoveryNdir=%d', FnName, Nverify, Ndir));
        for Idir = 1:Nverify
            DataDir = procMatBuildDataDirFromDiscovery_(Dirs, Idir, Args.ProcDirList);
            [Ok, Detail] = verifyVisitFitsVsProcMat_(DataDir, Args.FileNameTemplate, Args.ExpectedProcCatFiles, LogCtx);
            if Ok
                CntVerifyOk = CntVerifyOk + 1;
                procMatBuildLogFull_(LogCtx, sprintf('%s verifyVisit ok %s', FnName, DataDir));
            else
                CntVerifyFail = CntVerifyFail + 1;
                procMatBuildLogError_(LogCtx, sprintf('%s verifyVisit FAIL %s %s', FnName, DataDir, Detail));
                procMatBuildLogFull_(LogCtx, sprintf('%s verifyVisit FAIL %s %s', FnName, DataDir, Detail));
                if Args.VerifyStrict
                    cd(StartDir);
                    error('writeArchiveProcCatMat:VerifyStrict', 'verifyVisit failed: %s (%s)', DataDir, Detail);
                end
            end
        end
        procMatBuildLogFull_(LogCtx, sprintf('%s verifyDone ok=%d fail=%d', FnName, CntVerifyOk, CntVerifyFail));
    end

    procMatBuildLogFull_(LogCtx, sprintf('%s SUMMARY processedVisits=%d procFileWarnings=%d verifyOk=%d verifyFail=%d', ...
        FnName, numel(VisitPaths), CntProcFileWarn, CntVerifyOk, CntVerifyFail));
    procMatBuildLogFull_(LogCtx, sprintf('%s FINISH', FnName), toc(RunT0));
    cd(StartDir);
    fprintf(1, '%s done in %.1fs — log: %s\n', FnName, toc(RunT0), LogCtx.BuildPath);
end

%% Logging

function LogCtx = procMatBuildLogInit_()
    LogCtx.LogDir = procMatBuildExpandHome_('~/matlab/log/last/insertArchive/');
    if ~isfolder(LogCtx.LogDir)
        mkdir(LogCtx.LogDir);
    end
    LogCtx.BuildPath = fullfile(LogCtx.LogDir, 'insertDB_proc_mat_build.log');
    LogCtx.ErrorsPath = fullfile(LogCtx.LogDir, 'insertDB_proc_mat_build_errors.log');
    LogCtx.VisitsPath = fullfile(LogCtx.LogDir, 'insertDB_proc_mat_build_visits.log');
    LogCtx.VisitRowsPath = fullfile(LogCtx.LogDir, 'insertDB_proc_mat_build_visit_rows.log');
    procMatBuildTruncate_(LogCtx.BuildPath);
    procMatBuildTruncate_(LogCtx.ErrorsPath);
    procMatBuildTruncate_(LogCtx.VisitsPath);
    procMatBuildTruncate_(LogCtx.VisitRowsPath);
end

function procMatBuildLogFull_(LogCtx, Message, Seconds)
    Ts = datestr(now, 'yyyy-mm-dd HH:MM:SS.FFF');
    if nargin >= 3 && ~isempty(Seconds)
        Line = sprintf('%s %s %.1fs', Ts, Message, Seconds);
    else
        Line = sprintf('%s %s', Ts, Message);
    end
    procMatBuildAppendLine_(LogCtx.BuildPath, Line);
end

function procMatBuildLogFullLines_(LogCtx, Lines)
    if isempty(Lines)
        return
    end
    Fid = fopen(LogCtx.BuildPath, 'a');
    if Fid < 0
        return
    end
    for I = 1:numel(Lines)
        fprintf(Fid, '%s\n', Lines{I});
    end
    fclose(Fid);
end

function procMatBuildLogError_(LogCtx, Message)
    procMatBuildLogIssue_(LogCtx.ErrorsPath, Message);
end

function procMatBuildLogWarn_(LogCtx, Message)
    procMatBuildLogIssue_(LogCtx.ErrorsPath, Message);
end

function procMatBuildLogIssue_(Path, Message)
    Ts = datestr(now, 'yyyy-mm-dd HH:MM:SS.FFF');
    procMatBuildAppendLine_(Path, sprintf('%s %s', Ts, Message));
end

function procMatBuildAppendLine_(Path, Line)
    Fid = fopen(Path, 'a');
    if Fid < 0
        return
    end
    fprintf(Fid, '%s\n', Line);
    fclose(Fid);
end

function procMatBuildTruncate_(Path)
    Fid = fopen(Path, 'w');
    if Fid >= 0
        fclose(Fid);
    end
end

function procMatBuildScanPerfErrors_(LogCtx, Lines)
    Pat = '(?i)(ERROR=|visitEnd broken|saveProcCatMat ERROR|\bwarning\b|\bexception\b)';
    for I = 1:numel(Lines)
        if ~isempty(regexp(Lines{I}, Pat, 'once'))
            procMatBuildLogError_(LogCtx, Lines{I});
        end
    end
end

%% Perf log parse

function Slice = procMatBuildParsePerfSlice_(Lines)
    Slice.VisitPaths = {};
    Slice.RowByPath = containers.Map('KeyType', 'char', 'ValueType', 'double');
    for I = 1:numel(Lines)
        Line = Lines{I};
        Tok = regexp(Line, 'insertArchiveCatalogs2DB_perf visitStart\s+(\S+)', 'tokens', 'once');
        if ~isempty(Tok)
            Slice.VisitPaths{end+1} = Tok{1}; %#ok<AGROW>
            continue
        end
        Tok = regexp(Line, 'insertArchiveCatalogs2DB_perf catalogAstroPerf nCat=\d+ nRows=(\d+) catSec=[\d.]+ rowsPerSec=\S+ (\S+)\s*$', 'tokens', 'once');
        if ~isempty(Tok)
            Nrows = str2double(Tok{1});
            Path = Tok{2};
            Slice.RowByPath(Path) = Nrows;
            continue
        end
    end
end

function N = procMatBuildFileSize_(Path)
    if ~isfile(Path)
        N = 0;
        return
    end
    D = dir(Path);
    N = D.bytes;
end

function Lines = procMatBuildReadLinesFromOffset_(Path, Offset)
    Lines = procMatBuildReadLinesRange_(Path, Offset, inf);
end

function Lines = procMatBuildReadLinesRange_(Path, Offset, EndOffset)
    Lines = {};
    if ~isfile(Path)
        return
    end
    Fid = fopen(Path, 'r');
    if Fid < 0
        return
    end
    Cleaner = onCleanup(@() fclose(Fid));
    if Offset > 0
        fseek(Fid, Offset, 'bof');
    end
    if isinf(EndOffset)
        Raw = fread(Fid, '*char')';
    else
        Nbytes = max(0, EndOffset - Offset);
        Raw = fread(Fid, Nbytes, '*char=>char')';
    end
    if isempty(Raw)
        return
    end
    Lines = splitlines(string(Raw));
    Lines = cellstr(Lines);
end

%% Discovery / inventory

function DataDir = procMatBuildDataDirFromDiscovery_(Dirs, Idir, ProcDirList)
    if isempty(ProcDirList)
        DataDir = fullfile(Dirs(Idir).folder, Dirs(Idir).name);
    else
        DataDir = Dirs(Idir);
        if iscell(DataDir)
            DataDir = DataDir{1};
        end
        if isstring(DataDir)
            DataDir = char(DataDir);
        end
    end
end

function N = procMatBuildCountProcCatFiles_(DataDir, FileNameTemplate)
    List = dir(fullfile(DataDir, FileNameTemplate));
    List = List(~[List.isdir]);
    N = numel(List);
end

function Nrows = procMatBuildSumRowsFromMat_(DataDir)
    Nrows = NaN;
    try
        [Cat, ~] = pipeline.last.load.loadProcCatMat(DataDir, ...
            'CatTemplate', procMatBuildCatTemplate_(), ...
            'HeadTemplate', procMatBuildHeadTemplate_());
        Nrows = procMatBuildSumCatRows_(Cat);
    catch
        Nrows = NaN;
    end
end

function T = procMatBuildCatTemplate_()
    T = '*_sci_proc_Cat_1.mat';
end

function T = procMatBuildHeadTemplate_()
    T = '*_sci_proc_Head_1.mat';
end

function N = procMatBuildSumCatRows_(Cat)
    if isempty(Cat)
        N = 0;
        return
    end
    C = Cat(:);
    N = sum(arrayfun(@(c) size(c.Catalog, 1), C));
end

%% Verify FITS vs MAT

function [Ok, Detail] = verifyVisitFitsVsProcMat_(DataDir, FileNameTemplate, ExpectedProcCatFiles, LogCtx)
    Ok = false;
    Detail = '';
    PrevDir = pwd;
    try
        Nproc = procMatBuildCountProcCatFiles_(DataDir, FileNameTemplate);
        if Nproc ~= ExpectedProcCatFiles
            Msg = sprintf('WARNING procCatFileCount=%d expected=%d path=%s', Nproc, ExpectedProcCatFiles, DataDir);
            procMatBuildLogWarn_(LogCtx, Msg);
        end
        cd(DataDir);
        CatF = AstroCatalog(FileNameTemplate, 'UseMex', true);
        AHf = AstroHeader(FileNameTemplate, 3, 'UseMex', true);
        if numel(CatF) < 2
            Detail = 'FITS read yielded fewer than 2 catalogs';
            cd(PrevDir);
            return
        end
        NCol = max(arrayfun(@(x) size(x.Catalog, 2), CatF));
        Idx = arrayfun(@(x) size(x.Catalog, 2) < NCol, CatF);
        if Idx(1)
            Detail = 'first FITS catalog broken (column count)';
            cd(PrevDir);
            return
        end
        CatF(Idx) = [];
        AHf(Idx) = [];
        Nfits = numel(CatF);
        NrowsF = procMatBuildSumCatRows_(CatF);

        CatTemplate = procMatBuildCatTemplate_();
        HeadTemplate = procMatBuildHeadTemplate_();
        MatCatList = dir(fullfile(DataDir, CatTemplate));
        MatHeadList = dir(fullfile(DataDir, HeadTemplate));
        if numel(MatCatList) ~= 1 || numel(MatHeadList) ~= 1
            Detail = 'proc MAT Cat/Head missing or ambiguous in visit folder';
            cd(PrevDir);
            return
        end
        [CatM, AHm, Info] = pipeline.last.load.loadProcCatMat(DataDir, ...
            'CatTemplate', CatTemplate, 'HeadTemplate', HeadTemplate);
        Ngrid = numel(CatM);
        Nempty = sum(arrayfun(@(c) size(c.Catalog, 2) == 0, CatM(:)));
        Nmat = Ngrid - Nempty;
        NrowsM = procMatBuildSumCatRows_(CatM);
        if Nfits ~= Nmat
            Detail = sprintf('catalog count FITS=%d MAT=%d (grid=%d empty=%d)', Nfits, Nmat, Ngrid, Nempty);
            cd(PrevDir);
            return
        end
        if NrowsF ~= NrowsM
            Detail = sprintf('total row count FITS=%d MAT=%d', NrowsF, NrowsM);
            cd(PrevDir);
            return
        end
        FileNameGrid = string(Info.FileName);
        for I = 1:Nfits
            Key = procMatBuildFilenameKey_(AHf(I));
            Mask = FileNameGrid == Key;
            if ~any(Mask(:))
                Mask = contains(FileNameGrid, string(filesep)) & endsWith(FileNameGrid, Key);
                if ~any(Mask(:))
                    [~, Base, Ext] = fileparts(char(Key));
                    Short = string([Base, Ext]);
                    Mask = endsWith(FileNameGrid, Short);
                end
            end
            if ~any(Mask(:))
                Detail = sprintf('no MAT grid cell for FILENAME=%s (FITS index %d)', Key, I);
                cd(PrevDir);
                return
            end
            [Ie, Ic] = find(Mask, 1, 'first');
            Dmax = max(abs(CatF(I).Catalog(:) - CatM(Ie, Ic).Catalog(:)), [], 'omitnan');
            if isempty(Dmax) || Dmax > 0
                Detail = sprintf('catalog data mismatch maxAbsDiff=%g FILENAME=%s', Dmax, Key);
                cd(PrevDir);
                return
            end
            for Kw = ["JD", "FIELDID", "FILENAME"]
                Vf = string(AHf(I).getVal(char(Kw)));
                Vm = string(AHm(Ie, Ic).getVal(char(Kw)));
                if strtrim(Vf) ~= strtrim(Vm)
                    Detail = sprintf('header %s FITS=%s MAT=%s', Kw, Vf, Vm);
                    cd(PrevDir);
                    return
                end
            end
        end
        Ok = true;
        Detail = sprintf('nCat=%d nRows=%d', Nfits, NrowsF);
    catch ME
        Detail = ME.message;
    end
    cd(PrevDir);
end

function Key = procMatBuildFilenameKey_(AH)
    Key = string(AH.getVal('FILENAME'));
    if strlength(Key) == 0
        F = string(AH.File);
        if strlength(F) == 0
            Key = "";
        else
            [~, Base, Ext] = fileparts(char(F));
            Key = string([Base, Ext]);
        end
    end
end

function Path = procMatBuildExpandHome_(Path)
    if startsWith(Path, '~')
        if ispc
            Home = getenv('USERPROFILE');
        else
            Home = getenv('HOME');
        end
        if isempty(Home)
            Home = char(java.lang.System.getProperty('user.home'));
        end
        if startsWith(Path, '~/') || strcmp(Path, '~')
            Path = fullfile(Home, Path(3:end));
        else
            Path = fullfile(Home, Path(2:end));
        end
    end
end
