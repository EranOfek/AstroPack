function writeArchiveEpochCatMat(Args)
% writeArchiveEpochCatMat Batch-build epoch proc Cat/Head MAT files in each visit folder.
% Delegates FITS read and MAT write to insertArchiveCatalogs2DB_perf (EpochCatMat='write'),
% routes perfLog to insertDB_epoch_mat_build.log for the perf pass, writes error/visit sidecar logs,
% and optionally verifies the first N visits against FITS.
% Input  : * ...,key,val,...
%            'RootDir', 'ProcDirTemplate', 'ProcDirList', 'FileNameTemplate'
%            'OverWrite', 'VerifyFirstN', 'VerifyStrict'
%            'ExpectedProcCatFiles', 'CatalogAstroPerf'
% Example: pipeline.last.insertDB.writeArchiveEpochCatMat('RootDir','/bigdata2/projects/last/data/');

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
    end

    FnName = 'writeArchiveEpochCatMat';
    RunT0 = tic;
    StartDir = pwd;
    LogCtx = epochMatBuildLogInit_();
    fprintf(1, '%s logs: %s\n', FnName, LogCtx.LogDir);

    epochMatBuildLogFull_(LogCtx, sprintf('%s RUN_START RootDir=%s ProcDirTemplate=%s OverWrite=%d VerifyFirstN=%d VerifyStrict=%d ExpectedProcCatFiles=%d', ...
        FnName, Args.RootDir, Args.ProcDirTemplate, Args.OverWrite, Args.VerifyFirstN, Args.VerifyStrict, Args.ExpectedProcCatFiles));

    BuildLogOffset = epochMatBuildFileSize_(LogCtx.BuildPath);

    CntVerifyOk = 0;
    CntVerifyFail = 0;
    CntProcFileWarn = 0;

    try
        pipeline.last.insertDB.perfLogAlternateBaseName('set', 'insertDB_epoch_mat_build.log');
        PerfLogCleanup = onCleanup(@() pipeline.last.insertDB.perfLogAlternateBaseName('clear'));
        pipeline.last.insertDB.insertArchiveCatalogs2DB_perf(Args.RootDir, Args.FileNameTemplate, ...
            'ProcDirTemplate', Args.ProcDirTemplate, ...
            'ProcDirList', Args.ProcDirList, ...
            'EpochCatMat', 'write', ...
            'EpochCatMatOverWrite', Args.OverWrite, ...
            'UpdateStatus', false, ...
            'IgnoreStatus', true, ...
            'CatalogFileInventory', false, ...
            'CatalogRawFileRead', false, ...
            'CatalogAstroPerf', Args.CatalogAstroPerf);
    catch ME
        epochMatBuildLogError_(LogCtx, sprintf('%s perf EXCEPTION %s', FnName, ME.message));
        epochMatBuildLogFull_(LogCtx, sprintf('%s RUN_ABORT perf EXCEPTION %s', FnName, ME.message));
        cd(StartDir);
        rethrow(ME);
    end

    PerfLogEndOffset = epochMatBuildFileSize_(LogCtx.BuildPath);
    PerfLines = epochMatBuildReadLinesRange_(LogCtx.BuildPath, BuildLogOffset, PerfLogEndOffset);
    epochMatBuildScanPerfErrors_(LogCtx, PerfLines);

    Slice = epochMatBuildParsePerfSlice_(PerfLines);
    VisitPaths = unique(Slice.VisitPaths, 'stable');
    RowByPath = Slice.RowByPath;

    for Iv = 1:numel(VisitPaths)
        DataDir = VisitPaths{Iv};
        Nproc = epochMatBuildCountProcCatFiles_(DataDir, Args.FileNameTemplate);
        if Nproc ~= Args.ExpectedProcCatFiles
            CntProcFileWarn = CntProcFileWarn + 1;
            Msg = sprintf('WARNING procCatFileCount=%d expected=%d path=%s', Nproc, Args.ExpectedProcCatFiles, DataDir);
            epochMatBuildLogWarn_(LogCtx, Msg);
            epochMatBuildLogFull_(LogCtx, sprintf('%s %s', FnName, Msg));
        end
        if ~isKey(RowByPath, DataDir)
            Nrows = epochMatBuildSumRowsFromMat_(DataDir);
            if isnan(Nrows)
                RowByPath(DataDir) = -1;
            else
                RowByPath(DataDir) = Nrows;
            end
        end
    end

    epochMatBuildWriteVisitsSidecars_(LogCtx, VisitPaths, RowByPath);

    if Args.VerifyFirstN > 0
        Dirs = pipeline.last.insertDB.visitMat.perfDiscoverProcVisitDirs( ...
            Args.RootDir, Args.ProcDirTemplate, Args.ProcDirList);
        Ndir = numel(Dirs);
        Nverify = min(Args.VerifyFirstN, Ndir);
        epochMatBuildLogFull_(LogCtx, sprintf('%s verifyStart Nverify=%d discoveryNdir=%d', FnName, Nverify, Ndir));
        for Idir = 1:Nverify
            DataDir = epochMatBuildDataDirFromDiscovery_(Dirs, Idir, Args.ProcDirList);
            [Ok, Detail] = verifyVisitFitsVsEpochMat_(DataDir, Args.FileNameTemplate, Args.ExpectedProcCatFiles, LogCtx);
            if Ok
                CntVerifyOk = CntVerifyOk + 1;
                epochMatBuildLogFull_(LogCtx, sprintf('%s verifyVisit ok %s', FnName, DataDir));
            else
                CntVerifyFail = CntVerifyFail + 1;
                epochMatBuildLogError_(LogCtx, sprintf('%s verifyVisit FAIL %s %s', FnName, DataDir, Detail));
                epochMatBuildLogFull_(LogCtx, sprintf('%s verifyVisit FAIL %s %s', FnName, DataDir, Detail));
                if Args.VerifyStrict
                    cd(StartDir);
                    error('writeArchiveEpochCatMat:VerifyStrict', 'verifyVisit failed: %s (%s)', DataDir, Detail);
                end
            end
        end
        epochMatBuildLogFull_(LogCtx, sprintf('%s verifyDone ok=%d fail=%d', FnName, CntVerifyOk, CntVerifyFail));
    end

    epochMatBuildLogFull_(LogCtx, sprintf('%s SUMMARY processedVisits=%d procFileWarnings=%d verifyOk=%d verifyFail=%d', ...
        FnName, numel(VisitPaths), CntProcFileWarn, CntVerifyOk, CntVerifyFail));
    epochMatBuildLogFull_(LogCtx, sprintf('%s FINISH', FnName), toc(RunT0));
    cd(StartDir);
    fprintf(1, '%s done in %.1fs — log: %s\n', FnName, toc(RunT0), LogCtx.BuildPath);
end

%% Logging

function LogCtx = epochMatBuildLogInit_()
    LogCtx.LogDir = epochMatBuildExpandHome_('~/matlab/log/last/insertArchive/');
    if ~isfolder(LogCtx.LogDir)
        mkdir(LogCtx.LogDir);
    end
    LogCtx.BuildPath = fullfile(LogCtx.LogDir, 'insertDB_epoch_mat_build.log');
    LogCtx.ErrorsPath = fullfile(LogCtx.LogDir, 'insertDB_epoch_mat_build_errors.log');
    LogCtx.VisitsPath = fullfile(LogCtx.LogDir, 'insertDB_epoch_mat_build_visits.log');
    LogCtx.VisitRowsPath = fullfile(LogCtx.LogDir, 'insertDB_epoch_mat_build_visit_rows.log');
    epochMatBuildTruncate_(LogCtx.BuildPath);
    epochMatBuildTruncate_(LogCtx.ErrorsPath);
    epochMatBuildTruncate_(LogCtx.VisitsPath);
    epochMatBuildTruncate_(LogCtx.VisitRowsPath);
end

function epochMatBuildLogFull_(LogCtx, Message, Seconds)
    Ts = datestr(now, 'yyyy-mm-dd HH:MM:SS.FFF');
    if nargin >= 3 && ~isempty(Seconds)
        Line = sprintf('%s %s %.1fs', Ts, Message, Seconds);
    else
        Line = sprintf('%s %s', Ts, Message);
    end
    epochMatBuildAppendLine_(LogCtx.BuildPath, Line);
end

function epochMatBuildLogFullLines_(LogCtx, Lines)
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

function epochMatBuildLogError_(LogCtx, Message)
    epochMatBuildLogIssue_(LogCtx.ErrorsPath, Message);
end

function epochMatBuildLogWarn_(LogCtx, Message)
    epochMatBuildLogIssue_(LogCtx.ErrorsPath, Message);
end

function epochMatBuildLogIssue_(Path, Message)
    Ts = datestr(now, 'yyyy-mm-dd HH:MM:SS.FFF');
    epochMatBuildAppendLine_(Path, sprintf('%s %s', Ts, Message));
end

function epochMatBuildAppendLine_(Path, Line)
    Fid = fopen(Path, 'a');
    if Fid < 0
        return
    end
    fprintf(Fid, '%s\n', Line);
    fclose(Fid);
end

function epochMatBuildTruncate_(Path)
    Fid = fopen(Path, 'w');
    if Fid >= 0
        fclose(Fid);
    end
end

function epochMatBuildScanPerfErrors_(LogCtx, Lines)
    Pat = '(?i)(ERROR=|visitEnd broken|saveEpochCatMat ERROR|\bwarning\b|\bexception\b)';
    for I = 1:numel(Lines)
        if ~isempty(regexp(Lines{I}, Pat, 'once'))
            epochMatBuildLogError_(LogCtx, Lines{I});
        end
    end
end

function epochMatBuildWriteVisitsSidecars_(LogCtx, VisitPaths, RowByPath)
    if isempty(VisitPaths)
        epochMatBuildTruncate_(LogCtx.VisitsPath);
        epochMatBuildTruncate_(LogCtx.VisitRowsPath);
        return
    end
    Sorted = sort(VisitPaths);
    FidV = fopen(LogCtx.VisitsPath, 'w');
    FidR = fopen(LogCtx.VisitRowsPath, 'w');
    if FidV < 0 || FidR < 0
        if FidV >= 0, fclose(FidV); end
        if FidR >= 0, fclose(FidR); end
        return
    end
    for I = 1:numel(Sorted)
        P = Sorted{I};
        fprintf(FidV, '%s\n', P);
        if isKey(RowByPath, P)
            Nrows = RowByPath(P);
        else
            Nrows = -1;
        end
        fprintf(FidR, '%s\t%d\n', P, Nrows);
    end
    fclose(FidV);
    fclose(FidR);
end

%% Perf log parse

function Slice = epochMatBuildParsePerfSlice_(Lines)
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

function N = epochMatBuildFileSize_(Path)
    if ~isfile(Path)
        N = 0;
        return
    end
    D = dir(Path);
    N = D.bytes;
end

function Lines = epochMatBuildReadLinesFromOffset_(Path, Offset)
    Lines = epochMatBuildReadLinesRange_(Path, Offset, inf);
end

function Lines = epochMatBuildReadLinesRange_(Path, Offset, EndOffset)
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

function DataDir = epochMatBuildDataDirFromDiscovery_(Dirs, Idir, ProcDirList)
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

function N = epochMatBuildCountProcCatFiles_(DataDir, FileNameTemplate)
    List = dir(fullfile(DataDir, FileNameTemplate));
    List = List(~[List.isdir]);
    N = numel(List);
end

function Nrows = epochMatBuildSumRowsFromMat_(DataDir)
    Nrows = NaN;
    try
        [Cat, ~] = pipeline.last.load.loadEpochCatMat(DataDir, ...
            'CatTemplate', epochMatBuildCatTemplate_(), ...
            'HeadTemplate', epochMatBuildHeadTemplate_());
        Nrows = epochMatBuildSumCatRows_(Cat);
    catch
        Nrows = NaN;
    end
end

function T = epochMatBuildCatTemplate_()
    T = '*_000_001_000_sci_proc_Cat_1.mat';
end

function T = epochMatBuildHeadTemplate_()
    T = '*_000_001_000_sci_proc_Head_1.mat';
end

function N = epochMatBuildSumCatRows_(Cat)
    if isempty(Cat)
        N = 0;
        return
    end
    C = Cat(:);
    N = sum(arrayfun(@(c) size(c.Catalog, 1), C));
end

%% Verify FITS vs MAT

function [Ok, Detail] = verifyVisitFitsVsEpochMat_(DataDir, FileNameTemplate, ExpectedProcCatFiles, LogCtx)
    Ok = false;
    Detail = '';
    PrevDir = pwd;
    try
        Nproc = epochMatBuildCountProcCatFiles_(DataDir, FileNameTemplate);
        if Nproc ~= ExpectedProcCatFiles
            Msg = sprintf('WARNING procCatFileCount=%d expected=%d path=%s', Nproc, ExpectedProcCatFiles, DataDir);
            epochMatBuildLogWarn_(LogCtx, Msg);
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
        NrowsF = epochMatBuildSumCatRows_(CatF);

        CatTemplate = epochMatBuildCatTemplate_();
        HeadTemplate = epochMatBuildHeadTemplate_();
        MatCatList = dir(fullfile(DataDir, CatTemplate));
        MatHeadList = dir(fullfile(DataDir, HeadTemplate));
        if numel(MatCatList) ~= 1 || numel(MatHeadList) ~= 1
            Detail = 'epoch MAT Cat/Head missing or ambiguous in visit folder';
            cd(PrevDir);
            return
        end
        [CatM, AHm, Info] = pipeline.last.load.loadEpochCatMat(DataDir, ...
            'CatTemplate', CatTemplate, 'HeadTemplate', HeadTemplate);
        Ngrid = numel(CatM);
        Nempty = sum(arrayfun(@(c) size(c.Catalog, 2) == 0, CatM(:)));
        Nmat = Ngrid - Nempty;
        NrowsM = epochMatBuildSumCatRows_(CatM);
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
            Key = epochMatBuildFilenameKey_(AHf(I));
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

function Key = epochMatBuildFilenameKey_(AH)
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

function Path = epochMatBuildExpandHome_(Path)
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
