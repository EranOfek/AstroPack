function Result = writeVisitCatalogs2Mat_perf(RootDir, FileNameTemplate, Args)
    % Write two visit-level MAT files (Cat and AH) after reading proc Cat FITS.
    % Input  : - RootDir, FileNameTemplate (same as insertArchiveCatalogs2DB_perf)
    %          * ...,key,val,...
    %        'ProcDirTemplate', 'ProcDirList', 'MatSubDir' (default '_mat')
    %        'OverWrite', 'IgnoreStatus', 'UseZipCatalogs', 'ZipDir', 'ExtractRoot'
    % Output : - Result struct with summary counts
    % Example:
    %   pipeline.last.insertDB.visitMat.writeVisitCatalogs2Mat_perf('/bigdata2/projects/last/data/', ...
    %       'ProcDirTemplate','LAST.01.01.01/2026/06/01/proc/*');
    arguments
        RootDir                = '/bigdata2/projects/last/data/';
        FileNameTemplate       = 'LAST*proc_Cat_1.fits*';
        Args.ProcDirTemplate   = '/proc/*';
        Args.ProcDirList       = [];
        Args.MatSubDir (1,:) char = '_mat';
        Args.OverWrite(1,1) logical = false;
        Args.IgnoreStatus(1,1) logical = true;
        Args.UseZipCatalogs(1,1) logical = false;
        Args.ZipDir (1,:) char = '';
        Args.ExtractRoot (1,:) char = '';
    end
    FnName = 'writeVisitCatalogs2Mat_perf';
    RunT0 = tic;
    pipeline.last.insertDB.perfLogRunBanner(FnName);
    perfLog(sprintf('%s START RootDir=%s OverWrite=%d IgnoreStatus=%d UseZipCatalogs=%d MatSubDir=%s', ...
        FnName, RootDir, Args.OverWrite, Args.IgnoreStatus, Args.UseZipCatalogs, Args.MatSubDir));
    CntSkippedExists = 0;
    CntNoStatus = 0;
    CntNoData = 0;
    CntBroken = 0;
    CntSaved = 0;
    VisitStats = pipeline.last.insertDB.perfVisitStats('init');
    Dir = pwd;
    TicDir = tic;
    Dirs = pipeline.last.insertDB.visitMat.perfDiscoverProcVisitDirs(RootDir, Args.ProcDirTemplate, Args.ProcDirList);
    Ndir = numel(Dirs);
    perfLog(sprintf('%s dirDiscovery Ndir=%d', FnName, Ndir), toc(TicDir));
    for Idir = 1:Ndir
        if isempty(Args.ProcDirList)
            DataDir = strcat(Dirs(Idir).folder, '/', Dirs(Idir).name);
        else
            DataDir = Dirs(Idir);
            if iscell(DataDir)
                DataDir = DataDir{1};
            end
            if isstring(DataDir)
                DataDir = char(DataDir);
            end
        end
        TicVisit = tic;
        perfLog(sprintf('%s visitStart %s', FnName, DataDir));
        VisitId = perfVisitIdFromDataDir_(DataDir);
        [CatPath, HdrPath] = perfMatPaths_(DataDir, Args.MatSubDir, VisitId);
        if ~Args.OverWrite && isfile(CatPath) && isfile(HdrPath)
            cd(Dir);
            CntSkippedExists = CntSkippedExists + 1;
            VisitStats = perfVisitEnd_(VisitStats, TicVisit, 0, 'skipExists', FnName, DataDir);
            continue
        end
        cd(DataDir);
        if Args.IgnoreStatus
            Injected = false;
        else
            try
                TicStatus = tic;
                Injected = contains(fileread('.status'), "injected into the proc catalog DB") ...
                    | contains(fileread('.status'), "not injectable into the proc catalog DB due to broken data files");
                perfLog(sprintf('%s readStatus %s', FnName, DataDir), toc(TicStatus));
            catch
                cd(Dir);
                CntNoStatus = CntNoStatus + 1;
                VisitStats = perfVisitEnd_(VisitStats, TicVisit, 0, 'noStatus', FnName, DataDir);
                continue
            end
        end
        if Injected
            cd(Dir);
            VisitStats = perfVisitEnd_(VisitStats, TicVisit, 0, 'skippedInjected', FnName, DataDir);
            continue
        end
        ZipVisit = perfZipVisitStateInit_();
        if Args.UseZipCatalogs
            [ZipOk, ZipVisit, ZipFailOutcome] = perfZipPrepareAndExtract_(FnName, DataDir, Dir, Args);
            if ~ZipOk
                cd(Dir);
                VisitStats = perfVisitEnd_(VisitStats, TicVisit, 0, ZipFailOutcome, FnName, DataDir);
                continue
            end
        end
        try
            TicCat = tic;
            Cat = AstroCatalog(FileNameTemplate, 'UseMex', true);
            perfLog(sprintf('%s readAstroCatalog %s', FnName, DataDir), toc(TicCat));
            TicHdr = tic;
            AH = AstroHeader(FileNameTemplate, 3, 'UseMex', true);
            perfLog(sprintf('%s readAstroHeader %s', FnName, DataDir), toc(TicHdr));
        catch
            cd(Dir);
            perfZipRmExtract_(FnName, ZipVisit, Dir);
            CntBroken = CntBroken + 1;
            VisitStats = perfVisitEnd_(VisitStats, TicVisit, 0, 'broken', FnName, DataDir);
            continue
        end
        cd(Dir);
        perfZipRmExtract_(FnName, ZipVisit, Dir);
        if numel(Cat) < 2
            CntNoData = CntNoData + 1;
            VisitStats = perfVisitEnd_(VisitStats, TicVisit, 0, 'noData', FnName, DataDir);
            continue
        end
        NCol = max(arrayfun(@(X) size(X.Catalog, 2), Cat));
        Idx = arrayfun(@(X) size(X.Catalog, 2) < NCol, Cat);
        if Idx(1)
            CntBroken = CntBroken + 1;
            VisitStats = perfVisitEnd_(VisitStats, TicVisit, 0, 'brokenFirstCatalog', FnName, DataDir);
            continue
        end
        Cat(Idx) = [];
        AH(Idx) = [];
        if numel(Cat) < 2
            CntNoData = CntNoData + 1;
            VisitStats = perfVisitEnd_(VisitStats, TicVisit, 0, 'noData', FnName, DataDir);
            continue
        end
        MatDir = fileparts(CatPath);
        if ~isfolder(MatDir)
            mkdir(MatDir);
        end
        TicSaveCat = tic;
        try
            pipeline.last.insertDB.visitMat.saveVisitCatalogMat(CatPath, Cat);
        catch ME
            perfLog(sprintf('%s saveMatCat ERROR=%s dest=%s %s', FnName, ME.message, CatPath, DataDir), toc(TicSaveCat));
            CntBroken = CntBroken + 1;
            VisitStats = perfVisitEnd_(VisitStats, TicVisit, 0, 'broken', FnName, DataDir);
            continue
        end
        CatInfo = dir(CatPath);
        CatSizeMB = CatInfo.bytes / (1024^2);
        perfLog(sprintf('%s saveMatCat dest=%s nCat=%d sizeMB=%.2f InsertDbMatVersion=3 %s', ...
            FnName, CatPath, numel(Cat), CatSizeMB, DataDir), toc(TicSaveCat));
        TicSaveHdr = tic;
        try
            pipeline.last.insertDB.visitMat.saveVisitHeaderMat(HdrPath, AH);
        catch ME
            perfLog(sprintf('%s saveMatHdr ERROR=%s dest=%s %s', FnName, ME.message, HdrPath, DataDir), toc(TicSaveHdr));
            CntBroken = CntBroken + 1;
            VisitStats = perfVisitEnd_(VisitStats, TicVisit, 0, 'broken', FnName, DataDir);
            continue
        end
        HdrInfo = dir(HdrPath);
        HdrSizeMB = HdrInfo.bytes / (1024^2);
        perfLog(sprintf('%s saveMatHdr dest=%s nHdr=%d sizeMB=%.2f InsertDbMatVersion=3 %s', ...
            FnName, HdrPath, numel(AH), HdrSizeMB, DataDir), toc(TicSaveHdr));
        CntSaved = CntSaved + 1;
        VisitStats = perfVisitEnd_(VisitStats, TicVisit, 0, 'saved', FnName, DataDir);
    end
    pipeline.last.insertDB.perfVisitStats('logSummary', VisitStats, FnName);
    perfLog(sprintf('%s SUMMARY saved=%d skipExists=%d noStatus=%d noData=%d broken=%d', ...
        FnName, CntSaved, CntSkippedExists, CntNoStatus, CntNoData, CntBroken));
    perfLog(sprintf('%s FINISH', FnName), toc(RunT0));
    Result = struct('Saved', CntSaved, 'SkipExists', CntSkippedExists, ...
        'NoStatus', CntNoStatus, 'NoData', CntNoData, 'Broken', CntBroken);
end

function [CatPath, HdrPath] = perfMatPaths_(DataDir, MatSubDir, VisitId)
    if ~perfZipVisitIdValid_(VisitId)
        error('writeVisitCatalogs2Mat_perf:InvalidVisitId', '%s', VisitId);
    end
    MatDir = fullfile(fileparts(DataDir), MatSubDir);
    CatPath = fullfile(MatDir, [VisitId '_Cat.mat']);
    HdrPath = fullfile(MatDir, [VisitId '_Hdr.mat']);
end

function VisitStats = perfVisitEnd_(VisitStats, TicVisit, VisitInsertSec, Outcome, FnName, DataDir)
    TotalSec = toc(TicVisit);
    fprintf(' ..visit done in %.1fs (log: insertDB_perf.log)\n', TotalSec);
    VisitStats = pipeline.last.insertDB.perfVisitStats('endVisit', VisitStats, TotalSec, VisitInsertSec, Outcome);
    perfLog(sprintf('%s visitEnd %s %s', FnName, Outcome, DataDir), TotalSec);
    pipeline.last.insertDB.perfLogBlank();
end

function perfLog(Message, Seconds)
    if nargin < 2
        pipeline.last.insertDB.perfLog(Message);
    else
        pipeline.last.insertDB.perfLog(Message, Seconds);
    end
end

function S = perfZipVisitStateInit_()
    S.Active = false;
    S.ExtractRoot = '';
    S.VisitId = '';
    S.ExtractDir = '';
end

function Root = perfZipExtractRoot_(ExtractRootArg)
    if ~isempty(ExtractRootArg)
        Root = ExtractRootArg;
        return
    end
    if ispc
        Root = 'C:/Temp';
    elseif isfolder('/dev/shm')
        Root = '/dev/shm';
    else
        Root = '/tmp';
    end
end

function VisitId = perfVisitIdFromDataDir_(DataDir)
    DataDir = strtrim(DataDir);
    DataDir = regexprep(DataDir, '[\\/]+$', '');
    [~, VisitId, ~] = fileparts(DataDir);
end

function Ok = perfZipVisitIdValid_(VisitId)
    Ok = ~isempty(VisitId) && ~contains(VisitId, '/') && ~contains(VisitId, '\') ...
        && ~contains(VisitId, '..');
end

function Ok = perfZipIsAllowedExtractDir_(ExtractDir, ExtractRoot, VisitId)
    if ~perfZipVisitIdValid_(VisitId) || isempty(ExtractRoot)
        Ok = false;
        return
    end
    Expected = fullfile(ExtractRoot, 'insertArchiveCatalogs2DB', VisitId);
    ParentOnly = fullfile(ExtractRoot, 'insertArchiveCatalogs2DB');
    ExtractDir = fullfile(ExtractDir);
    ExtractRoot = fullfile(ExtractRoot);
    Ok = strcmp(ExtractDir, Expected) && ~strcmp(ExtractDir, ExtractRoot) ...
        && ~strcmp(ExtractDir, ParentOnly);
end

function [Ok, ZipVisit, FailOutcome] = perfZipPrepareAndExtract_(FnName, DataDir, Dir, Args)
    ZipVisit = perfZipVisitStateInit_();
    FailOutcome = 'noZip';
    Ok = false;
    VisitId = perfVisitIdFromDataDir_(DataDir);
    if ~perfZipVisitIdValid_(VisitId)
        perfLog(sprintf('%s noZip invalidVisitId=%s %s', FnName, VisitId, DataDir));
        return
    end
    ExtractRoot = perfZipExtractRoot_(Args.ExtractRoot);
    if isempty(Args.ZipDir)
        ZipDir = fullfile(fileparts(DataDir), '_zip');
    else
        ZipDir = Args.ZipDir;
    end
    ZipPath = fullfile(ZipDir, [VisitId '.zip']);
    if ~isfile(ZipPath)
        perfLog(sprintf('%s noZip zip=%s %s', FnName, ZipPath, DataDir));
        return
    end
    ExtractDir = fullfile(ExtractRoot, 'insertArchiveCatalogs2DB', VisitId);
    if ~perfZipIsAllowedExtractDir_(ExtractDir, ExtractRoot, VisitId)
        perfLog(sprintf('%s noZip extractPathRejected dest=%s %s', FnName, ExtractDir, DataDir));
        return
    end
    cd(Dir);
    perfZipRmExtract_(FnName, struct('Active', true, 'ExtractRoot', ExtractRoot, ...
        'VisitId', VisitId, 'ExtractDir', ExtractDir), Dir);
    if ~isfolder(ExtractDir)
        mkdir(ExtractDir);
    end
    TicUnzip = tic;
    try
        unzip(ZipPath, ExtractDir);
    catch ME
        cd(Dir);
        perfZipRmExtract_(FnName, struct('Active', true, 'ExtractRoot', ExtractRoot, ...
            'VisitId', VisitId, 'ExtractDir', ExtractDir), Dir);
        perfLog(sprintf('%s unzipVisit ERROR=%s zip=%s dest=%s %s', ...
            FnName, ME.message, ZipPath, ExtractDir, DataDir), toc(TicUnzip));
        FailOutcome = 'broken';
        return
    end
    perfLog(sprintf('%s unzipVisit zip=%s dest=%s %s', FnName, ZipPath, ExtractDir, DataDir), toc(TicUnzip));
    cd(ExtractDir);
    ZipVisit.Active = true;
    ZipVisit.ExtractRoot = ExtractRoot;
    ZipVisit.VisitId = VisitId;
    ZipVisit.ExtractDir = ExtractDir;
    Ok = true;
    FailOutcome = '';
end

function perfZipRmExtract_(FnName, ZipVisit, Dir)
    if ~ZipVisit.Active
        return
    end
    ExtractDir = ZipVisit.ExtractDir;
    if ~perfZipIsAllowedExtractDir_(ExtractDir, ZipVisit.ExtractRoot, ZipVisit.VisitId)
        return
    end
    if ~isempty(Dir)
        cd(Dir);
    end
    if isfolder(ExtractDir)
        TicRm = tic;
        rmdir(ExtractDir, 's');
        perfLog(sprintf('%s rmExtract dest=%s', FnName, ExtractDir), toc(TicRm));
    end
end
