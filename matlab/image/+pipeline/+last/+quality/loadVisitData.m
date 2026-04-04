function AI = loadVisitData(Args)
    % Load LAST pipeline FITS files into AstroImage arrays per visit
    % Description: Two input modes:
    %   1. DataDir mode — single directory with all visits' proc files,
    %      grouped by visit number parsed from LAST filenames.
    %   2. VisitDirs mode — string array of visit folder paths (or .mat file
    %      containing such arrays). Each folder = one visit. Supports proc
    %      and coadd file types.
    %
    % Input  : * ...,key,val,...
    %   --- DataDir mode (original) ---
    %            'DataDir' - Directory with proc FITS files. Default is ''.
    %            'Visits'  - Vector of visit indices to load. Default is 1:20.
    %   --- VisitDirs mode ---
    %            'VisitDirs' - String array of visit folder paths. Default is [].
    %                          When non-empty, overrides DataDir mode.
    %            'ListFile'  - Path to .mat file containing visit folder lists.
    %                          Default is ''.
    %            'ListFields'- Field name(s) to read from ListFile. String or
    %                          cell array. Default is {} (all fields, concatenated).
    %            'VisitIdx'  - Indices into the folder list to load. Default is
    %                          [] (all folders).
    %   --- Common ---
    %            'FileType'  - 'proc' or 'coadd'. Default is 'proc'.
    %            'Verbose'   - Print progress. Default is true.
    % Output : - AI cell(Nvisits,1), each element a 1xNcrop AstroImage array.
    %            Empty cells for visits with no matching files.
    % Author : D. Kovaleva (Mar 2026)
    % Example: % DataDir mode (original):
    %          AI = pipeline.last.quality.loadVisitData('DataDir', '/data/222625v1');
    %
    %          % VisitDirs mode — from .mat file, coadd files:
    %          AI = pipeline.last.quality.loadVisitData('ListFile', ...
    %               '/home/dana/N3_M2C4Jul2_7_list.mat', ...
    %               'ListFields', 'M2C4Jul2p1', 'FileType', 'coadd');
    %
    %          % VisitDirs mode — all fields, first 10 folders:
    %          AI = pipeline.last.quality.loadVisitData('ListFile', ...
    %               '/home/dana/N3_M2C4Jul2_7_list.mat', ...
    %               'VisitIdx', 1:10, 'FileType', 'coadd');
    %
    %          % VisitDirs mode — explicit folder list:
    %          AI = pipeline.last.quality.loadVisitData('VisitDirs', ...
    %               ["/path/to/visit1", "/path/to/visit2"], 'FileType', 'coadd');

    arguments
        Args.DataDir     = ''
        Args.Visits      = 1:20
        Args.VisitDirs   = []       % string array of visit folder paths
        Args.ListFile    = ''       % .mat file with visit folder lists
        Args.ListFields  = {}       % field name(s) to read from ListFile
        Args.VisitIdx    = []       % indices into folder list
        Args.FileType    = 'proc'   % 'proc' | 'coadd'
        Args.Verbose logical = true
    end

    % Resolve visit directories
    Dirs = resolveVisitDirs(Args);

    if isempty(Dirs)
        % Fall back to DataDir mode
        AI = loadFromDataDir(Args);
        return;
    end

    % VisitDirs mode — each directory is one epoch
    Nvisits = numel(Dirs);
    if Args.Verbose
        fprintf('Loading %d epochs (%s files)\n', Nvisits, Args.FileType);
    end

    CatPattern = sprintf('*_sci_%s_Cat_1.fits', Args.FileType);
    ImPattern  = sprintf('*_sci_%s_Image_1.fits', Args.FileType);
    CatPatternBz2 = [CatPattern '.bz2'];
    ImPatternBz2  = [ImPattern '.bz2'];

    AI = cell(Nvisits, 1);
    for Iv = 1:Nvisits
        D = char(Dirs(Iv));
        if ~exist(D, 'dir')
            if Args.Verbose
                fprintf('  Epoch %d: directory not found, skipping: %s\n', Iv, D);
            end
            continue;
        end

        CatFiles = io.files.filelist(fullfile(D, CatPattern));
        ImFiles  = io.files.filelist(fullfile(D, ImPattern));

        % Decompress .bz2 to local temp if no uncompressed files found
        TmpDir = '';
        if isempty(CatFiles)
            CatBz2 = io.files.filelist(fullfile(D, CatPatternBz2));
            ImBz2  = io.files.filelist(fullfile(D, ImPatternBz2));
            if ~isempty(CatBz2)
                TmpDir = fullfile(tempdir, sprintf('lastQual_%05d', Iv));
                if ~exist(TmpDir, 'dir'); mkdir(TmpDir); end
                if Args.Verbose
                    fprintf('  Epoch %d: decompressing %d bz2 files to %s\n', ...
                        Iv, numel(CatBz2) + numel(ImBz2), TmpDir);
                end
                CatFiles = decompressBz2(CatBz2, TmpDir);
                ImFiles  = decompressBz2(ImBz2, TmpDir);
            end
        end

        if isempty(CatFiles)
            if Args.Verbose
                fprintf('  Epoch %d: no %s Cat files in %s\n', Iv, Args.FileType, D);
            end
            continue;
        end

        Ncf = numel(CatFiles);
        AIv = AstroImage([1, Ncf]);
        for Ic = 1:Ncf
            AIv(Ic).CatData = AstroCatalog(CatFiles{Ic});
            if Ic <= numel(ImFiles)
                AIv(Ic).HeaderData = AstroHeader(ImFiles{Ic}, 1);
            end
        end
        AI{Iv} = AIv;

        % Clean up temp files
        if ~isempty(TmpDir)
            rmdir(TmpDir, 's');
        end

        if Args.Verbose
            fprintf('  Epoch %d: %d crops from %s\n', Iv, Ncf, D);
        end
    end
end

% =========================================================================
function Dirs = resolveVisitDirs(Args)
    % Build string array of visit directories from VisitDirs/ListFile/ListFields
    Dirs = [];

    % Explicit VisitDirs takes priority
    if ~isempty(Args.VisitDirs)
        Dirs = string(Args.VisitDirs(:));
    elseif ~isempty(Args.ListFile)
        S = load(Args.ListFile);
        Fields = Args.ListFields;
        if isempty(Fields)
            Fields = fieldnames(S);
        elseif ischar(Fields) || isstring(Fields)
            Fields = cellstr(Fields);
        end
        AllDirs = [];
        for If = 1:numel(Fields)
            AllDirs = [AllDirs; string(S.(Fields{If})(:))];
        end
        Dirs = AllDirs;
    end

    % Apply VisitIdx filter
    if ~isempty(Dirs) && ~isempty(Args.VisitIdx)
        Dirs = Dirs(Args.VisitIdx);
    end
end

% =========================================================================
function AI = loadFromDataDir(Args)
    % Original DataDir mode — single directory, visits by filename token
    if isempty(Args.DataDir)
        AI = {};
        return;
    end

    Nvisits = numel(Args.Visits);
    if Args.Verbose
        fprintf('Loading %d visits from %s\n', Nvisits, Args.DataDir);
    end

    AI = cell(Nvisits, 1);

    AllCatFiles = io.files.filelist(fullfile(Args.DataDir, '*_sci_proc_Cat_1.fits'));
    AllImFiles  = io.files.filelist(fullfile(Args.DataDir, '*_sci_proc_Image_1.fits'));

    for Iv = 1:Nvisits
        VisitNum = Args.Visits(Iv);
        VStr = sprintf('%03d', VisitNum);

        CatKeep = false(numel(AllCatFiles), 1);
        for If = 1:numel(AllCatFiles)
            [~, Name] = fileparts(AllCatFiles{If});
            Tokens = strsplit(Name, '_');
            if numel(Tokens) >= 7
                CatKeep(If) = str2double(Tokens{end-6}) == VisitNum;
            end
        end
        ImKeep = false(numel(AllImFiles), 1);
        for If = 1:numel(AllImFiles)
            [~, Name] = fileparts(AllImFiles{If});
            Tokens = strsplit(Name, '_');
            if numel(Tokens) >= 7
                ImKeep(If) = str2double(Tokens{end-6}) == VisitNum;
            end
        end
        CatFiles = AllCatFiles(CatKeep);
        ImFiles  = AllImFiles(ImKeep);

        if isempty(CatFiles)
            if Args.Verbose
                fprintf('  Visit %s: no files, skipping\n', VStr);
            end
            continue;
        end

        Ncf = numel(CatFiles);
        AIv = AstroImage([1, Ncf]);
        for Ic = 1:Ncf
            AIv(Ic).CatData = AstroCatalog(CatFiles{Ic});
            if Ic <= numel(ImFiles)
                AIv(Ic).HeaderData = AstroHeader(ImFiles{Ic}, 1);
            end
        end
        AI{Iv} = AIv;

        if Args.Verbose
            fprintf('  Visit %s: %d crops\n', VStr, Ncf);
        end
    end
end

% =========================================================================
function OutFiles = decompressBz2(Bz2Files, TmpDir)
    % Decompress .bz2 files to local temp directory
    % Input  : - Cell array of .bz2 file paths
    %          - Target directory (local, writable)
    % Output : - Cell array of decompressed file paths
    OutFiles = cell(size(Bz2Files));
    for Ib = 1:numel(Bz2Files)
        [~, Name] = fileparts(Bz2Files{Ib});  % strips .bz2, keeps .fits
        OutFile = fullfile(TmpDir, Name);
        [Status, ~] = system(sprintf('bunzip2 -k -c "%s" > "%s"', Bz2Files{Ib}, OutFile));
        if Status == 0
            OutFiles{Ib} = OutFile;
        end
    end
    OutFiles = OutFiles(~cellfun(@isempty, OutFiles));
end
