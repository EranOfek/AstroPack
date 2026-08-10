function [AI, Files] = loadVisitCatHdr(Args)
    % Load LAST pipeline files into lightweight AstroImage arrays (Cat + Header only)
    % Description: Lightweight loader that populates AstroImage.CatData and
    %              AstroImage.HeaderData only — no Image pixels, Mask, or PSF.
    %              Use this for diagnostics that need catalogs + headers but
    %              not pixel data (e.g. photometric calibration QA).
    %              For the full image products, see pipeline.last.load.loadVisit.
    %
    %              Two input modes:
    %              1. DataDir mode — single directory with all visits' proc files,
    %                 grouped by visit number parsed from LAST filenames.
    %              2. VisitDirs mode — string array of visit folder paths (or
    %                 .mat file containing such arrays). Each folder = one visit.
    %                 Supports proc and coadd file types.
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
    %            'FieldId'   - Keep only files whose AstroFileName.FieldID
    %                          (the token after '_clear_' in the LAST
    %                          filename, e.g. '1781' or '346+79') equals
    %                          this value. Char/string/numeric. Default
    %                          is '' (no filter). Mismatches dropped with
    %                          a Verbose-only message.
    %            'CropID'    - Keep only files whose AstroFileName.CropID
    %                          slot (3-digit zero-padded crop index in
    %                          the LAST filename, e.g. '014' for crop
    %                          14) matches. Integer scalar (1..Ncrop);
    %                          formatted to '%03d' internally for
    %                          comparison. Default [] (no filter).
    %                          Combined with FieldId via AND.
    %            'Verbose'   - Print progress. Default is true.
    % Output : - AI cell(Nvisits,1), each element a 1xNcrop AstroImage array
    %            with only CatData + HeaderData populated.
    %            Empty cells for visits with no matching files.
    % Author : D. Kovaleva (Mar 2026)
    % Example: % DataDir mode:
    %          AI = pipeline.last.load.loadVisitCatHdr('DataDir', '/data/222625v1');
    %
    %          % VisitDirs mode — from .mat file, coadd files:
    %          AI = pipeline.last.load.loadVisitCatHdr('ListFile', ...
    %               '/home/dana/N3_M2C4Jul2_7_list.mat', ...
    %               'ListFields', 'M2C4Jul2p1', 'FileType', 'coadd');
    %
    %          % VisitDirs mode — all fields, first 10 folders:
    %          AI = pipeline.last.load.loadVisitCatHdr('ListFile', ...
    %               '/home/dana/N3_M2C4Jul2_7_list.mat', ...
    %               'VisitIdx', 1:10, 'FileType', 'coadd');
    %
    %          % VisitDirs mode — explicit folder list:
    %          AI = pipeline.last.load.loadVisitCatHdr('VisitDirs', ...
    %               ["/path/to/visit1", "/path/to/visit2"], 'FileType', 'coadd');
    %
    %          % Restrict to a specific observation field (token after
    %          % '_clear_' in the LAST filename, e.g. '1781' or '346+79'):
    %          AI = pipeline.last.load.loadVisitCatHdr('DataDir', '/data/222625v1', ...
    %                  'FieldId', '1781');

    arguments
        Args.DataDir     = ''
        Args.Visits      = 1:20
        Args.VisitDirs   = []       % string array of visit folder paths
        Args.ListFile    = ''       % .mat file with visit folder lists
        Args.ListFields  = {}       % field name(s) to read from ListFile
        Args.VisitIdx    = []       % indices into folder list
        Args.FileType    = 'proc'   % 'proc' | 'coadd'
        Args.CatHDU      = []       % HDU N of the Cat FITS to ADDITIONALLY read
        % Non-empty scalar (typical: 3 for LAST calibrated coadds) makes
        % buildAIfromFiles append every Cat[CatHDU] header entry to the
        % existing Image HDU 1 header. Motivation: PT_* photometric-model
        % keywords stamped by PhotCalibTrans.calibrate live on Cat HDU 3,
        % not on the Image; without this the getVal/getStructKey calls
        % downstream (extractHeaderData, plotHeaderScatter, ...) can't see
        % PT_NORM, PT_RMS, PT_ARMS, PT_CHI2, PT_DOF, PT_NCALI, PT_P_V*, etc.
        % Empty (default) keeps the historical behaviour (Image HDU 1 only).
        Args.FieldId     = ''       % keep only files whose AstroFileName.FieldID matches
        Args.CropID      double {mustBeInteger, mustBeNonnegative} = []   % integer 1..Ncrop; [] = no filter
        Args.Verbose logical = true
    end

    % Resolve visit directories
    Dirs = resolveVisitDirs(Args);

    if isempty(Dirs)
        % Fall back to DataDir mode
        [AI, Files] = loadFromDataDir(Args);
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

    AI    = cell(Nvisits, 1);
    Files = cell(Nvisits, 1);
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

        [CatFiles, ImFiles] = filterFieldIdCropID(CatFiles, ImFiles, ...
            Args.FieldId, Args.CropID, Args.Verbose, sprintf('Epoch %d', Iv));

        if isempty(CatFiles)
            if Args.Verbose
                fprintf('  Epoch %d: no %s Cat files in %s\n', Iv, Args.FileType, D);
            end
            continue;
        end

        AI{Iv}    = buildAIfromFiles(CatFiles, ImFiles, Args.CatHDU);
        Files{Iv} = CatFiles;

        % Clean up temp files
        if ~isempty(TmpDir)
            rmdir(TmpDir, 's');
        end

        if Args.Verbose
            fprintf('  Epoch %d: %d crops from %s\n', Iv, numel(CatFiles), D);
        end
    end
end

% =========================================================================
function AIv = buildAIfromFiles(CatFiles, ImFiles, CatHDU)
    % Batched AstroCatalog + AstroHeader loader — much faster than per-file loop.
    % CatHDU (optional, default []): when non-empty, ALSO read the Cat
    % file's CatHDU header and append its rows to AIv(Ic).HeaderData.Data
    % so keys stamped there (PT_* on LAST coadd HDU 3) are visible to
    % downstream getVal / getStructKey callers.
    if nargin < 3; CatHDU = []; end
    Ncf = numel(CatFiles);
    AIv = AstroImage([1, Ncf]);
    try
        Cats = AstroCatalog(CatFiles);
    catch
        Cats = [];
    end
    try
        Heads = AstroHeader(ImFiles, 1);
    catch
        Heads = [];
    end
    CatHeads = [];
    if ~isempty(CatHDU)
        try
            CatHeads = AstroHeader(CatFiles, CatHDU);
        catch
            CatHeads = [];
        end
    end
    for Ic = 1:Ncf
        if ~isempty(Cats) && Ic <= numel(Cats)
            AIv(Ic).CatData = Cats(Ic);
        end
        if ~isempty(Heads) && Ic <= numel(Heads)
            AIv(Ic).HeaderData = Heads(Ic);
        end
        if ~isempty(CatHeads) && Ic <= numel(CatHeads) ...
                && ~isempty(CatHeads(Ic).Data)
            % Merge Cat HDU keys AFTER Image HDU 1 so any duplicate keys
            % from the Cat header win (last write - matches how AstroHeader
            % getVal itself resolves duplicates).
            if isempty(AIv(Ic).HeaderData)
                AIv(Ic).HeaderData = CatHeads(Ic);
            else
                AIv(Ic).HeaderData.Data = ...
                    [AIv(Ic).HeaderData.Data; CatHeads(Ic).Data];
            end
        end
    end
end


% =========================================================================
function Stems = i_visitStemFromFiles(Files)
    % Extract the visit stem (everything up to '_<crop>_sci_coadd_(Cat|Image)_<N>')
    % from each LAST filename. Files that don't match the convention get
    % their basename as the stem (grouped alone).
    N = numel(Files);
    Stems = cell(N, 1);
    for I = 1:N
        [~, Name, ~] = fileparts(Files{I});
        Tok = regexp(Name, '^(.+?)_(\d+)_sci_coadd_(?:Image|Cat)_\d+$', 'tokens', 'once');
        if isempty(Tok); Stems{I} = Name; else; Stems{I} = Tok{1}; end
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
function [AI, Files] = loadFromDataDir(Args)
    % Original DataDir mode — single directory, visits by filename token
    if isempty(Args.DataDir)
        AI    = {};
        Files = {};
        return;
    end

    CatPattern = sprintf('*_sci_%s_Cat_1.fits', Args.FileType);
    ImPattern  = sprintf('*_sci_%s_Image_1.fits', Args.FileType);

    AllCatFiles = io.files.filelist(fullfile(Args.DataDir, CatPattern));
    AllImFiles  = io.files.filelist(fullfile(Args.DataDir, ImPattern));

    [AllCatFiles, AllImFiles] = filterFieldIdCropID(AllCatFiles, AllImFiles, ...
        Args.FieldId, Args.CropID, Args.Verbose, 'DataDir');

    % Coadd: DataDir may hold many visits' output flat in one directory
    % (batchPhotCalibTrans joint per-crop write dumps all 4800 files into
    % one OutDir). Group by visit stem parsed out of the LAST filename
    % convention '<stem>_<crop>_sci_coadd_(Cat|Image)_<N>.fits'. Each
    % unique stem = one visit; each visit contributes its own 1 x Ncrop
    % AstroImage array in the returned cell.
    if strcmpi(Args.FileType, 'coadd')
        if isempty(AllCatFiles)
            if Args.Verbose
                fprintf('Loading coadd from %s: no Cat files found\n', Args.DataDir);
            end
            AI    = cell(0, 1);
            Files = cell(0, 1);
            return;
        end
        Stems = i_visitStemFromFiles(AllCatFiles);
        [UStems, ~, IUs] = unique(Stems);
        Nvisits = numel(UStems);
        if Args.Verbose
            fprintf('Loading coadd from %s: %d visits x <=Ncrop files each\n', ...
                    Args.DataDir, Nvisits);
        end
        AI    = cell(Nvisits, 1);
        Files = cell(Nvisits, 1);
        for Iv = 1:Nvisits
            Sel  = find(IUs == Iv);
            VCat = AllCatFiles(Sel);
            % Pair each Cat with its sibling Image (swap Cat_ -> Image_)
            VIm  = cellfun(@(p) strrep(p, '_Cat_', '_Image_'), VCat, 'UniformOutput', false);
            VIm  = VIm(cellfun(@isfile, VIm));   % drop siblings that don't exist
            AI{Iv}    = buildAIfromFiles(VCat, VIm, Args.CatHDU);
            Files{Iv} = VCat;
            if Args.Verbose
                fprintf('  visit %4d/%d: %s  Ncrop=%d\n', ...
                        Iv, Nvisits, UStems{Iv}, numel(VCat));
            end
        end
        return;
    end

    Nvisits = numel(Args.Visits);
    if Args.Verbose
        fprintf('Loading %d visits from %s\n', Nvisits, Args.DataDir);
    end

    AI    = cell(Nvisits, 1);
    Files = cell(Nvisits, 1);

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

        AI{Iv}    = buildAIfromFiles(CatFiles, ImFiles, Args.CatHDU);
        Files{Iv} = CatFiles;

        if Args.Verbose
            fprintf('  Visit %s: %d crops\n', VStr, numel(CatFiles));
        end
    end
end

% =========================================================================
function [CatFiles, ImFiles] = filterFieldIdCropID(CatFiles, ImFiles, FieldId, CropID, Verbose, Tag)
    % Drop files whose AstroFileName.FieldID and/or CropID do not match.
    % '' / [] for either argument disables that filter; both can be on.
    if isempty(FieldId) && isempty(CropID); return; end
    NCBefore = numel(CatFiles);  NIBefore = numel(ImFiles);
    [CatFiles] = filterOne(CatFiles, FieldId, CropID);
    [ImFiles]  = filterOne(ImFiles,  FieldId, CropID);
    if Verbose
        NCDrop = NCBefore - numel(CatFiles);
        NIDrop = NIBefore - numel(ImFiles);
        if NCDrop > 0 || NIDrop > 0
            fprintf('  %s: FieldId=%s CropID=%s filter dropped %d/%d Cat, %d/%d Im\n', ...
                Tag, char(string(FieldId)), char(string(CropID)), ...
                NCDrop, NCBefore, NIDrop, NIBefore);
        end
    end
end

function Files = filterOne(Files, FieldId, CropID)
    if isempty(Files); return; end
    AFN = AstroFileName(Files);
    Keep = true(numel(Files), 1);
    if ~isempty(FieldId)
        Keep = Keep & strcmp(string(AFN.FieldID), string(FieldId));
    end
    if ~isempty(CropID)
        Keep = Keep & strcmp(string(AFN.CropID), sprintf('%03d', CropID));
    end
    Files = Files(Keep);
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
