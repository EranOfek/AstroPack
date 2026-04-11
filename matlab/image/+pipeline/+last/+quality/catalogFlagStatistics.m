function Report = catalogFlagStatistics(DataPath, Args)
    % Per-bit source statistics over a set of FITS catalog files
    % Description: Recursively scans a directory tree for catalog FITS files
    %   matching FileType (e.g. 'sci_coadd'), reads each catalog via
    %   AstroCatalog with the MEX-accelerated path, and for each bit
    %   (either a user-selected subset, or all bits in the dictionary)
    %   counts the number and fraction of sources that have that bit set
    %   in the FLAGS column. Reports per-file (long form) and aggregated
    %   statistics.
    %
    %   Bit indices are resolved by name via BitDictionary, so no bit index
    %   is hardcoded. This mirrors pipeline.last.quality.maskBitStatistics
    %   at the source (catalog) level rather than the pixel (mask) level.
    %
    % Input  : - Path to directory containing catalog FITS files. The
    %            directory is scanned recursively (subdirs included).
    %          * ...,key,val,...
    %            'FileType'     - File type pattern. Default is 'sci_coadd'.
    %                             Used to glob '*<FileType>_Cat_*.fits'.
    %            'BitNames'     - Cell array of bit names to compute statistics
    %                             for. If empty, uses all bits in the
    %                             dictionary. Default is {}.
    %            'BitDictName'  - BitDictionary name for bit name lookups.
    %                             Default is 'BitMask.Image.Default'.
    %            'ColFlags'     - FLAGS column name. Default is 'FLAGS'.
    %            'ColX'         - X coordinate column name. Default is 'XPEAK'.
    %            'ColY'         - Y coordinate column name. Default is 'YPEAK'.
    %            'KeepFlaggedSourcesXY' - If true, populate Report.FlaggedSources
    %                             with one row per flagged source per bit
    %                             (FileKey, BitName, BitIndex, X, Y, FlagsValue).
    %                             Default is true.
    %            'KeepAllSourcesXY' - If true, populate Report.AllSources with
    %                             one row per source (FileKey, X, Y, FlagsValue).
    %                             Needed for fraction-density plots.
    %                             Default is true.
    %            'Recursive'    - Recurse into subdirectories. Default is true.
    %            'ReportFile'   - Optional file path to save report (.mat).
    %                             Default is '' (no save).
    %            'Verbose'      - Print progress to console. Default is false.
    % Output : - Struct with fields:
    %            .Timestamp     - datetime of the run
    %            .DataPath      - path scanned
    %            .FileType      - file type pattern used
    %            .BitDictName   - bit dictionary name
    %            .BitDict       - table {BitName, BitIndex, BitValue} for the
    %                             bits actually checked
    %            .NumFiles      - number of catalog files processed
    %            .PerFile       - long-form table with one row per (file, bit):
    %                             FileKey, BitName, BitIndex, NumSources,
    %                             NumSourcesWithBit, FractionSourcesWithBit
    %            .FlaggedSources- (if KeepFlaggedSourcesXY) long-form table with
    %                             one row per flagged source per bit:
    %                             FileKey, BitName, BitIndex, X, Y, FlagsValue
    %            .AllSources    - (if KeepAllSourcesXY) long-form table with
    %                             one row per source (across all files):
    %                             FileKey, X, Y, FlagsValue
    %            .Aggregate     - per-bit aggregate table:
    %                             BitName, BitIndex, NumFiles, TotalSources,
    %                             TotalSourcesWithBit, OverallFraction,
    %                             MeanFraction, MedianFraction, MaxFraction
    %            .Summary       - text summary
    %
    % Author : Dana Kovaleva (Apr 2026)
    % Example: DataPath = '/bigdata2/projects/last/testNewPipe/222625v3';
    %          % Single bit, default settings (collects FlaggedSources +
    %          % AllSources for downstream plotting):
    %          R = pipeline.last.quality.catalogFlagStatistics(DataPath, ...
    %                  'BitNames', {'CoaddLessImages'});
    %          % All bits in the dictionary, no per-source data (light):
    %          R = pipeline.last.quality.catalogFlagStatistics(DataPath, ...
    %                  'KeepFlaggedSourcesXY', false, ...
    %                  'KeepAllSourcesXY',     false);
    %          % Plot the spatial fraction of flagged sources per bin:
    %          pipeline.last.quality.plotCatalogFlagStatistics(R, 'Mode','fraction');

    arguments
        DataPath                     (1,:) char
        Args.FileType                (1,:) char  = 'sci_coadd'
        Args.BitNames                cell        = {}
        Args.BitDictName             (1,:) char  = 'BitMask.Image.Default'
        Args.ColFlags                (1,:) char  = 'FLAGS'
        Args.ColX                    (1,:) char  = 'XPEAK'
        Args.ColY                    (1,:) char  = 'YPEAK'
        Args.KeepFlaggedSourcesXY    (1,1) logical = true
        Args.KeepAllSourcesXY        (1,1) logical = true
        Args.Recursive               (1,1) logical = true
        Args.ReportFile              (1,:) char  = ''
        Args.Verbose                 (1,1) logical = false
    end

    % --- Resolve bit names/indices from the dictionary ------------------
    BD = BitDictionary(Args.BitDictName);

    if isempty(Args.BitNames)
        BitNames = BD.Dic.BitName(:);
        BitInds  = double(BD.Dic.BitInd(:));
    else
        BitNames = cellstr(Args.BitNames(:));
        [BitInds, ~] = BD.name2bit(BitNames);
        BitInds = double(BitInds(:));
        if any(isnan(BitInds))
            Missing = BitNames(isnan(BitInds));
            error('Bit name(s) not found in dictionary %s: %s', ...
                  Args.BitDictName, strjoin(Missing, ', '));
        end
    end

    Nbits     = numel(BitInds);
    BitValues = bitshift(uint32(1), uint32(BitInds));   % 2^BitInd as uint32

    BitDictTable = table(BitNames, BitInds, double(BitValues), ...
        'VariableNames', {'BitName', 'BitIndex', 'BitValue'});

    if Args.Verbose
        fprintf('Data path: %s\n', DataPath);
        fprintf('File type: %s\n', Args.FileType);
        fprintf('Checking %d bit(s) from dictionary %s\n', Nbits, Args.BitDictName);
    end

    % --- Find catalog files --------------------------------------------
    Pattern = ['*' Args.FileType '_Cat_*.fits'];
    if Args.Recursive
        Dc = dir(fullfile(DataPath, '**', Pattern));
    else
        Dc = dir(fullfile(DataPath, Pattern));
    end

    if isempty(Dc)
        error('No catalog files found matching %s under %s', Pattern, DataPath);
    end

    Nfiles = numel(Dc);
    if Args.Verbose
        fprintf('Found %d catalog files\n\n', Nfiles);
    end

    % --- File key regex (shared convention with checkMaskPropagationToCoadd) ---
    FileKeyPattern = '_clear_(.+)_sci_';

    % --- Pre-allocate per-file long-form storage ------------------------
    Nrows = Nfiles * Nbits;
    FileKeyCol       = strings(Nrows, 1);
    BitNameCol       = strings(Nrows, 1);
    BitIndexCol      = zeros(Nrows, 1);
    NumSourcesCol    = zeros(Nrows, 1);
    NumBitSourcesCol = zeros(Nrows, 1);
    FracBitSourcesCol= zeros(Nrows, 1);

    Row = 0;

    % Per-(file, bit) accumulators of flagged-source coordinates.
    % Stored as cell-of-cells then concatenated at the end. Avoids growing
    % a large table inside the loop.
    if Args.KeepFlaggedSourcesXY
        FsFileKey  = cell(Nfiles * Nbits, 1);
        FsBitName  = cell(Nfiles * Nbits, 1);
        FsBitIndex = cell(Nfiles * Nbits, 1);
        FsX        = cell(Nfiles * Nbits, 1);
        FsY        = cell(Nfiles * Nbits, 1);
        FsFlagsVal = cell(Nfiles * Nbits, 1);
        FsCell     = 0;
    end

    % Per-file accumulators of ALL source coordinates (one row per source).
    if Args.KeepAllSourcesXY
        AsFileKey  = cell(Nfiles, 1);
        AsX        = cell(Nfiles, 1);
        AsY        = cell(Nfiles, 1);
        AsFlagsVal = cell(Nfiles, 1);
        AsCell     = 0;
    end

    NeedXY = Args.KeepFlaggedSourcesXY || Args.KeepAllSourcesXY;

    % --- Main loop over catalog files ----------------------------------
    for Ifile = 1:Nfiles
        FileName = fullfile(Dc(Ifile).folder, Dc(Ifile).name);

        % Read catalog via AstroCatalog with MEX-accelerated path
        try
              AC = AstroCatalog(FileName, 'UseMex', true);
 %            AC = AstroCatalog(FileName);
        catch ME
            warning('Failed to read %s: %s', Dc(Ifile).name, ME.message);
            continue;
        end

        Flags = AC.getCol(Args.ColFlags);
        if isempty(Flags)
            warning('Column %s not found in %s', Args.ColFlags, Dc(Ifile).name);
            continue;
        end
        Flags = uint32(Flags);
        NumSources = numel(Flags);

        % Coordinates (needed when collecting flagged- or all-source XY)
        if NeedXY
            Xcol = AC.getCol(Args.ColX);
            Ycol = AC.getCol(Args.ColY);
            HaveXY = ~isempty(Xcol) && ~isempty(Ycol) && ...
                     numel(Xcol) == NumSources && numel(Ycol) == NumSources;
            if ~HaveXY
                warning('X/Y columns (%s,%s) missing or size-mismatched in %s', ...
                        Args.ColX, Args.ColY, Dc(Ifile).name);
            end
        else
            HaveXY = false;
        end

        Tokens = regexp(Dc(Ifile).name, FileKeyPattern, 'tokens');
        if ~isempty(Tokens)
            FileKey = Tokens{1}{1};
        else
            FileKey = Dc(Ifile).name;
        end

        if Args.Verbose
            fprintf('[%d/%d] %s (%d sources)\n', Ifile, Nfiles, Dc(Ifile).name, NumSources);
        end

        % Collect ALL source positions for this file (one row per source)
        if Args.KeepAllSourcesXY && HaveXY && NumSources > 0
            AsCell = AsCell + 1;
            AsFileKey{AsCell}  = repmat(string(FileKey), NumSources, 1);
            AsX{AsCell}        = double(Xcol);
            AsY{AsCell}        = double(Ycol);
            AsFlagsVal{AsCell} = double(Flags);
        end

        for Ib = 1:Nbits
            if NumSources > 0
                BitMask    = bitand(Flags, BitValues(Ib));
                FlaggedSel = BitMask > 0;
                NumBitSrc  = nnz(FlaggedSel);
                FracBitSrc = NumBitSrc / NumSources;
            else
                FlaggedSel = false(0,1);
                NumBitSrc  = 0;
 Rmask = pipeline.last.quality.maskBitStatistics(DataPath, ...
'BitNames', {'CoaddLessImages'}, ...
'AccumulateMaps', true);
pipeline.last.quality.plotMaskBitStatistics(Rmask);                FracBitSrc = 0;
            end

            Row = Row + 1;
            FileKeyCol(Row)        = string(FileKey);
            BitNameCol(Row)        = string(BitNames{Ib});
            BitIndexCol(Row)       = BitInds(Ib);
            NumSourcesCol(Row)     = NumSources;
            NumBitSourcesCol(Row)  = NumBitSrc;
            FracBitSourcesCol(Row) = FracBitSrc;

            % Collect XY of flagged sources for this (file, bit)
            if Args.KeepFlaggedSourcesXY && HaveXY && NumBitSrc > 0
                FsCell = FsCell + 1;
                FsFileKey{FsCell}  = repmat(string(FileKey), NumBitSrc, 1);
                FsBitName{FsCell}  = repmat(string(BitNames{Ib}), NumBitSrc, 1);
                FsBitIndex{FsCell} = repmat(BitInds(Ib), NumBitSrc, 1);
                FsX{FsCell}        = double(Xcol(FlaggedSel));
                FsY{FsCell}        = double(Ycol(FlaggedSel));
                FsFlagsVal{FsCell} = double(Flags(FlaggedSel));
            end
        end
    end

    % Trim unused pre-allocated rows
    FileKeyCol        = FileKeyCol(1:Row);
    BitNameCol        = BitNameCol(1:Row);
    BitIndexCol       = BitIndexCol(1:Row);
    NumSourcesCol     = NumSourcesCol(1:Row);
    NumBitSourcesCol  = NumBitSourcesCol(1:Row);
    FracBitSourcesCol = FracBitSourcesCol(1:Row);

    PerFile = table(FileKeyCol, BitNameCol, BitIndexCol, ...
                    NumSourcesCol, NumBitSourcesCol, FracBitSourcesCol, ...
        'VariableNames', {'FileKey', 'BitName', 'BitIndex', ...
                          'NumSources', 'NumSourcesWithBit', 'FractionSourcesWithBit'});

    % --- Build FlaggedSources long-form table --------------------------
    if Args.KeepFlaggedSourcesXY && FsCell > 0
        FlaggedSources = table( ...
            vertcat(FsFileKey{1:FsCell}), ...
            vertcat(FsBitName{1:FsCell}), ...
            vertcat(FsBitIndex{1:FsCell}), ...
            vertcat(FsX{1:FsCell}), ...
            vertcat(FsY{1:FsCell}), ...
            vertcat(FsFlagsVal{1:FsCell}), ...
            'VariableNames', {'FileKey', 'BitName', 'BitIndex', ...
                              'X', 'Y', 'FlagsValue'});
    else
        FlaggedSources = table('Size', [0, 6], ...
            'VariableTypes', {'string','string','double','double','double','double'}, ...
            'VariableNames', {'FileKey','BitName','BitIndex','X','Y','FlagsValue'});
    end

    % --- Build AllSources long-form table ------------------------------
    if Args.KeepAllSourcesXY && AsCell > 0
        AllSources = table( ...
            vertcat(AsFileKey{1:AsCell}), ...
            vertcat(AsX{1:AsCell}), ...
            vertcat(AsY{1:AsCell}), ...
            vertcat(AsFlagsVal{1:AsCell}), ...
            'VariableNames', {'FileKey', 'X', 'Y', 'FlagsValue'});
    else
        AllSources = table('Size', [0, 4], ...
            'VariableTypes', {'string','double','double','double'}, ...
            'VariableNames', {'FileKey','X','Y','FlagsValue'});
    end

    % --- Aggregate per bit ---------------------------------------------
    AggBitName         = strings(Nbits, 1);
    AggBitIndex        = zeros(Nbits, 1);
    AggNumFiles        = zeros(Nbits, 1);
    AggTotalSources    = zeros(Nbits, 1);
    AggTotalBitSources = zeros(Nbits, 1);
    AggOverallFraction = zeros(Nbits, 1);
    AggMeanFraction    = zeros(Nbits, 1);
    AggMedianFraction  = zeros(Nbits, 1);
    AggMaxFraction     = zeros(Nbits, 1);

    for Ib = 1:Nbits
        Sel = PerFile.BitIndex == BitInds(Ib);
        SubFrac  = PerFile.FractionSourcesWithBit(Sel);
        SubNsrc  = PerFile.NumSources(Sel);
        SubBitSr = PerFile.NumSourcesWithBit(Sel);

        AggBitName(Ib)         = string(BitNames{Ib});
        AggBitIndex(Ib)        = BitInds(Ib);
        AggNumFiles(Ib)        = numel(SubFrac);
        AggTotalSources(Ib)    = sum(SubNsrc);
        AggTotalBitSources(Ib) = sum(SubBitSr);

        if AggTotalSources(Ib) > 0
            AggOverallFraction(Ib) = AggTotalBitSources(Ib) / AggTotalSources(Ib);
        end
        if ~isempty(SubFrac)
            AggMeanFraction(Ib)   = mean(SubFrac);
            AggMedianFraction(Ib) = median(SubFrac);
            AggMaxFraction(Ib)    = max(SubFrac);
        end
    end

    Aggregate = table(AggBitName, AggBitIndex, AggNumFiles, ...
                      AggTotalSources, AggTotalBitSources, ...
                      AggOverallFraction, AggMeanFraction, AggMedianFraction, AggMaxFraction, ...
        'VariableNames', {'BitName', 'BitIndex', 'NumFiles', ...
                          'TotalSources', 'TotalSourcesWithBit', ...
                          'OverallFraction', 'MeanFraction', ...
                          'MedianFraction', 'MaxFraction'});

    % --- Summary --------------------------------------------------------
    Summary = sprintf(['Catalog FLAGS Statistics Report\n', ...
                       '===============================\n', ...
                       'Data path     : %s\n', ...
                       'File type     : %s\n', ...
                       'Flags column  : %s\n', ...
                       'Bit dictionary: %s\n', ...
                       'Files scanned : %d\n', ...
                       'Bits analyzed : %d\n'], ...
                       DataPath, Args.FileType, Args.ColFlags, ...
                       Args.BitDictName, Nfiles, Nbits);

    % --- Assemble report -----------------------------------------------
    Report = struct();
    Report.Timestamp   = datetime('now');
    Report.DataPath    = DataPath;
    Report.FileType    = Args.FileType;
    Report.BitDictName = Args.BitDictName;
    Report.BitDict     = BitDictTable;
    Report.NumFiles    = Nfiles;
    Report.PerFile     = PerFile;
    Report.FlaggedSources = FlaggedSources;
    Report.AllSources  = AllSources;
    Report.Aggregate   = Aggregate;
    Report.Summary     = Summary;

    if Args.Verbose
        fprintf('\n%s', Summary);
        disp(Aggregate);
    end

    if ~isempty(Args.ReportFile)
        save(Args.ReportFile, 'Report');
        if Args.Verbose
            fprintf('Report saved to: %s\n', Args.ReportFile);
        end
    end
end
