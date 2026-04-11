function Report = maskBitStatistics(DataPath, Args)
    % Per-bit pixel statistics over a set of FITS mask files
    % Description: Recursively scans a directory tree for Mask FITS files
    %   matching FileType (e.g. 'sci_coadd'), loads each mask, and for each
    %   bit (either a user-selected subset, or all bits in the dictionary)
    %   counts the number and fraction of pixels that have that bit set.
    %   Reports per-file (long form) and aggregated statistics.
    %
    %   Bit indices are resolved by name via BitDictionary, so no bit index
    %   is hardcoded. This keeps the function robust across dictionary
    %   revisions.
    %
    % Input  : - Path to directory containing mask FITS files. The directory
    %            is scanned recursively (subdirs included).
    %          * ...,key,val,...
    %            'FileType'     - File type pattern. Default is 'sci_coadd'.
    %                             Used to glob '*<FileType>_Mask_*.fits'.
    %            'BitNames'     - Cell array of bit names to compute statistics
    %                             for. If empty, uses all bits in the
    %                             dictionary. Default is {}.
    %            'BitDictName'  - BitDictionary name for bit name lookups.
    %                             Default is 'BitMask.Image.Default'.
    %            'AccumulateMaps' - If true, accumulate per-bit 2D count maps
    %                             across all files (one (SizeY x SizeX) uint16
    %                             map per bit per size group). Stored in
    %                             Report.AccumMaps. Default is false.
    %            'KeepPerFileMaps' - If true, also store per-(file,bit) logical
    %                             2D maps in Report.PerFileMaps (memory-heavy).
    %                             Default is false.
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
    %            .NumFiles      - number of mask files processed
    %            .PerFile       - long-form table with one row per (file, bit):
    %                             FileKey, BitName, BitIndex, SizeY, SizeX,
    %                             NumPixels, NumPixelsWithBit, FractionPixelsWithBit
    %            .Aggregate     - per-bit aggregate table:
    %                             BitName, BitIndex, NumFiles, TotalPixels,
    %                             TotalPixelsWithBit, OverallFraction,
    %                             MeanFraction, MedianFraction, MaxFraction
    %            .AccumMaps     - (if AccumulateMaps) struct of per-bit
    %                             accumulator maps. Each field is named after
    %                             a size group ("S<SizeY>x<SizeX>") and holds
    %                             a struct keyed by sanitized bit name, with
    %                             a (SizeY x SizeX) uint16 count of files in
    %                             which that pixel has the bit set.
    %            .AccumMapsBitNames - (if AccumulateMaps) containers.Map from
    %                             sanitized bit name (struct field key) to the
    %                             original dictionary bit name. Used by
    %                             plotMaskBitStatistics.
    %            .PerFileMaps   - (if KeepPerFileMaps) cell array of size
    %                             (Nfiles x Nbits) holding per-file logical
    %                             bit maps. Empty for files that failed.
    %            .Summary       - text summary
    %
    % Author : Dana Kovaleva (Apr 2026)
    % Example: DataPath = '/bigdata2/projects/last/testNewPipe/222625v3';
    %          % Single bit, statistics only:
    %          R = pipeline.last.quality.maskBitStatistics(DataPath, ...
    %                  'BitNames', {'CoaddLessImages'});
    %          % Same but also accumulate the 2D bit map for plotting:
    %          R = pipeline.last.quality.maskBitStatistics(DataPath, ...
    %                  'BitNames', {'CoaddLessImages'}, ...
    %                  'AccumulateMaps', true);
    %          pipeline.last.quality.plotMaskBitStatistics(R);
    %          % All bits in the dictionary, statistics only:
    %          R = pipeline.last.quality.maskBitStatistics(DataPath);

    arguments
        DataPath                     (1,:) char
        Args.FileType                (1,:) char  = 'sci_coadd'
        Args.BitNames                cell        = {}
        Args.BitDictName             (1,:) char  = 'BitMask.Image.Default'
        Args.AccumulateMaps          (1,1) logical = false
        Args.KeepPerFileMaps         (1,1) logical = false
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

    Nbits    = numel(BitInds);
    BitValues = bitshift(uint32(1), uint32(BitInds));   % 2^BitInd as uint32

    BitDictTable = table(BitNames, BitInds, double(BitValues), ...
        'VariableNames', {'BitName', 'BitIndex', 'BitValue'});

    if Args.Verbose
        fprintf('Data path: %s\n', DataPath);
        fprintf('File type: %s\n', Args.FileType);
        fprintf('Checking %d bit(s) from dictionary %s\n', Nbits, Args.BitDictName);
    end

    % --- Find mask files ------------------------------------------------
    Pattern = ['*' Args.FileType '_Mask_*.fits'];
    if Args.Recursive
        Dm = dir(fullfile(DataPath, '**', Pattern));
    else
        Dm = dir(fullfile(DataPath, Pattern));
    end

    if isempty(Dm)
        error('No mask files found matching %s under %s', Pattern, DataPath);
    end

    Nfiles = numel(Dm);
    if Args.Verbose
        fprintf('Found %d mask files\n\n', Nfiles);
    end

    % --- File key regex (shared convention with checkMaskPropagationToCoadd) ---
    FileKeyPattern = '_clear_(.+)_sci_';

    % --- Pre-allocate per-file long-form storage ------------------------
    Nrows = Nfiles * Nbits;
    FileKeyCol      = strings(Nrows, 1);
    BitNameCol      = strings(Nrows, 1);
    BitIndexCol     = zeros(Nrows, 1);
    SizeYCol        = zeros(Nrows, 1);
    SizeXCol        = zeros(Nrows, 1);
    NumPixelsCol    = zeros(Nrows, 1);
    NumBitPixelsCol = zeros(Nrows, 1);
    FracBitPixelsCol= zeros(Nrows, 1);

    Row = 0;

    % --- Optional 2D accumulators ---------------------------------------
    % AccumMaps.<SizeKey>.<BitKey> -> uint16(SizeY x SizeX) count map.
    % Sanitized bit names are used as struct field keys.
    AccumMaps = struct();
    SanBitNames = matlab.lang.makeValidName(BitNames);

    if Args.KeepPerFileMaps
        PerFileMaps = cell(Nfiles, Nbits);
    else
        PerFileMaps = {};
    end

    % --- Main loop over mask files --------------------------------------
    for Ifile = 1:Nfiles
        FileName = fullfile(Dm(Ifile).folder, Dm(Ifile).name);

        try
            AI = AstroImage(FileName);
            Mask = uint32(AI.Image);
        catch ME
            warning('Failed to read %s: %s', Dm(Ifile).name, ME.message);
            continue;
        end

        [SizeY, SizeX] = size(Mask);
        NumPixels = SizeY * SizeX;

        Tokens = regexp(Dm(Ifile).name, FileKeyPattern, 'tokens');
        if ~isempty(Tokens)
            FileKey = Tokens{1}{1};
        else
            FileKey = Dm(Ifile).name;
        end

        if Args.Verbose
            fprintf('[%d/%d] %s (%dx%d)\n', Ifile, Nfiles, Dm(Ifile).name, SizeY, SizeX);
        end

        % Size group key for the accumulator (handles heterogeneous sizes)
        SizeKey = sprintf('S%dx%d', SizeY, SizeX);

        for Ib = 1:Nbits
            BitMap     = bitand(Mask, BitValues(Ib)) > 0;     % logical (SizeY x SizeX)
            NumBitPix  = nnz(BitMap);
            FracBitPix = NumBitPix / NumPixels;

            Row = Row + 1;
            FileKeyCol(Row)       = string(FileKey);
            BitNameCol(Row)       = string(BitNames{Ib});
            BitIndexCol(Row)      = BitInds(Ib);
            SizeYCol(Row)         = SizeY;
            SizeXCol(Row)         = SizeX;
            NumPixelsCol(Row)     = NumPixels;
            NumBitPixelsCol(Row)  = NumBitPix;
            FracBitPixelsCol(Row) = FracBitPix;

            % --- 2D accumulator ---
            if Args.AccumulateMaps
                BitKey = SanBitNames{Ib};
                if ~isfield(AccumMaps, SizeKey)
                    AccumMaps.(SizeKey) = struct();
                end
                if ~isfield(AccumMaps.(SizeKey), BitKey)
                    AccumMaps.(SizeKey).(BitKey) = zeros(SizeY, SizeX, 'uint16');
                end
                AccumMaps.(SizeKey).(BitKey) = ...
                    AccumMaps.(SizeKey).(BitKey) + uint16(BitMap);
            end

            % --- Per-file logical map ---
            if Args.KeepPerFileMaps
                PerFileMaps{Ifile, Ib} = BitMap;
            end
        end
    end

    % Trim unused pre-allocated rows (in case some files failed)
    FileKeyCol       = FileKeyCol(1:Row);
    BitNameCol       = BitNameCol(1:Row);
    BitIndexCol      = BitIndexCol(1:Row);
    SizeYCol         = SizeYCol(1:Row);
    SizeXCol         = SizeXCol(1:Row);
    NumPixelsCol     = NumPixelsCol(1:Row);
    NumBitPixelsCol  = NumBitPixelsCol(1:Row);
    FracBitPixelsCol = FracBitPixelsCol(1:Row);

    PerFile = table(FileKeyCol, BitNameCol, BitIndexCol, SizeYCol, SizeXCol, ...
                    NumPixelsCol, NumBitPixelsCol, FracBitPixelsCol, ...
        'VariableNames', {'FileKey', 'BitName', 'BitIndex', 'SizeY', 'SizeX', ...
                          'NumPixels', 'NumPixelsWithBit', 'FractionPixelsWithBit'});

    % --- Aggregate per bit ---------------------------------------------
    AggBitName         = strings(Nbits, 1);
    AggBitIndex        = zeros(Nbits, 1);
    AggNumFiles        = zeros(Nbits, 1);
    AggTotalPixels     = zeros(Nbits, 1);
    AggTotalBitPixels  = zeros(Nbits, 1);
    AggOverallFraction = zeros(Nbits, 1);
    AggMeanFraction    = zeros(Nbits, 1);
    AggMedianFraction  = zeros(Nbits, 1);
    AggMaxFraction     = zeros(Nbits, 1);

    for Ib = 1:Nbits
        Sel = PerFile.BitIndex == BitInds(Ib);
        SubFrac  = PerFile.FractionPixelsWithBit(Sel);
        SubNumPx = PerFile.NumPixels(Sel);
        SubBitPx = PerFile.NumPixelsWithBit(Sel);

        AggBitName(Ib)        = string(BitNames{Ib});
        AggBitIndex(Ib)       = BitInds(Ib);
        AggNumFiles(Ib)       = numel(SubFrac);
        AggTotalPixels(Ib)    = sum(SubNumPx);
        AggTotalBitPixels(Ib) = sum(SubBitPx);

        if AggTotalPixels(Ib) > 0
            AggOverallFraction(Ib) = AggTotalBitPixels(Ib) / AggTotalPixels(Ib);
        end
        if ~isempty(SubFrac)
            AggMeanFraction(Ib)   = mean(SubFrac);
            AggMedianFraction(Ib) = median(SubFrac);
            AggMaxFraction(Ib)    = max(SubFrac);
        end
    end

    Aggregate = table(AggBitName, AggBitIndex, AggNumFiles, ...
                      AggTotalPixels, AggTotalBitPixels, ...
                      AggOverallFraction, AggMeanFraction, AggMedianFraction, AggMaxFraction, ...
        'VariableNames', {'BitName', 'BitIndex', 'NumFiles', ...
                          'TotalPixels', 'TotalPixelsWithBit', ...
                          'OverallFraction', 'MeanFraction', ...
                          'MedianFraction', 'MaxFraction'});

    % --- Summary --------------------------------------------------------
    Summary = sprintf(['Mask Bit Statistics Report\n', ...
                       '==========================\n', ...
                       'Data path     : %s\n', ...
                       'File type     : %s\n', ...
                       'Bit dictionary: %s\n', ...
                       'Files scanned : %d\n', ...
                       'Bits analyzed : %d\n'], ...
                       DataPath, Args.FileType, Args.BitDictName, Nfiles, Nbits);

    % --- Assemble report -----------------------------------------------
    Report = struct();
    Report.Timestamp   = datetime('now');
    Report.DataPath    = DataPath;
    Report.FileType    = Args.FileType;
    Report.BitDictName = Args.BitDictName;
    Report.BitDict     = BitDictTable;
    Report.NumFiles    = Nfiles;
    Report.PerFile     = PerFile;
    Report.Aggregate   = Aggregate;
    if Args.AccumulateMaps
        Report.AccumMaps = AccumMaps;
        % Also expose the sanitized bit name <-> original bit name mapping
        % so plotters can map bit names back to dictionary names.
        Report.AccumMapsBitNames = containers.Map(SanBitNames, BitNames);
    end
    if Args.KeepPerFileMaps
        Report.PerFileMaps = PerFileMaps;
    end
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
