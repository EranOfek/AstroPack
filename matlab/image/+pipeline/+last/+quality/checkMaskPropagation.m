function [Report, AI] = checkMaskPropagation(Input, Args)
    % Check that mask bits are properly propagated to catalog FLAGS
    % Description: Validates that mask bits at source positions are correctly
    %              propagated to the FLAGS column in catalogs. For each source,
    %              verifies that bitand(FLAGS, MASK_SRC) == MASK_SRC, meaning
    %              all mask bits are present in FLAGS.
    %
    % Input  : - Input - Either:
    %            (1) AstroImage array with Mask and CatData populated, or
    %            (2) Path string to directory containing FITS files
    %          * ...,key,val,...
    %            'FileType'   - File type pattern when loading from path.
    %                           Options: 'sci_proc', 'sci_coadd', or custom.
    %                           Default is 'sci_proc'.
    %            'ColX'       - X coordinate column name. Default is 'XPEAK'.
    %            'ColY'       - Y coordinate column name. Default is 'YPEAK'.
    %            'ColFlags'   - FLAGS column name. Default is 'FLAGS'.
    %            'BitDictName'- BitDictionary name. Default is 'BitMask.Image.Default'.
    %            'ReportFile' - Optional file path to save report (.mat).
    %                           Default is '' (no save).
    %            'Verbose'    - Print summary to console. Default is true.
    %
    % Output : - Report - struct with validation results:
    %            .Timestamp      - datetime of validation run
    %            .DataPath       - path to data (if loaded from files)
    %            .FileType       - file type pattern (if loaded from files)
    %            .NumFragments   - number of AstroImage elements processed
    %            .TotalSources   - total sources checked
    %            .FailedCount    - number of sources with missing bits
    %            .PassRate       - percentage of sources that passed
    %            .Failures       - table with failed source details
    %            .Summary        - text summary
    %
    % Author : Dana Kovaleva (Jan 2026)
    % Example: % Load from path and validate sci_proc files:
    %          DataPath = '~/Downloads/tm/TestMask/';
    %          Report = pipeline.last.quality.checkMaskPropagation(DataPath);
    %
    %          % Load from path with sci_coadd files:
    %          Report = pipeline.last.quality.checkMaskPropagation(DataPath, 'FileType', 'sci_coadd');
    %
    %          % Save report to file:
    %          Report = pipeline.last.quality.checkMaskPropagation(DataPath, 'ReportFile', fullfile(DataPath, 'report.mat'));
    %
    %          % Use with pre-loaded AstroImage:
    %          Report = pipeline.last.quality.checkMaskPropagation(AI);

    arguments
        Input
        Args.FileType       = 'sci_proc';
        Args.ColX           = 'XPEAK';
        Args.ColY           = 'YPEAK';
        Args.ColFlags       = 'FLAGS';
        Args.BitDictName    = 'BitMask.Image.Default';
        Args.ReportFile     = '';
        Args.Verbose        = true;
    end

    % Handle input - either AstroImage or path string
    DataPath = '';
    FileType = Args.FileType;
    if ischar(Input) || isstring(Input)
        % Input is a path - load data from files
        DataPath = char(Input);

        if Args.Verbose
            fprintf('Loading data from: %s\n', DataPath);
            fprintf('File type: %s\n', FileType);
        end

        % Find Image, Catalog, and Mask files
        Di = dir(fullfile(DataPath, ['*' FileType '_Image*.fits']));
        Dc = dir(fullfile(DataPath, ['*' FileType '_Cat*.fits']));
        Dm = dir(fullfile(DataPath, ['*' FileType '_Mask*.fits']));

        if isempty(Di)
            error('No Image files found matching pattern *%s_Image*.fits in %s', FileType, DataPath);
        end
        if isempty(Dc)
            error('No Catalog files found matching pattern *%s_Cat*.fits in %s', FileType, DataPath);
        end
        if isempty(Dm)
            error('No Mask files found matching pattern *%s_Mask*.fits in %s', FileType, DataPath);
        end

        % Build full file paths
        FNi = fullfile({Di.folder}, {Di.name});
        FNc = fullfile({Dc.folder}, {Dc.name});
        FNm = fullfile({Dm.folder}, {Dm.name});

        % Sort to ensure matching order
        FNi = sort(FNi);
        FNc = sort(FNc);
        FNm = sort(FNm);

        if Args.Verbose
            fprintf('Found %d Image files, %d Catalog files, %d Mask files\n', ...
                    numel(FNi), numel(FNc), numel(FNm));
        end

        % Verify same number of files
        if numel(FNi) ~= numel(FNc) || numel(FNi) ~= numel(FNm)
            error('Mismatch in number of files: %d Images, %d Catalogs, %d Masks', ...
                  numel(FNi), numel(FNc), numel(FNm));
        end

        % Load AstroImage with Mask
        AI = AstroImage(FNi, 'Mask', FNm);

        % Load Catalogs and attach to AstroImage
        AC = AstroCatalog(FNc);
        for Ifile = 1:1:numel(AI)
            AI(Ifile).CatData = AC(Ifile);
        end

        if Args.Verbose
            fprintf('Data loaded successfully\n\n');
        end
    elseif isa(Input, 'AstroImage')
        AI = Input;
    else
        error('Input must be either an AstroImage array or a path string');
    end

    % Initialize BitDictionary for bit name lookups
    BD = BitDictionary(Args.BitDictName);

    % Initialize counters
    TotalSources = 0;
    FailedCount = 0;

    % Pre-allocate failures storage (will grow if needed)
    MaxFailures = 1000;
    FailureData = struct('FragmentIdx', cell(MaxFailures, 1), ...
                         'SourceIdx', cell(MaxFailures, 1), ...
                         'X', cell(MaxFailures, 1), ...
                         'Y', cell(MaxFailures, 1), ...
                         'MaskValue', cell(MaxFailures, 1), ...
                         'FlagsValue', cell(MaxFailures, 1), ...
                         'MissingBits', cell(MaxFailures, 1), ...
                         'MaskBitInd', cell(MaxFailures, 1), ...
                         'FlagsBitInd', cell(MaxFailures, 1), ...
                         'MissingBitInd', cell(MaxFailures, 1), ...
                         'MaskBitNames', cell(MaxFailures, 1), ...
                         'FlagsBitNames', cell(MaxFailures, 1), ...
                         'MissingBitNames', cell(MaxFailures, 1));
    FailIdx = 0;

    Nfrag = numel(AI);

    for Ifrag = 1:1:Nfrag
        % Get catalog columns
        if isempty(AI(Ifrag).CatData) || AI(Ifrag).CatData.sizeCatalog == 0
            continue;
        end

        X = AI(Ifrag).CatData.getCol(Args.ColX);
        Y = AI(Ifrag).CatData.getCol(Args.ColY);
        FLAGS = AI(Ifrag).CatData.getCol(Args.ColFlags);

        if isempty(X) || isempty(Y) || isempty(FLAGS)
            continue;
        end

        % Get mask data
        MaskData = AI(Ifrag).Mask;
        if isempty(MaskData)
            continue;
        end

        [MaskSizeY, MaskSizeX] = size(MaskData);
        Nsrc = numel(X);
        TotalSources = TotalSources + Nsrc;

        for Isrc = 1:1:Nsrc
            xi = round(X(Isrc));
            yi = round(Y(Isrc));

            % Bounds check
            if xi >= 1 && xi <= MaskSizeX && yi >= 1 && yi <= MaskSizeY
                maskVal = uint32(MaskData(yi, xi));
                flagsVal = uint32(FLAGS(Isrc));

                % Core validation: all mask bits must be in flags
                if bitand(flagsVal, maskVal) ~= maskVal
                    FailedCount = FailedCount + 1;
                    FailIdx = FailIdx + 1;

                    % Expand storage if needed
                    if FailIdx > MaxFailures
                        MaxFailures = MaxFailures * 2;
                        FailureData(MaxFailures).FragmentIdx = [];
                    end

                    % Calculate missing bits
                    missingBits = bitand(maskVal, bitxor(maskVal, bitand(flagsVal, maskVal)));

                    % Get bit indices and names using bitget (no toolbox required)
                    [maskBitInd, maskBitNames] = getBitIndicesAndNames(BD, maskVal);
                    [flagsBitInd, flagsBitNames] = getBitIndicesAndNames(BD, flagsVal);
                    [missingBitInd, missingBitNames] = getBitIndicesAndNames(BD, missingBits);

                    % Store failure details
                    FailureData(FailIdx).FragmentIdx = Ifrag;
                    FailureData(FailIdx).SourceIdx = Isrc;
                    FailureData(FailIdx).X = xi;
                    FailureData(FailIdx).Y = yi;
                    FailureData(FailIdx).MaskValue = maskVal;
                    FailureData(FailIdx).FlagsValue = flagsVal;
                    FailureData(FailIdx).MissingBits = missingBits;
                    FailureData(FailIdx).MaskBitInd = maskBitInd;
                    FailureData(FailIdx).FlagsBitInd = flagsBitInd;
                    FailureData(FailIdx).MissingBitInd = missingBitInd;
                    FailureData(FailIdx).MaskBitNames = maskBitNames;
                    FailureData(FailIdx).FlagsBitNames = flagsBitNames;
                    FailureData(FailIdx).MissingBitNames = missingBitNames;
                end
            end
        end
    end

    % Trim failure data
    if FailIdx > 0
        FailureData = FailureData(1:FailIdx);

        % Convert to table
        FailuresTable = table(...
            [FailureData.FragmentIdx]', ...
            [FailureData.SourceIdx]', ...
            [FailureData.X]', ...
            [FailureData.Y]', ...
            [FailureData.MaskValue]', ...
            [FailureData.FlagsValue]', ...
            [FailureData.MissingBits]', ...
            {FailureData.MaskBitInd}', ...
            {FailureData.FlagsBitInd}', ...
            {FailureData.MissingBitInd}', ...
            {FailureData.MaskBitNames}', ...
            {FailureData.FlagsBitNames}', ...
            {FailureData.MissingBitNames}', ...
            'VariableNames', {'FragmentIdx', 'SourceIdx', 'X', 'Y', ...
                              'MaskValue', 'FlagsValue', 'MissingBits', ...
                              'MaskBitInd', 'FlagsBitInd', 'MissingBitInd', ...
                              'MaskBitNames', 'FlagsBitNames', 'MissingBitNames'});
    else
        % Empty table with correct columns
        FailuresTable = table('Size', [0, 13], ...
            'VariableTypes', {'double', 'double', 'double', 'double', ...
                              'uint32', 'uint32', 'uint32', ...
                              'cell', 'cell', 'cell', ...
                              'cell', 'cell', 'cell'}, ...
            'VariableNames', {'FragmentIdx', 'SourceIdx', 'X', 'Y', ...
                              'MaskValue', 'FlagsValue', 'MissingBits', ...
                              'MaskBitInd', 'FlagsBitInd', 'MissingBitInd', ...
                              'MaskBitNames', 'FlagsBitNames', 'MissingBitNames'});
    end

    % Calculate pass rate
    if TotalSources > 0
        PassRate = 100 * (TotalSources - FailedCount) / TotalSources;
    else
        PassRate = 100;
    end

    % Build summary text
    Summary = sprintf(['Mask Propagation Validation Report\n', ...
                       '===================================\n', ...
                       'Fragments processed: %d\n', ...
                       'Total sources checked: %d\n', ...
                       'Failed sources: %d\n', ...
                       'Pass rate: %.2f%%\n'], ...
                       Nfrag, TotalSources, FailedCount, PassRate);

    % Build report structure
    Report = struct();
    Report.Timestamp = datetime('now');
    Report.DataPath = DataPath;
    Report.FileType = FileType;
    Report.NumFragments = Nfrag;
    Report.TotalSources = TotalSources;
    Report.FailedCount = FailedCount;
    Report.PassRate = PassRate;
    Report.Failures = FailuresTable;
    Report.Summary = Summary;

    % Verbose output
    if Args.Verbose
        fprintf('%s', Summary);
        if FailedCount > 0 && FailedCount <= 20
            fprintf('\nFailed sources:\n');
            disp(FailuresTable);
        elseif FailedCount > 20
            fprintf('\nFirst 20 failed sources:\n');
            disp(FailuresTable(1:20, :));
            fprintf('... and %d more failures.\n', FailedCount - 20);
        end
    end

    % Save report if requested
    if ~isempty(Args.ReportFile)
        save(Args.ReportFile, 'Report');
        if Args.Verbose
            fprintf('\nReport saved to: %s\n', Args.ReportFile);
        end
    end

end

%% Local helper function
function [BitInd, BitNames] = getBitIndicesAndNames(BD, DecVal)
    % Convert decimal value to bit indices and names using bitget
    % Does not require Communications Toolbox (unlike de2bi)
    %
    % Input  : - BD - BitDictionary object
    %          - DecVal - Decimal value (uint32)
    % Output : - BitInd - Array of bit indices (0-indexed)
    %          - BitNames - Cell array of bit names from dictionary

    % Get bit indices using bitget (1-indexed in MATLAB)
    BitInd = [];
    for Ibit = 1:32
        if bitget(DecVal, Ibit)
            BitInd = [BitInd, Ibit - 1];  % Convert to 0-indexed
        end
    end

    % Look up bit names from dictionary
    BitNames = cell(1, numel(BitInd));
    for Ibit = 1:numel(BitInd)
        idx = find(BD.Dic.BitInd == BitInd(Ibit));
        if ~isempty(idx)
            BitNames{Ibit} = BD.Dic.BitName{idx};
        else
            BitNames{Ibit} = sprintf('Bit%d', BitInd(Ibit));
        end
    end
end
