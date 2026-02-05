function Report = compareMasksPipelines(OldPath, NewPath, Args)
    % Compare masks between two pipeline outputs
    % Input  : - OldPath: Path to old pipeline data directory, or path to
    %            a single old mask file (if ends with .fits).
    %          - NewPath: Path to new pipeline data directory, or path to
    %            a single new mask file (if ends with .fits).
    %          * ...,key,val,...
    %            'FileType'         - 'sci_coadd' or 'sci_proc'. Default is 'sci_proc'.
    %                                 Only used in directory mode.
    %            'EdgeOffset'       - Pixel offset for edge trimming. Default is 5.
    %            'MaxDetailsPerFile'- Max mismatch details to store per file.
    %                                 Set to Inf for all details. Default is 100.
    %            'ReportFile'       - Optional file path to save report (.mat).
    %                                 Default is '' (no save).
    %            'Verbose'          - Print progress to console. Default is true.
    % Output : - Report: struct with comparison results containing fields:
    %            .Timestamp           - datetime of comparison run
    %            .OldPath             - path to old pipeline data
    %            .NewPath             - path to new pipeline data
    %            .FileType            - file type pattern used
    %            .EdgeOffset          - edge offset applied
    %            .NumFilesPaired      - number of matched file pairs
    %            .TotalPixelsCompared - total pixels compared
    %            .TotalMismatches     - number of mismatched pixels
    %            .MatchRate           - percentage of matching pixels
    %            .FileComparisons     - table with per-file comparison results
    %            .MismatchDetails     - struct array with mismatch details
    %            .Summary             - text summary
    % Author : Dana Kovaleva (Jan 2026)
    % Example: % Directory mode:
    %          OldPath = '/bigdata2/projects/last/testNewPipe/222635v0/';
    %          NewPath = '/bigdata2/projects/last/testNewPipe/222625v1/';
    %          Report = pipeline.last.quality.compareMasksPipelines(OldPath, NewPath);
    %          % Single file mode:
    %          OldFile = '/bigdata2/projects/last/testNewPipe/222635v0/LAST.01.08.03_20230616.222635.384_clear_346+79_001_001_001_sci_proc_Mask_1.fits';
    %          NewFile = '/bigdata2/projects/last/testNewPipe/222625v1/LAST.01.08.03_20230616.222625.384_clear_346+79_001_001_001_sci_proc_Mask_1.fits';
    %          Report = pipeline.last.quality.compareMasksPipelines(OldFile, NewFile);

    arguments
        OldPath
        NewPath
        Args.FileType          = 'sci_proc';
        Args.EdgeOffset        = 5;
        Args.MaxDetailsPerFile = 1000000;
        Args.ReportFile        = '';
        Args.Verbose           = true;
    end

    % Detect if inputs are files or directories
    IsFileMode = endsWith(OldPath, '.fits', 'IgnoreCase', true) && ...
                 endsWith(NewPath, '.fits', 'IgnoreCase', true);

    if IsFileMode
        % Single file comparison mode
        if ~isfile(OldPath)
            error('Old file not found: %s', OldPath);
        end
        if ~isfile(NewPath)
            error('New file not found: %s', NewPath);
        end

        if Args.Verbose
            fprintf('Single file comparison mode\n');
            fprintf('Old file: %s\n', OldPath);
            fprintf('New file: %s\n', NewPath);
        end

        % Create file info structures
        [OldFolder, OldName, OldExt] = fileparts(OldPath);
        [NewFolder, NewName, NewExt] = fileparts(NewPath);

        OldFiles(1).folder = OldFolder;
        OldFiles(1).name = [OldName, OldExt];
        NewFiles(1).folder = NewFolder;
        NewFiles(1).name = [NewName, NewExt];

        CommonKeys = {[OldName, OldExt]};
        OldIdx = 1;
        NewIdx = 1;

    else
        % Directory comparison mode
        OldFiles = dir(fullfile(OldPath, ['*_' Args.FileType '_Mask_*.fits']));
        NewFiles = dir(fullfile(NewPath, ['*_' Args.FileType '_Mask_*.fits']));

        if isempty(OldFiles)
            error('No mask files found in OldPath: %s', OldPath);
        end
        if isempty(NewFiles)
            error('No mask files found in NewPath: %s', NewPath);
        end

        if Args.Verbose
            fprintf('Found %d mask files in OldPath\n', numel(OldFiles));
            fprintf('Found %d mask files in NewPath\n', numel(NewFiles));
        end

        % Extract matching keys from filenames based on FileType
        if contains(Args.FileType, 'coadd')
            % For coadd: match by end pattern _NNN_NNN_sci_coadd_Mask_N.fits
            KeyPattern = '_(\d{3}_\d{3}_sci_coadd_Mask_\d+\.fits)$';
        else
            % For proc: match by everything after _clear_
            KeyPattern = '_clear_(.+)$';
        end

        OldKeys = cell(numel(OldFiles), 1);
        for I = 1:numel(OldFiles)
            Tokens = regexp(OldFiles(I).name, KeyPattern, 'tokens');
            if ~isempty(Tokens)
                OldKeys{I} = Tokens{1}{1};
            else
                OldKeys{I} = '';
            end
        end

        NewKeys = cell(numel(NewFiles), 1);
        for I = 1:numel(NewFiles)
            Tokens = regexp(NewFiles(I).name, KeyPattern, 'tokens');
            if ~isempty(Tokens)
                NewKeys{I} = Tokens{1}{1};
            else
                NewKeys{I} = '';
            end
        end

        % Find matching pairs
        [CommonKeys, OldIdx, NewIdx] = intersect(OldKeys, NewKeys);

        if isempty(CommonKeys)
            error('No matching file pairs found between OldPath and NewPath');
        end

        if Args.Verbose
            fprintf('Found %d matching file pairs\n\n', numel(CommonKeys));
        end
    end

    % Initialize counters
    TotalPixels = 0;
    TotalMismatches = 0;

    % Initialize file comparisons table
    FileComparisons = table('Size', [numel(CommonKeys), 5], ...
        'VariableTypes', {'string', 'string', 'string', 'double', 'double'}, ...
        'VariableNames', {'MatchKey', 'OldFile', 'NewFile', 'PixelsCompared', 'Mismatches'});

    % Pre-allocate mismatch details storage for performance
    MaxTotalDetails = Args.MaxDetailsPerFile * numel(CommonKeys);
    MismatchDetails = struct('FileIdx', cell(MaxTotalDetails, 1), ...
                             'MatchKey', cell(MaxTotalDetails, 1), ...
                             'Row', cell(MaxTotalDetails, 1), ...
                             'Col', cell(MaxTotalDetails, 1), ...
                             'OldValue', cell(MaxTotalDetails, 1), ...
                             'NewValue', cell(MaxTotalDetails, 1));
    DetailIdx = 0;

    Offset = Args.EdgeOffset;

    % Compare each pair
    for I = 1:numel(CommonKeys)
        OldFile = fullfile(OldFiles(OldIdx(I)).folder, OldFiles(OldIdx(I)).name);
        NewFile = fullfile(NewFiles(NewIdx(I)).folder, NewFiles(NewIdx(I)).name);

        if Args.Verbose
            fprintf('Comparing pair %d/%d: %s\n', I, numel(CommonKeys), CommonKeys{I});
        end

        % Load masks using AstroImage
        OldAI = AstroImage(OldFile);
        NewAI = AstroImage(NewFile);
        OldMask = OldAI.Image;
        NewMask = NewAI.Image;

        % Get sizes
        [OldSizeY, OldSizeX] = size(OldMask);
        [NewSizeY, NewSizeX] = size(NewMask);

        % Verify size relationship
        ExpectedOldY = NewSizeY + 2 * Offset;
        ExpectedOldX = NewSizeX + 2 * Offset;

        if OldSizeY ~= ExpectedOldY || OldSizeX ~= ExpectedOldX
            warning('Size mismatch for %s: Old=%dx%d, New=%dx%d, Expected Old=%dx%d', ...
                    CommonKeys{I}, OldSizeY, OldSizeX, NewSizeY, NewSizeX, ExpectedOldY, ExpectedOldX);
        else
            % Trim old mask to match new mask
            OldMaskTrimmed = OldMask((Offset+1):(end-Offset), (Offset+1):(end-Offset));

            % Compare masks
            MismatchMask = (OldMaskTrimmed ~= NewMask);
            NumMismatches = sum(MismatchMask, 'all');
            NumPixels = numel(NewMask);

            TotalPixels = TotalPixels + NumPixels;
            TotalMismatches = TotalMismatches + NumMismatches;

            % Store file comparison
            FileComparisons.MatchKey(I) = CommonKeys{I};
            FileComparisons.OldFile(I) = OldFiles(OldIdx(I)).name;
            FileComparisons.NewFile(I) = NewFiles(NewIdx(I)).name;
            FileComparisons.PixelsCompared(I) = NumPixels;
            FileComparisons.Mismatches(I) = NumMismatches;

            % Record mismatch details (limited by MaxDetailsPerFile)
            if NumMismatches > 0
                [Rows, Cols] = find(MismatchMask);
                MaxDetails = min(Args.MaxDetailsPerFile, NumMismatches);
                for J = 1:MaxDetails
                    DetailIdx = DetailIdx + 1;
                    R = Rows(J);
                    C = Cols(J);
                    MismatchDetails(DetailIdx).FileIdx = I;
                    MismatchDetails(DetailIdx).MatchKey = CommonKeys{I};
                    MismatchDetails(DetailIdx).Row = R;
                    MismatchDetails(DetailIdx).Col = C;
                    MismatchDetails(DetailIdx).OldValue = OldMaskTrimmed(R, C);
                    MismatchDetails(DetailIdx).NewValue = NewMask(R, C);
                end
            end
        end
    end

    % Trim pre-allocated mismatch details to actual size
    if DetailIdx > 0
        MismatchDetails = MismatchDetails(1:DetailIdx);
    else
        MismatchDetails = [];
    end

    % Calculate match rate
    if TotalPixels > 0
        MatchRate = 100 * (TotalPixels - TotalMismatches) / TotalPixels;
    else
        MatchRate = 100;
    end

    % Build summary
    Summary = sprintf(['Mask Pipeline Comparison Report\n', ...
                       '================================\n', ...
                       'Old path: %s\n', ...
                       'New path: %s\n', ...
                       'File pairs compared: %d\n', ...
                       'Total pixels compared: %d\n', ...
                       'Total mismatches: %d\n', ...
                       'Match rate: %.4f%%\n'], ...
                       OldPath, NewPath, numel(CommonKeys), TotalPixels, TotalMismatches, MatchRate);

    % Build report structure
    Report = struct();
    Report.Timestamp = datetime('now');
    Report.OldPath = OldPath;
    Report.NewPath = NewPath;
    Report.FileType = Args.FileType;
    Report.EdgeOffset = Offset;
    Report.NumFilesPaired = numel(CommonKeys);
    Report.TotalPixelsCompared = TotalPixels;
    Report.TotalMismatches = TotalMismatches;
    Report.MatchRate = MatchRate;
    Report.FileComparisons = FileComparisons;
    Report.MismatchDetails = MismatchDetails;
    Report.Summary = Summary;

    % Verbose output
    if Args.Verbose
        fprintf('\n%s', Summary);
        if TotalMismatches > 0
            fprintf('\nFiles with mismatches:\n');
            disp(FileComparisons(FileComparisons.Mismatches > 0, :));
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
