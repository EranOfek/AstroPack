function Report = compareMasksPipelines(OldPath, NewPath, Args)
    % Compare masks between two pipeline outputs
    %
    % Description: Compares mask values between two different pipeline outputs
    %              for matching files. Handles different image sizes by applying
    %              edge offset (old images are larger, new images are trimmed).
    %
    % Input:
    %   OldPath - Path to old pipeline data (larger images, 1726x1726)
    %   NewPath - Path to new pipeline data (trimmed images, 1716x1716)
    %   * ...,key,val,...
    %     'FileType'   - 'sci_coadd' or 'sci_proc'. Default is 'sci_coadd'.
    %     'EdgeOffset' - Pixel offset for edge trimming. Default is 5.
    %     'ReportFile' - Optional file path to save report (.mat).
    %     'Verbose'    - Print progress to console. Default is true.
    %
    % Output:
    %   Report - struct with comparison results
    %
    % Matching: Files matched by suffix _NNN_NNN_sci_{type}_Mask_{num}.fits
    %
    % Author: Dana Kovaleva (Jan 2026)
    % Example:
    %   OldPath = '/bigdata2/projects/last/testNewPipe/222635v0/';
    %   NewPath = '/bigdata2/projects/last/testNewPipe/222625v1/';
    %   Report = pipeline.last.quality.compareMasksPipelines(OldPath, NewPath);

    arguments
        OldPath
        NewPath
        Args.FileType   = 'sci_coadd';
        Args.EdgeOffset = 5;
        Args.ReportFile = '';
        Args.Verbose    = true;
    end

    % Find mask files in both paths
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

    % Extract matching keys from filenames
    % Pattern: _NNN_NNN_sci_{type}_Mask_{num}.fits
    keyPattern = '_(\d{3}_\d{3}_sci_\w+_Mask_\d+)\.fits';

    OldKeys = cell(numel(OldFiles), 1);
    for I = 1:numel(OldFiles)
        tokens = regexp(OldFiles(I).name, keyPattern, 'tokens');
        if ~isempty(tokens)
            OldKeys{I} = tokens{1}{1};
        else
            OldKeys{I} = '';
        end
    end

    NewKeys = cell(numel(NewFiles), 1);
    for I = 1:numel(NewFiles)
        tokens = regexp(NewFiles(I).name, keyPattern, 'tokens');
        if ~isempty(tokens)
            NewKeys{I} = tokens{1}{1};
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

    % Initialize counters
    TotalPixels = 0;
    TotalMismatches = 0;

    % Initialize file comparisons table
    FileComparisons = table('Size', [numel(CommonKeys), 5], ...
        'VariableTypes', {'string', 'string', 'string', 'double', 'double'}, ...
        'VariableNames', {'MatchKey', 'OldFile', 'NewFile', 'PixelsCompared', 'Mismatches'});

    % Initialize mismatch details storage
    MismatchDetails = [];

    Offset = Args.EdgeOffset;

    % Compare each pair
    for I = 1:numel(CommonKeys)
        oldFile = fullfile(OldFiles(OldIdx(I)).folder, OldFiles(OldIdx(I)).name);
        newFile = fullfile(NewFiles(NewIdx(I)).folder, NewFiles(NewIdx(I)).name);

        if Args.Verbose
            fprintf('Comparing pair %d/%d: %s\n', I, numel(CommonKeys), CommonKeys{I});
        end

        % Load masks using AstroImage
        OldAI = AstroImage(oldFile);
        NewAI = AstroImage(newFile);
        OldMask = OldAI.Image;
        NewMask = NewAI.Image;

        % Get sizes
        [OldSizeY, OldSizeX] = size(OldMask);
        [NewSizeY, NewSizeX] = size(NewMask);

        % Verify size relationship
        expectedOldY = NewSizeY + 2 * Offset;
        expectedOldX = NewSizeX + 2 * Offset;

        if OldSizeY ~= expectedOldY || OldSizeX ~= expectedOldX
            warning('Size mismatch for %s: Old=%dx%d, New=%dx%d, Expected Old=%dx%d', ...
                    CommonKeys{I}, OldSizeY, OldSizeX, NewSizeY, NewSizeX, expectedOldY, expectedOldX);
        else
            % Trim old mask to match new mask
            OldMaskTrimmed = OldMask((Offset+1):(end-Offset), (Offset+1):(end-Offset));

            % Compare
            mismatchMask = (OldMaskTrimmed ~= NewMask);
            numMismatches = sum(mismatchMask, 'all');
            numPixels = numel(NewMask);

            TotalPixels = TotalPixels + numPixels;
            TotalMismatches = TotalMismatches + numMismatches;

            % Store file comparison
            FileComparisons.MatchKey(I) = CommonKeys{I};
            FileComparisons.OldFile(I) = OldFiles(OldIdx(I)).name;
            FileComparisons.NewFile(I) = NewFiles(NewIdx(I)).name;
            FileComparisons.PixelsCompared(I) = numPixels;
            FileComparisons.Mismatches(I) = numMismatches;

            % Record mismatch details (limit to first 100 per file)
            if numMismatches > 0
                [rows, cols] = find(mismatchMask);
                maxDetails = min(100, numMismatches);
                for J = 1:maxDetails
                    r = rows(J);
                    c = cols(J);
                    detail.FileIdx = I;
                    detail.MatchKey = CommonKeys{I};
                    detail.Row = r;
                    detail.Col = c;
                    detail.OldValue = OldMaskTrimmed(r, c);
                    detail.NewValue = NewMask(r, c);
                    MismatchDetails = [MismatchDetails; detail];
                end
            end
        end
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
