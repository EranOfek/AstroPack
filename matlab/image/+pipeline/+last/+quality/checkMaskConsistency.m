function Report = checkMaskConsistency(Input, Args)
    % Check self-consistency of bitmasks in pipeline FITS files
    % Description: Validates multiple bit mask properties against image data
    %   and catalog. Checks performed:
    %   1. NearEdge (bit 23) - edge pixels match image geometry
    %   2. NaN (bit 6) - NaN bit matches NaN pixels in image
    %   3. Saturated (bit 0) - saturated bit matches pixels > SatLevel
    %   4. Negative (bit 10) - negative bit matches negative pixels
    %   5. SrcDetected (bit 30) - source bit matches catalog positions
    % Input  : - Input: path string to directory containing FITS files.
    %          * ...,key,val,...
    %            'FileType'    - 'sci_proc' or 'sci_coadd'. Default is 'sci_proc'.
    %            'Checks'      - Cell array of check names to run. Any subset
    %                            of {'NearEdge','NaN','Saturated','Negative','SrcDetected'}.
    %                            Default is {'NearEdge'}.
    %                            SrcDetected (bit 30) is not yet implemented in
    %                            the pipeline; include it explicitly to test.
    %            'EdgeWidth'   - NearEdge band width in pixels. Default is 10.
    %            'SatLevel'    - Saturation threshold in ADU. Default is 62000.
    %            'FWHM'        - FWHM radius for SrcDetected (pixels).
    %                            If empty, estimated from catalog X2,Y2.
    %                            Default is [].
    %            'BitDictName' - BitDictionary name.
    %                            Default is 'BitMask.Image.Default'.
    %            'ReportFile'  - Optional file path to save report (.mat).
    %                            Default is '' (no save).
    %            'Verbose'     - Print progress to console. Default is true.
    % Output : - Report: struct with fields per check (NearEdge, NaN_Check,
    %            Saturated, Negative, SrcDetected), each containing a
    %            FileResults table with columns:
    %              MaskAndImage - condition present in both mask and image
    %              MaskOnly     - bit set in mask but condition absent in image
    %              ImageOnly    - condition present in image but bit not in mask
    %            plus overall Summary.
    % Author : Dana Kovaleva (Feb 2026)
    % Example: DataPath = '/bigdata2/projects/last/testNewPipe/222625v1/';
    %          Report = pipeline.last.quality.checkMaskConsistency(DataPath);
    %          Report = pipeline.last.quality.checkMaskConsistency(DataPath, 'FileType', 'sci_coadd');
    %          Report = pipeline.last.quality.checkMaskConsistency(DataPath, 'Checks', {'NearEdge','NaN'});

    arguments
        Input
        Args.FileType       = 'sci_proc';
        Args.Checks         = {'NearEdge'};
        Args.EdgeWidth      = 10;
        Args.SatLevel       = 62000;
        Args.FWHM           = [];
        Args.BitDictName    = 'BitMask.Image.Default';
        Args.ReportFile     = '';
        Args.Verbose        = true;
    end

    % Determine which checks to run
    RunNearEdge    = any(strcmpi(Args.Checks, 'NearEdge'));
    RunNaN         = any(strcmpi(Args.Checks, 'NaN'));
    RunSaturated   = any(strcmpi(Args.Checks, 'Saturated'));
    RunNegative    = any(strcmpi(Args.Checks, 'Negative'));
    RunSrcDetected = any(strcmpi(Args.Checks, 'SrcDetected'));

    NeedImage = RunNaN || RunSaturated || RunNegative;
    NeedCat   = RunSrcDetected;

    % Initialize BitDictionary and get bit values
    BD = BitDictionary(Args.BitDictName);
    if RunNearEdge;    NearEdgeBitVal    = getBitDecVal(BD, 'NearEdge');    end
    if RunNaN;         NaNBitVal         = getBitDecVal(BD, 'NaN');         end
    if RunSaturated;   SaturatedBitVal   = getBitDecVal(BD, 'Saturated');   end
    if RunNegative;    NegativeBitVal    = getBitDecVal(BD, 'Negative');    end
    if RunSrcDetected; SrcDetectedBitVal = getBitDecVal(BD, 'SrcDetected'); end

    % Load file lists
    DataPath = char(Input);
    FileType = Args.FileType;

    if Args.Verbose
        fprintf('Data path: %s\n', DataPath);
        fprintf('File type: %s\n', FileType);
    end

    % Find Mask files (always needed)
    Dm = dir(fullfile(DataPath, ['*' FileType '_Mask*.fits']));
    if isempty(Dm)
        error('No Mask files found matching *%s_Mask*.fits in %s', FileType, DataPath);
    end
    FNm = sort(fullfile({Dm.folder}, {Dm.name}));
    MaskNames = sort({Dm.name});

    % Find Image files (if needed)
    FNi = {};
    if NeedImage
        Di = dir(fullfile(DataPath, ['*' FileType '_Image*.fits']));
        if isempty(Di)
            error('No Image files found matching *%s_Image*.fits in %s', FileType, DataPath);
        end
        FNi = sort(fullfile({Di.folder}, {Di.name}));
        if numel(FNi) ~= numel(FNm)
            error('Mismatch: %d Mask files vs %d Image files', numel(FNm), numel(FNi));
        end
    end

    % Find Cat files (if needed)
    FNc = {};
    if NeedCat
        Dc = dir(fullfile(DataPath, ['*' FileType '_Cat*.fits']));
        if isempty(Dc)
            error('No Cat files found matching *%s_Cat*.fits in %s', FileType, DataPath);
        end
        FNc = sort(fullfile({Dc.folder}, {Dc.name}));
        if numel(FNc) ~= numel(FNm)
            error('Mismatch: %d Mask files vs %d Cat files', numel(FNm), numel(FNc));
        end
    end

    Nfiles = numel(FNm);
    if Args.Verbose
        fprintf('Found %d file sets\n\n', Nfiles);
    end

    % --- Initialize per-check result tables ---
    % Each check reports three counts per file:
    %   MaskAndImage = condition present in both mask and image (agreement)
    %   MaskOnly     = bit set in mask but condition absent in image
    %   ImageOnly    = condition present in image but bit not in mask
    %
    % Bidirectional check (NearEdge):
    %   Pass requires ImageOnly==0 AND MaskOnly==0.
    % One-directional checks (NaN, Saturated, Negative, SrcDetected):
    %   Pass requires ImageOnly==0 only. MaskOnly is informational because
    %   mask bits are set during processing; corrections applied afterwards
    %   (interpolation, flat-fielding, etc.) may change pixel values.
    ResultCols  = {'FileName', 'SizeY', 'SizeX', ...
                   'MaskAndImage', 'MaskOnly', 'ImageOnly', 'Pass'};
    ResultTypes = {'string', 'double', 'double', ...
                   'double', 'double', 'double', 'logical'};

    if RunNearEdge;    NearEdgeResults    = table('Size', [Nfiles, 7], 'VariableTypes', ResultTypes, 'VariableNames', ResultCols); end
    if RunNaN;         NaNResults         = table('Size', [Nfiles, 7], 'VariableTypes', ResultTypes, 'VariableNames', ResultCols); end
    if RunSaturated;   SaturatedResults   = table('Size', [Nfiles, 7], 'VariableTypes', ResultTypes, 'VariableNames', ResultCols); end
    if RunNegative;    NegativeResults    = table('Size', [Nfiles, 7], 'VariableTypes', ResultTypes, 'VariableNames', ResultCols); end
    if RunSrcDetected
        SrcDetCols  = [ResultCols, {'FWHM_used'}];
        SrcDetTypes = [ResultTypes, {'double'}];
        SrcDetectedResults = table('Size', [Nfiles, 8], 'VariableTypes', SrcDetTypes, 'VariableNames', SrcDetCols);
    end

    % --- Main loop: process one file at a time to limit memory ---
    for Ifile = 1:Nfiles

        % Load mask
        MaskAI = AstroImage(FNm{Ifile});
        MaskData = uint32(MaskAI.Image);
        [SizeY, SizeX] = size(MaskData);

        if Args.Verbose
            fprintf('[%d/%d] %s (%dx%d)\n', Ifile, Nfiles, MaskNames{Ifile}, SizeY, SizeX);
        end

        % Load image if needed
        ImageData = [];
        if NeedImage
            ImAI = AstroImage(FNi{Ifile});
            ImageData = ImAI.Image;
        end

        % Load catalog if needed
        CatObj = [];
        if NeedCat
            CatObj = AstroCatalog(FNc{Ifile});
        end

        % --- 1. NearEdge check ---
        if RunNearEdge
            Res = checkNearEdge(MaskData, NearEdgeBitVal, Args.EdgeWidth, SizeY, SizeX);
            NearEdgeResults.FileName(Ifile)     = MaskNames{Ifile};
            NearEdgeResults.SizeY(Ifile)        = SizeY;
            NearEdgeResults.SizeX(Ifile)        = SizeX;
            NearEdgeResults.MaskAndImage(Ifile)  = Res.MaskAndImage;
            NearEdgeResults.MaskOnly(Ifile)     = Res.MaskOnly;
            NearEdgeResults.ImageOnly(Ifile)    = Res.ImageOnly;
            NearEdgeResults.Pass(Ifile)         = Res.Pass;
            printCheckResult(Args.Verbose, 'NearEdge', Res);
        end

        % --- 2. NaN check ---
        if RunNaN
            Res = checkNaNBit(MaskData, ImageData, NaNBitVal);
            NaNResults.FileName(Ifile)     = MaskNames{Ifile};
            NaNResults.SizeY(Ifile)        = SizeY;
            NaNResults.SizeX(Ifile)        = SizeX;
            NaNResults.MaskAndImage(Ifile)  = Res.MaskAndImage;
            NaNResults.MaskOnly(Ifile)     = Res.MaskOnly;
            NaNResults.ImageOnly(Ifile)    = Res.ImageOnly;
            NaNResults.Pass(Ifile)         = Res.Pass;
            printCheckResult(Args.Verbose, 'NaN', Res);
        end

        % --- 3. Saturated check (one-directional) ---
        if RunSaturated
            Res = checkSaturatedBit(MaskData, ImageData, SaturatedBitVal, Args.SatLevel);
            SaturatedResults.FileName(Ifile)     = MaskNames{Ifile};
            SaturatedResults.SizeY(Ifile)        = SizeY;
            SaturatedResults.SizeX(Ifile)        = SizeX;
            SaturatedResults.MaskAndImage(Ifile)  = Res.MaskAndImage;
            SaturatedResults.MaskOnly(Ifile)     = Res.MaskOnly;
            SaturatedResults.ImageOnly(Ifile)    = Res.ImageOnly;
            SaturatedResults.Pass(Ifile)         = Res.Pass;
            printCheckResult(Args.Verbose, 'Saturated', Res);
        end

        % --- 4. Negative check (one-directional) ---
        if RunNegative
            Res = checkNegativeBit(MaskData, ImageData, NegativeBitVal);
            NegativeResults.FileName(Ifile)     = MaskNames{Ifile};
            NegativeResults.SizeY(Ifile)        = SizeY;
            NegativeResults.SizeX(Ifile)        = SizeX;
            NegativeResults.MaskAndImage(Ifile)  = Res.MaskAndImage;
            NegativeResults.MaskOnly(Ifile)     = Res.MaskOnly;
            NegativeResults.ImageOnly(Ifile)    = Res.ImageOnly;
            NegativeResults.Pass(Ifile)         = Res.Pass;
            printCheckResult(Args.Verbose, 'Negative', Res);
        end

        % --- 5. SrcDetected check ---
        if RunSrcDetected
            FWHM = Args.FWHM;
            if isempty(FWHM) && ~isempty(CatObj) && CatObj.sizeCatalog > 0
                % Estimate FWHM from catalog 2nd moments
                FWHM = estimateFWHMfromCat(CatObj);
            end
            if isempty(FWHM)
                FWHM = 1;  % fallback default in pixels
                if Args.Verbose
                    fprintf('    SrcDetected: using default FWHM = %.1f pix\n', FWHM);
                end
            end
            Res = checkSrcDetectedBit(MaskData, CatObj, SrcDetectedBitVal, FWHM, SizeY, SizeX);
            SrcDetectedResults.FileName(Ifile)     = MaskNames{Ifile};
            SrcDetectedResults.SizeY(Ifile)        = SizeY;
            SrcDetectedResults.SizeX(Ifile)        = SizeX;
            SrcDetectedResults.MaskAndImage(Ifile)  = Res.MaskAndImage;
            SrcDetectedResults.MaskOnly(Ifile)     = Res.MaskOnly;
            SrcDetectedResults.ImageOnly(Ifile)    = Res.ImageOnly;
            SrcDetectedResults.Pass(Ifile)         = Res.Pass;
            SrcDetectedResults.FWHM_used(Ifile)    = FWHM;
            printCheckResult(Args.Verbose, 'SrcDetected', Res);
        end
    end

    % --- Build Report ---
    Report = struct();
    Report.Timestamp = datetime('now');
    Report.DataPath = DataPath;
    Report.FileType = FileType;
    Report.NumFiles = Nfiles;

    SummaryLines = {};
    SummaryLines{end+1} = 'Mask Consistency Validation Report';
    SummaryLines{end+1} = '====================================';
    SummaryLines{end+1} = sprintf('Data path: %s', DataPath);
    SummaryLines{end+1} = sprintf('File type: %s', FileType);
    SummaryLines{end+1} = sprintf('Files processed: %d', Nfiles);
    SummaryLines{end+1} = '';

    if RunNearEdge
        Report.NearEdge = struct();
        Report.NearEdge.BitIndex = 23;
        Report.NearEdge.EdgeWidth = Args.EdgeWidth;
        Report.NearEdge.CheckType = 'bidirectional';
        Report.NearEdge.Note = sprintf(['Bidirectional check against image geometry.\n', ...
            'Condition: pixel within EdgeWidth of image border\n', ...
            '  (X<=EdgeWidth or X>SizeX-EdgeWidth or Y<=EdgeWidth or Y>SizeY-EdgeWidth,\n', ...
            '   same as imUtil.ccdsec.selectNearEdges).\n', ...
            'MaskAndImage = edge pixel with NearEdge bit set.\n', ...
            'MaskOnly     = non-edge pixel with NearEdge bit set.\n', ...
            'ImageOnly    = edge pixel without NearEdge bit set.\n', ...
            'Pass requires ImageOnly==0 AND MaskOnly==0.']);
        Report.NearEdge.FileResults = NearEdgeResults;
        NPass = sum(NearEdgeResults.Pass);
        Report.NearEdge.NumPassed = NPass;
        SummaryLines{end+1} = sprintf('NearEdge (bit 23, width=%d): %d/%d passed', ...
                                      Args.EdgeWidth, NPass, Nfiles);
    end

    if RunNaN
        Report.NaN_Check = struct();
        Report.NaN_Check.BitIndex = 6;
        Report.NaN_Check.CheckType = 'one-directional';
        Report.NaN_Check.Note = sprintf(['One-directional check against processed image.\n', ...
            'Condition: isnan(ImageData).\n', ...
            'MaskAndImage = NaN pixel with NaN bit set.\n', ...
            'MaskOnly     = NaN bit set but pixel is not NaN in processed image.\n', ...
            '  (pixel may have been NaN then interpolated/corrected)\n', ...
            'ImageOnly    = NaN pixel in image without NaN bit in mask.\n', ...
            'Pass requires ImageOnly==0 only (MaskOnly is informational).']);
        Report.NaN_Check.FileResults = NaNResults;
        NPass = sum(NaNResults.Pass);
        Report.NaN_Check.NumPassed = NPass;
        SummaryLines{end+1} = sprintf('NaN (bit 6): %d/%d passed', NPass, Nfiles);
    end

    if RunSaturated
        Report.Saturated = struct();
        Report.Saturated.BitIndex = 0;
        Report.Saturated.SatLevel = Args.SatLevel;
        Report.Saturated.CheckType = 'one-directional';
        Report.Saturated.Note = sprintf(['One-directional check against processed image.\n', ...
            'Condition: ImageData > %d.\n', ...
            'MaskAndImage = pixel > %d with Saturated bit set.\n', ...
            'MaskOnly     = Saturated bit set but pixel <= %d in processed image.\n', ...
            '  (bit set from raw image before bias/flat processing)\n', ...
            'ImageOnly    = pixel > %d without Saturated bit in mask.\n', ...
            'Pass requires ImageOnly==0 only (MaskOnly is informational).'], ...
            Args.SatLevel, Args.SatLevel, Args.SatLevel, Args.SatLevel);
        Report.Saturated.FileResults = SaturatedResults;
        NPass = sum(SaturatedResults.Pass);
        Report.Saturated.NumPassed = NPass;
        SummaryLines{end+1} = sprintf('Saturated (bit 0, level=%d): %d/%d passed', ...
                                      Args.SatLevel, NPass, Nfiles);
    end

    if RunNegative
        Report.Negative = struct();
        Report.Negative.BitIndex = 10;
        Report.Negative.CheckType = 'one-directional';
        Report.Negative.Note = sprintf(['One-directional check against processed image.\n', ...
            'Condition: ImageData < 0.\n', ...
            'MaskAndImage = negative pixel with Negative bit set.\n', ...
            'MaskOnly     = Negative bit set but pixel >= 0 in processed image.\n', ...
            '  (bit may have been set before pixel was corrected)\n', ...
            'ImageOnly    = negative pixel without Negative bit in mask.\n', ...
            'Pass requires ImageOnly==0 only (MaskOnly is informational).']);
        Report.Negative.FileResults = NegativeResults;
        NPass = sum(NegativeResults.Pass);
        Report.Negative.NumPassed = NPass;
        SummaryLines{end+1} = sprintf('Negative (bit 10): %d/%d passed', NPass, Nfiles);
    end

    if RunSrcDetected
        Report.SrcDetected = struct();
        Report.SrcDetected.BitIndex = 30;
        Report.SrcDetected.CheckType = 'one-directional';
        Report.SrcDetected.Note = sprintf(['One-directional check against catalog source positions.\n', ...
            'Condition: pixel within 1xFWHM radius of a source (X1,Y1 from Cat).\n', ...
            'MaskAndImage = pixel near source with SrcDetected bit set.\n', ...
            'MaskOnly     = SrcDetected bit set but no source nearby.\n', ...
            'ImageOnly    = pixel near source without SrcDetected bit.\n', ...
            'Pass requires ImageOnly==0 only (MaskOnly is informational).\n', ...
            'Note: bit 30 is not yet implemented in the current pipeline;\n', ...
            'include ''SrcDetected'' in Checks explicitly to test.']);
        Report.SrcDetected.FileResults = SrcDetectedResults;
        NPass = sum(SrcDetectedResults.Pass);
        Report.SrcDetected.NumPassed = NPass;
        SummaryLines{end+1} = sprintf('SrcDetected (bit 30): %d/%d passed', NPass, Nfiles);
    end

    Report.Summary = strjoin(SummaryLines, '\n');

    if Args.Verbose
        fprintf('\n%s\n', Report.Summary);

        % Print tables for failed files
        CheckNames = {};
        CheckTables = {};
        if RunNearEdge && any(~NearEdgeResults.Pass)
            CheckNames{end+1} = 'NearEdge';
            CheckTables{end+1} = NearEdgeResults(~NearEdgeResults.Pass, :);
        end
        if RunNaN && any(~NaNResults.Pass)
            CheckNames{end+1} = 'NaN';
            CheckTables{end+1} = NaNResults(~NaNResults.Pass, :);
        end
        if RunSaturated && any(~SaturatedResults.Pass)
            CheckNames{end+1} = 'Saturated';
            CheckTables{end+1} = SaturatedResults(~SaturatedResults.Pass, :);
        end
        if RunNegative && any(~NegativeResults.Pass)
            CheckNames{end+1} = 'Negative';
            CheckTables{end+1} = NegativeResults(~NegativeResults.Pass, :);
        end
        if RunSrcDetected && any(~SrcDetectedResults.Pass)
            CheckNames{end+1} = 'SrcDetected';
            CheckTables{end+1} = SrcDetectedResults(~SrcDetectedResults.Pass, :);
        end

        for Ic = 1:numel(CheckNames)
            T = CheckTables{Ic};
            fprintf('\nFailed %s files (%d):\n', CheckNames{Ic}, height(T));
            if height(T) <= 20
                disp(T);
            else
                disp(T(1:20, :));
                fprintf('... and %d more.\n', height(T) - 20);
            end
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


%% ===== Local helper functions =====

function BitDecVal = getBitDecVal(BD, BitName)
    % Get the decimal value (2^BitIndex) for a named bit
    Idx = find(strcmp(BD.Dic.BitName, BitName));
    if isempty(Idx)
        error('Bit name ''%s'' not found in BitDictionary', BitName);
    end
    BitDecVal = uint32(2^BD.Dic.BitInd(Idx));
end


function printCheckResult(Verbose, CheckName, Res)
    % Print a single check result line (same order for PASS and FAIL)
    if Verbose
        if Res.Pass
            fprintf('    %-12s [PASS] MaskAndImage=%d, MaskOnly=%d, ImageOnly=%d\n', ...
                    CheckName, Res.MaskAndImage, Res.MaskOnly, Res.ImageOnly);
        else
            fprintf('    %-12s [FAIL] MaskAndImage=%d, MaskOnly=%d, ImageOnly=%d\n', ...
                    CheckName, Res.MaskAndImage, Res.MaskOnly, Res.ImageOnly);
        end
    end
end


function Res = checkNearEdge(MaskData, BitVal, EdgeWidth, SizeY, SizeX)
    % Check NearEdge bit: edge pixels within EdgeWidth of border.
    % Uses same logic as imUtil.ccdsec.selectNearEdges (Algo 1):
    %   X <= EdgeWidth OR X > SizeX - EdgeWidth
    %   Y <= EdgeWidth OR Y > SizeY - EdgeWidth

    ExpectedEdge = false(SizeY, SizeX);
    ExpectedEdge(1:EdgeWidth, :) = true;                    % top rows
    ExpectedEdge(end-EdgeWidth+1:end, :) = true;            % bottom rows
    ExpectedEdge(:, 1:EdgeWidth) = true;                    % left columns
    ExpectedEdge(:, end-EdgeWidth+1:end) = true;            % right columns

    ActualEdge = bitand(MaskData, BitVal) > 0;

    Res.MaskAndImage = sum(ExpectedEdge & ActualEdge, 'all');
    Res.MaskOnly     = sum(~ExpectedEdge & ActualEdge, 'all');
    Res.ImageOnly    = sum(ExpectedEdge & ~ActualEdge, 'all');
    Res.Pass         = (Res.ImageOnly == 0) && (Res.MaskOnly == 0);
end


function Res = checkNaNBit(MaskData, ImageData, BitVal)
    % Check NaN bit (one-directional).
    % ImageOnly: pixel is NaN in processed image but no NaN bit in mask.
    % MaskOnly: NaN bit set but pixel is not NaN in processed image.
    %   MaskOnly is informational: the pixel may have been NaN at an
    %   earlier stage and then interpolated/corrected.

    ImageCond = isnan(ImageData);
    MaskBit   = bitand(MaskData, BitVal) > 0;

    Res.MaskAndImage = sum(ImageCond & MaskBit, 'all');
    Res.MaskOnly     = sum(~ImageCond & MaskBit, 'all');
    Res.ImageOnly    = sum(ImageCond & ~MaskBit, 'all');
    Res.Pass         = (Res.ImageOnly == 0);
end


function Res = checkSaturatedBit(MaskData, ImageData, BitVal, SatLevel)
    % Check Saturated bit (one-directional).
    % ImageOnly: pixel > SatLevel in processed image but no Saturated bit.
    % MaskOnly: Saturated bit set but pixel <= SatLevel in processed image.
    %   MaskOnly is informational: the bit is set from the raw image
    %   before bias subtraction and flat-fielding.

    ImageCond = ImageData > SatLevel;
    MaskBit   = bitand(MaskData, BitVal) > 0;

    Res.MaskAndImage = sum(ImageCond & MaskBit, 'all');
    Res.MaskOnly     = sum(~ImageCond & MaskBit, 'all');
    Res.ImageOnly    = sum(ImageCond & ~MaskBit, 'all');
    Res.Pass         = (Res.ImageOnly == 0);
end


function Res = checkNegativeBit(MaskData, ImageData, BitVal)
    % Check Negative bit (one-directional).
    % ImageOnly: pixel < 0 in processed image but no Negative bit.
    % MaskOnly: Negative bit set but pixel >= 0 in processed image.
    %   MaskOnly is informational: the bit may have been set at an earlier
    %   processing step before the pixel was corrected.

    ImageCond = ImageData < 0;
    MaskBit   = bitand(MaskData, BitVal) > 0;

    Res.MaskAndImage = sum(ImageCond & MaskBit, 'all');
    Res.MaskOnly     = sum(~ImageCond & MaskBit, 'all');
    Res.ImageOnly    = sum(ImageCond & ~MaskBit, 'all');
    Res.Pass         = (Res.ImageOnly == 0);
end


function Res = checkSrcDetectedBit(MaskData, CatObj, BitVal, FWHM, SizeY, SizeX)
    % Check SrcDetected bit (one-directional): all pixels within 1xFWHM
    % radius of a detected source (from catalog) should have the bit set.

    MaskBit = bitand(MaskData, BitVal) > 0;

    if isempty(CatObj) || CatObj.sizeCatalog == 0
        % No sources in catalog
        Res.MaskAndImage = 0;
        Res.MaskOnly     = sum(MaskBit, 'all');
        Res.ImageOnly    = 0;
        Res.Pass         = true;
        return;
    end

    % Get source positions from catalog (use X1,Y1 as centroid)
    X1 = tryGetCol(CatObj, {'X1', 'X', 'XPEAK'});
    Y1 = tryGetCol(CatObj, {'Y1', 'Y', 'YPEAK'});

    if isempty(X1) || isempty(Y1)
        warning('Cannot find X/Y columns in catalog; skipping SrcDetected check');
        Res.MaskAndImage = -1;
        Res.MaskOnly     = -1;
        Res.ImageOnly    = -1;
        Res.Pass         = false;
        return;
    end

    % Build expected SrcDetected mask: mark all pixels within FWHM of
    % any source position
    ImageCond = false(SizeY, SizeX);
    Radius = FWHM;
    RadiusSq = Radius^2;

    for Isrc = 1:numel(X1)
        Xc = X1(Isrc);
        Yc = Y1(Isrc);

        % Bounding box (clipped to image)
        Xmin = max(1, floor(Xc - Radius));
        Xmax = min(SizeX, ceil(Xc + Radius));
        Ymin = max(1, floor(Yc - Radius));
        Ymax = min(SizeY, ceil(Yc + Radius));

        % Mark pixels within circular aperture
        [GridX, GridY] = meshgrid(Xmin:Xmax, Ymin:Ymax);
        DistSq = (GridX - Xc).^2 + (GridY - Yc).^2;
        InCircle = DistSq <= RadiusSq;
        ImageCond(Ymin:Ymax, Xmin:Xmax) = ImageCond(Ymin:Ymax, Xmin:Xmax) | InCircle;
    end

    Res.MaskAndImage = sum(ImageCond & MaskBit, 'all');
    Res.MaskOnly     = sum(~ImageCond & MaskBit, 'all');
    Res.ImageOnly    = sum(ImageCond & ~MaskBit, 'all');
    Res.Pass         = (Res.ImageOnly == 0);
end


function FWHM = estimateFWHMfromCat(CatObj)
    % Estimate FWHM in pixels from catalog 2nd moment columns X2, Y2.
    % FWHM = 2.355 * sigma, sigma = sqrt(moment)

    X2 = tryGetCol(CatObj, {'X2'});
    Y2 = tryGetCol(CatObj, {'Y2'});

    if ~isempty(X2) && ~isempty(Y2)
        % Use geometric mean of X and Y moments
        MedX2 = median(X2(X2 > 0), 'omitnan');
        MedY2 = median(Y2(Y2 > 0), 'omitnan');
        SigmaMean = sqrt(sqrt(MedX2 * MedY2));
        FWHM = 2.355 * SigmaMean;
    else
        FWHM = [];
    end
end


function ColData = tryGetCol(CatObj, ColNames)
    % Try to get a column from catalog, trying multiple name variants.
    % Returns empty if none found.
    ColData = [];
    for Ic = 1:numel(ColNames)
        try
            Data = CatObj.getCol(ColNames{Ic});
            if ~isempty(Data)
                ColData = Data;
                return;
            end
        catch
            % Column not found, try next
        end
    end
end
