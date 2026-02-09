function Report = checkMaskPropagationToCoadd(Input, Args)
    % Check that sci_proc mask bits are properly propagated to sci_coadd
    % Description: Two-directional check:
    %   1. Proc->Coadd: For EACH individual proc mask file, verifies that
    %      every bit present in the proc mask is also set in the coadd mask.
    %      Reports per-proc-file results (ProcFileResults).
    %   2. Coadd->Procs: For each coadd mask, computes the bitwise OR of all
    %      parent proc masks and verifies that every bit in the coadd is
    %      present in at least one proc. Reports per-coadd results
    %      (CoaddFileResults).
    %   Files are matched by mount_subframe_CCD key extracted from filenames.
    % Input  : - Input: path string to directory containing both sci_proc
    %            and sci_coadd FITS mask files.
    %          * ...,key,val,...
    %            'BitDictName' - BitDictionary name for per-bit breakdown.
    %                            Default is 'BitMask.Image.Default'.
    %            'ReportFile'  - Optional file path to save report (.mat).
    %                            Default is '' (no save).
    %            'Verbose'     - Print progress to console. Default is true.
    % Output : - Report: struct with fields:
    %            .Timestamp      - datetime of validation run
    %            .DataPath       - path to data
    %            .NumCoadds      - number of coadd groups
    %            .NumProcsTotal  - total proc files matched
    %            .ProcFileResults - table with per-proc results:
    %              FileKey          - part between _clear_ and _sci_ in filename
    %              SizeY, SizeX     - mask size
    %              ProcAndCoadd     - pixels with bits in both proc and coadd
    %              ProcOnly         - bits in proc but missing in coadd (failure)
    %              ProcOnlyBitNames - cell array of bit names missing in coadd
    %              Pass             - ProcOnly == 0
    %            .CoaddFileResults - table with per-coadd results:
    %              CoaddFile      - coadd mask filename
    %              NumProcs       - number of parent proc files
    %              SizeY, SizeX   - coadd mask size
    %              CoaddAndProcs  - pixels with bits in both coadd and proc OR
    %              CoaddOnly      - bits in coadd but not in any proc (informational)
    %            .ProcBitBreakdown  - per-bit missing counts (proc->coadd direction)
    %            .Summary       - text summary
    %            .Note          - description of check logic
    % Author : Dana Kovaleva (Feb 2026)
    % Example: DataPath = '/bigdata2/projects/last/testNewPipe/222625v1/';
    %          Report = pipeline.last.quality.checkMaskPropagationToCoadd(DataPath);

    arguments
        Input
        Args.BitDictName    = 'BitMask.Image.Default';
        Args.ReportFile     = '';
        Args.Verbose        = true;
    end

    DataPath = char(Input);

    if Args.Verbose
        fprintf('Data path: %s\n', DataPath);
    end

    % Find all proc and coadd mask files
    DmProc  = dir(fullfile(DataPath, '*_sci_proc_Mask_*.fits'));
    DmCoadd = dir(fullfile(DataPath, '*_sci_coadd_Mask_*.fits'));

    if isempty(DmProc)
        error('No sci_proc mask files found in %s', DataPath);
    end
    if isempty(DmCoadd)
        error('No sci_coadd mask files found in %s', DataPath);
    end

    if Args.Verbose
        fprintf('Found %d sci_proc mask files, %d sci_coadd mask files\n', ...
                numel(DmProc), numel(DmCoadd));
    end

    % Parse filenames to extract matching key: mount_subframe_CCD
    % Filename pattern: ..._COUNTER_MOUNT_SUBFRAME_sci_TYPE_Mask_CCD.fits
    KeyPattern = '_(\d{3})_(\d{3})_(\d{3})_sci_(proc|coadd)_Mask_(\d+)\.fits$';
    % Pattern to extract file key: part between _clear_ and _sci_
    FileKeyPattern = '_clear_(.+)_sci_';

    % Build proc key map: key -> list of file indices
    ProcKeyMap = containers.Map();
    for I = 1:numel(DmProc)
        Tokens = regexp(DmProc(I).name, KeyPattern, 'tokens');
        if ~isempty(Tokens)
            % Key = mount_subframe_CCD (groups 2, 3, 5)
            Key = sprintf('%s_%s_%s', Tokens{1}{2}, Tokens{1}{3}, Tokens{1}{5});
            if ProcKeyMap.isKey(Key)
                ProcKeyMap(Key) = [ProcKeyMap(Key), I];
            else
                ProcKeyMap(Key) = I;
            end
        end
    end

    % Build coadd key map: key -> file index
    CoaddKeyMap = containers.Map();
    for I = 1:numel(DmCoadd)
        Tokens = regexp(DmCoadd(I).name, KeyPattern, 'tokens');
        if ~isempty(Tokens)
            Key = sprintf('%s_%s_%s', Tokens{1}{2}, Tokens{1}{3}, Tokens{1}{5});
            CoaddKeyMap(Key) = I;
        end
    end

    CoaddKeys = sort(keys(CoaddKeyMap));
    Ncoadds = numel(CoaddKeys);

    if Args.Verbose
        fprintf('Matched %d coadd groups\n\n', Ncoadds);
    end

    % Initialize BitDictionary for per-bit breakdown
    BD = BitDictionary(Args.BitDictName);
    Nbits = numel(BD.Dic.BitInd);

    % Count total proc files across all coadd groups
    TotalProcs = 0;
    for Ik = 1:Ncoadds
        Key = CoaddKeys{Ik};
        if ProcKeyMap.isKey(Key)
            TotalProcs = TotalProcs + numel(ProcKeyMap(Key));
        end
    end

    % --- Initialize per-proc results table (Direction 1: Proc->Coadd) ---
    ProcResultCols  = {'FileKey', 'SizeY', 'SizeX', ...
                       'ProcAndCoadd', 'ProcOnly', 'ProcOnlyBitNames', 'Pass'};
    ProcResultTypes = {'string', 'double', 'double', ...
                       'double', 'double', 'cell', 'logical'};
    ProcFileResults = table('Size', [TotalProcs, 7], ...
        'VariableTypes', ProcResultTypes, 'VariableNames', ProcResultCols);

    % --- Initialize per-coadd results table (Direction 2: Coadd->Procs) ---
    CoaddResultCols  = {'CoaddFile', 'NumProcs', 'SizeY', 'SizeX', ...
                        'CoaddAndProcs', 'CoaddOnly'};
    CoaddResultTypes = {'string', 'double', 'double', 'double', ...
                        'double', 'double'};
    CoaddFileResults = table('Size', [Ncoadds, 6], ...
        'VariableTypes', CoaddResultTypes, 'VariableNames', CoaddResultCols);

    % Per-bit pixel counters for Proc->Coadd direction
    ProcBitPropagated = zeros(Nbits, 1);  % properly propagated (in both)
    ProcBitProcOnly   = zeros(Nbits, 1);  % in proc but missing in coadd

    ProcRow = 0;  % running index into ProcFileResults

    % --- Main loop over coadd groups ---
    for Ik = 1:Ncoadds
        Key = CoaddKeys{Ik};
        CoaddIdx = CoaddKeyMap(Key);

        CoaddFile = fullfile(DmCoadd(CoaddIdx).folder, DmCoadd(CoaddIdx).name);
        CoaddAI = AstroImage(CoaddFile);
        CoaddMask = uint32(CoaddAI.Image);
        [SizeY, SizeX] = size(CoaddMask);

        % Find matching proc files
        if ~ProcKeyMap.isKey(Key)
            if Args.Verbose
                fprintf('[%d/%d] %s - NO MATCHING PROC FILES\n', Ik, Ncoadds, DmCoadd(CoaddIdx).name);
            end
            CoaddFileResults.CoaddFile(Ik)     = DmCoadd(CoaddIdx).name;
            CoaddFileResults.NumProcs(Ik)      = 0;
            CoaddFileResults.SizeY(Ik)         = SizeY;
            CoaddFileResults.SizeX(Ik)         = SizeX;
            CoaddFileResults.CoaddAndProcs(Ik) = 0;
            CoaddFileResults.CoaddOnly(Ik)     = sum(CoaddMask > 0, 'all');
            continue;
        end

        ProcIndices = ProcKeyMap(Key);
        Nprocs = numel(ProcIndices);

        if Args.Verbose
            fprintf('[%d/%d] %s (%d procs, %dx%d)\n', ...
                    Ik, Ncoadds, DmCoadd(CoaddIdx).name, Nprocs, SizeY, SizeX);
        end

        % --- Direction 1: Check EACH proc file individually vs coadd ---
        ProcOR = uint32(zeros(SizeY, SizeX));

        for Ip = 1:Nprocs
            ProcFile = fullfile(DmProc(ProcIndices(Ip)).folder, DmProc(ProcIndices(Ip)).name);
            ProcAI = AstroImage(ProcFile);
            ProcMask = uint32(ProcAI.Image);
            [ProcSizeY, ProcSizeX] = size(ProcMask);

            ProcRow = ProcRow + 1;

            FKTokens = regexp(DmProc(ProcIndices(Ip)).name, FileKeyPattern, 'tokens');
            if ~isempty(FKTokens); FK = FKTokens{1}{1}; else; FK = DmProc(ProcIndices(Ip)).name; end

            if ProcSizeY ~= SizeY || ProcSizeX ~= SizeX
                warning('Size mismatch: proc %dx%d vs coadd %dx%d in %s', ...
                        ProcSizeY, ProcSizeX, SizeY, SizeX, FK);
                ProcFileResults.FileKey(ProcRow)          = FK;
                ProcFileResults.SizeY(ProcRow)            = ProcSizeY;
                ProcFileResults.SizeX(ProcRow)            = ProcSizeX;
                ProcFileResults.ProcAndCoadd(ProcRow)     = -1;
                ProcFileResults.ProcOnly(ProcRow)         = -1;
                ProcFileResults.ProcOnlyBitNames{ProcRow} = {'SizeMismatch'};
                ProcFileResults.Pass(ProcRow)             = false;
                continue;
            end

            % Accumulate OR for Direction 2
            ProcOR = bitor(ProcOR, ProcMask);

            % Per-proc check: every bit in proc must be in coadd
            ProcOnlyMask = bitand(ProcMask, bitcmp(CoaddMask));
            BothMask     = bitand(ProcMask, CoaddMask);

            NumProcAndCoadd = sum(BothMask > 0, 'all');
            NumProcOnly     = sum(ProcOnlyMask > 0, 'all');

            % Per-bit breakdown: propagated and ProcOnly
            MissingBitNames = {};
            for Ibit = 1:Nbits
                BitVal = uint32(2^BD.Dic.BitInd(Ibit));
                NpropThisBit  = sum(bitand(BothMask, BitVal) > 0, 'all');
                NprocOnlyBit  = sum(bitand(ProcOnlyMask, BitVal) > 0, 'all');
                ProcBitPropagated(Ibit) = ProcBitPropagated(Ibit) + NpropThisBit;
                ProcBitProcOnly(Ibit)   = ProcBitProcOnly(Ibit) + NprocOnlyBit;
                if NprocOnlyBit > 0
                    MissingBitNames{end+1} = BD.Dic.BitName{Ibit}; %#ok<AGROW>
                end
            end

            ProcFileResults.FileKey(ProcRow)          = FK;
            ProcFileResults.SizeY(ProcRow)            = SizeY;
            ProcFileResults.SizeX(ProcRow)            = SizeX;
            ProcFileResults.ProcAndCoadd(ProcRow)     = NumProcAndCoadd;
            ProcFileResults.ProcOnly(ProcRow)         = NumProcOnly;
            ProcFileResults.ProcOnlyBitNames{ProcRow} = MissingBitNames;
            ProcFileResults.Pass(ProcRow)             = (NumProcOnly == 0);

            if Args.Verbose
                if NumProcOnly == 0
                    fprintf('    [PASS] %s: ProcAndCoadd=%d, ProcOnly=%d\n', ...
                            FK, NumProcAndCoadd, NumProcOnly);
                else
                    fprintf('    [FAIL] %s: ProcAndCoadd=%d, ProcOnly=%d, bits: %s\n', ...
                            FK, NumProcAndCoadd, NumProcOnly, ...
                            strjoin(MissingBitNames, ', '));
                end
            end
        end

        % --- Direction 2: Check coadd vs bitwise OR of all procs ---
        CoaddOnlyMask  = bitand(CoaddMask, bitcmp(ProcOR));
        BothMaskCoadd  = bitand(CoaddMask, ProcOR);

        NumCoaddAndProcs = sum(BothMaskCoadd > 0, 'all');
        NumCoaddOnly     = sum(CoaddOnlyMask > 0, 'all');

        CoaddFileResults.CoaddFile(Ik)     = DmCoadd(CoaddIdx).name;
        CoaddFileResults.NumProcs(Ik)      = Nprocs;
        CoaddFileResults.SizeY(Ik)         = SizeY;
        CoaddFileResults.SizeX(Ik)         = SizeX;
        CoaddFileResults.CoaddAndProcs(Ik) = NumCoaddAndProcs;
        CoaddFileResults.CoaddOnly(Ik)     = NumCoaddOnly;

        if Args.Verbose
            fprintf('    Coadd->Procs: CoaddAndProcs=%d, CoaddOnly=%d\n', ...
                    NumCoaddAndProcs, NumCoaddOnly);
        end
    end

    % Trim ProcFileResults to actual number of rows
    ProcFileResults = ProcFileResults(1:ProcRow, :);

    % --- Build per-bit breakdown table ---
    ProcBitBreakdown = table(BD.Dic.BitName(:), BD.Dic.BitInd(:), ProcBitPropagated, ProcBitProcOnly, ...
        'VariableNames', {'BitName', 'BitIndex', 'ProcAndCoaddPixels', 'ProcOnlyPixels'});
    ProcBitBreakdown = sortrows(ProcBitBreakdown, 'ProcOnlyPixels', 'descend');

    % --- Build Report ---
    NProcPass = sum(ProcFileResults.Pass);
    TotalProcOnly = sum(ProcFileResults.ProcOnly(ProcFileResults.ProcOnly >= 0));
    TotalCoaddOnly = sum(CoaddFileResults.CoaddOnly);

    Report = struct();
    Report.Timestamp = datetime('now');
    Report.DataPath = DataPath;
    Report.NumCoadds = Ncoadds;
    Report.NumProcsTotal = TotalProcs;
    Report.ProcFileResults = ProcFileResults;
    Report.CoaddFileResults = CoaddFileResults;
    Report.ProcBitBreakdown = ProcBitBreakdown;

    SummaryLines = {};
    SummaryLines{end+1} = 'Mask Propagation to Coadd Report';
    SummaryLines{end+1} = '==================================';
    SummaryLines{end+1} = sprintf('Data path: %s', DataPath);
    SummaryLines{end+1} = sprintf('Coadd groups: %d', Ncoadds);
    SummaryLines{end+1} = sprintf('Total proc files: %d', TotalProcs);
    SummaryLines{end+1} = '';
    SummaryLines{end+1} = 'Direction 1: Proc -> Coadd (per-proc-file)';
    SummaryLines{end+1} = sprintf('  Passed: %d / %d', NProcPass, TotalProcs);
    SummaryLines{end+1} = sprintf('  Total ProcOnly pixels (missing propagation): %d', TotalProcOnly);
    SummaryLines{end+1} = '';
    SummaryLines{end+1} = 'Direction 2: Coadd -> Procs (per-coadd bulk)';
    SummaryLines{end+1} = sprintf('  Total CoaddOnly pixels (not from any proc): %d', TotalCoaddOnly);

    Report.Summary = strjoin(SummaryLines, '\n');

    Report.Note = sprintf(['Two-directional mask propagation check.\n\n', ...
        'Direction 1 (Proc->Coadd): For each individual proc mask file,\n', ...
        '  checks that every bit at each pixel is also present in the\n', ...
        '  corresponding coadd mask. ProcOnly = bits in proc but missing\n', ...
        '  in coadd (failure). Pass requires ProcOnly==0 per proc file.\n\n', ...
        'Direction 2 (Coadd->Procs): For each coadd mask, computes the\n', ...
        '  bitwise OR of all parent proc masks and checks that every bit\n', ...
        '  in the coadd is present in at least one proc. CoaddOnly = bits\n', ...
        '  in coadd but not in any proc (informational; coadd may add\n', ...
        '  bits like CoaddLessImages).\n\n', ...
        'Proc files matched to coadd by mount_subframe_CCD key.']);

    if Args.Verbose
        fprintf('\n%s\n', Report.Summary);

        % Print failed proc files
        FailedProcs = ProcFileResults(~ProcFileResults.Pass, :);
        if height(FailedProcs) > 0
            fprintf('\nFailed proc files (%d):\n', height(FailedProcs));
            if height(FailedProcs) <= 20
                disp(FailedProcs);
            else
                disp(FailedProcs(1:20, :));
                fprintf('... and %d more.\n', height(FailedProcs) - 20);
            end
        end

        % Print coadd summary (CoaddOnly is expected, e.g. CoaddLessImages)
        fprintf('\nCoadd->Procs summary (CoaddOnly is expected, not a failure):\n');
        disp(CoaddFileResults);

        % Print per-bit breakdown (non-zero only)
        NonZeroBits = ProcBitBreakdown(ProcBitBreakdown.ProcOnlyPixels > 0, :);
        if height(NonZeroBits) > 0
            fprintf('\nPer-bit missing pixel breakdown (Proc->Coadd):\n');
            disp(NonZeroBits);
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
