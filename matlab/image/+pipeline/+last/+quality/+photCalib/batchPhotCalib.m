function R = batchPhotCalib(BaseDir, Args)
    % Run PhotCalibTrans calibration over multiple visit subdirectories
    % Description: Discovers visit subdirectories under BaseDir, loads each
    %              with pipeline.last.load.loadVisit (full AstroImage with
    %              .Image populated), calibrates with
    %              imProc.calib.fitPhotCalibTrans, and aggregates the
    %              resulting PhotCalibTrans objects into a single flat
    %              array (one entry per AstroImage processed).
    %              When CalibArgs contains 'EvaluateBackMag',true the
    %              function calls imProc.background.background after load
    %              to populate .Back (LAST coadd files do not store Back
    %              on disk, so it must be recomputed for BACKMAG to work).
    %              Designed for diagnostic surveys across many fields/visits
    %              (e.g. for plotPhotParamHist over Chi2_DOF, NCalib, etc.).
    %
    % Input  : - BaseDir char/string — parent directory containing visit
    %            subfolders (e.g. /data/.../2025/07/08/).
    %          * ...,key,val,...
    %            'VisitGlob' - Pattern matching visit subdirectories under
    %                          BaseDir. Default '*v*' (e.g. 000029v1, 012334v1).
    %            'Recursive' - If true, search for matching visit dirs at any
    %                          depth under BaseDir; otherwise only direct
    %                          children of BaseDir. Default false.
    %            'FileType'  - 'coadd' or 'proc'. Default 'coadd'.
    %            'CalibArgs' - Cell of NV pairs forwarded to fitPhotCalibTrans
    %                          (e.g. {'CalibArgs', {'MagRange', [12 18]}}).
    %                          Default {}.
    %            'OutFile'   - Path to save the aggregated result struct.
    %                          Default '' (no save).
    %            'Verbose'   - Print per-visit progress. Default false.
    % Output : - R - struct with fields:
    %            .PC         - flat [1 x Ntotal] PhotCalibTrans array.
    %            .VisitName  - {1 x Ntotal} cell of visit subdir names.
    %            .AIIndex    - [1 x Ntotal] index into the per-visit AI array
    %                          (useful for cross-referencing back to files).
    %            .Success    - [1 x Ntotal] logical, .Success per PC.
    %            .VisitDirs  - {1 x Nvisits} list of full paths processed.
    %            .NTotal, .NSuccess, .NFailed - counts.
    %            .FileType   - echo of Args.FileType.
    %            .Args       - echo of input arguments.
    % Author : D. Kovaleva (Apr 2026)
    % Example: % Coadd over 5 visits (one date dir, direct children):
    %          R = pipeline.last.quality.photCalib.batchPhotCalib( ...
    %              '/bigdata2/.../2025/07/08', ...
    %              'FileType', 'coadd', 'OutFile', '~/results/batch_coadd.mat');
    %          pipeline.last.quality.photCalib.plotPhotParamHist(R.PC, 'Param', 'Chi2_DOF');
    %
    %          % Recursive: every *v[0-9]* visit anywhere under a year tree:
    %          R = pipeline.last.quality.photCalib.batchPhotCalib( ...
    %              '/archimedes/LASTunitTest/2025', ...
    %              'VisitGlob', '*v[0-9]*', 'Recursive', true, ...
    %              'FileType', 'coadd');
    %
    %          % Proc over a single visit (development cycle):
    %          R = pipeline.last.quality.photCalib.batchPhotCalib( ...
    %              '/bigdata2/.../2025/07/08', ...
    %              'VisitGlob', '012334v1', 'FileType', 'proc');

    arguments
        BaseDir         {mustBeText}
        Args.VisitGlob  {mustBeText}          = '*v*'
        Args.Recursive  logical               = false
        Args.FileType   {mustBeMember(Args.FileType, {'coadd','proc'})} = 'coadd'
        Args.CalibArgs  cell                  = {}
        Args.OutFile    {mustBeText}          = ''
        Args.Verbose    logical               = false
    end

    BaseDir = char(BaseDir);

    % Discover visit subdirectories
    if Args.Recursive
        Listing = dir(fullfile(BaseDir, '**', Args.VisitGlob));
    else
        Listing = dir(fullfile(BaseDir, Args.VisitGlob));
    end
    Listing = Listing([Listing.isdir] & ~startsWith({Listing.name}, '.'));

    if isempty(Listing)
        ScopeStr = BaseDir;
        if Args.Recursive; ScopeStr = [ScopeStr ' (recursive)']; end
        error('batchPhotCalib:NoVisits', ...
            'No subdirectories matching "%s" found under %s', ...
            Args.VisitGlob, ScopeStr);
    end

    Nvisits = numel(Listing);
    VisitDirs = cell(Nvisits, 1);
    for I = 1:Nvisits
        VisitDirs{I} = fullfile(Listing(I).folder, Listing(I).name);
    end

    if Args.Verbose
        fprintf('batchPhotCalib: %d visit dirs under %s (FileType=%s)\n', ...
            Nvisits, BaseDir, Args.FileType);
    end

    % Detect whether the user asked for BackMag — if so we need to
    % populate AI.Back via imProc.background.background after loading,
    % since loadVisit/readProducts does not load Back from disk.
    NeedBack = false;
    if ~isempty(Args.CalibArgs)
        KeyIdx = find(strcmpi(Args.CalibArgs(1:2:end), 'EvaluateBackMag'), 1, 'last');
        if ~isempty(KeyIdx)
            NeedBack = isequal(Args.CalibArgs{2*KeyIdx}, true);
        end
    end

    % Per-visit accumulators (flatten at end)
    PCcell        = cell(1, Nvisits);
    VisitNameCell = cell(1, Nvisits);
    AIIndexCell   = cell(1, Nvisits);
    SuccessCell   = cell(1, Nvisits);

    for Iv = 1:Nvisits
        VName = Listing(Iv).name;
        Tstart = tic;

        % Load visit data (full AstroImage with Image; .Back is populated
        % below via imProc.background.background only if BackMag is requested,
        % since neither readProducts nor LAST coadd files carry .Back).
        AIvis = AstroImage.empty;
        try
            switch Args.FileType
                case 'coadd'
                    [~, AIvis, ~] = pipeline.last.load.loadVisit( ...
                        VisitDirs{Iv}, ...
                        'TempName_IndivIm', '', ...
                        'TempName_MS',      '');
                case 'proc'
                    [AIvis, ~, ~] = pipeline.last.load.loadVisit( ...
                        VisitDirs{Iv}, ...
                        'TempName_Coadd', '', ...
                        'TempName_MS',    '');
            end
        catch ME
            if Args.Verbose
                fprintf('  [%2d/%2d] %s : load failed (%s)\n', ...
                    Iv, Nvisits, VName, ME.message);
            end
            continue;
        end

        if isempty(AIvis)
            if Args.Verbose
                fprintf('  [%2d/%2d] %s : no %s files\n', ...
                    Iv, Nvisits, VName, Args.FileType);
            end
            continue;
        end

        % Flatten to 1 x Nimg (proc returns reshaped 2D; coadd is already 1D)
        AIvis = AIvis(:).';

        % Populate .Back if BackMag was requested. SubSizeXY=[] matches
        % the production setting in pipeline.last.coadd.coadd:189 so
        % BackMag values reproduce the pipeline's. Failures are logged
        % but non-fatal: BACKMAG will fall through to NaN downstream.
        if NeedBack
            try
                AIvis = imProc.background.background(AIvis, 'SubSizeXY', []);
            catch ME
                if Args.Verbose
                    fprintf('  [%2d/%2d] %s : background failed (%s) - BACKMAG will be NaN\n', ...
                        Iv, Nvisits, VName, ME.message);
                end
            end
        end

        % Calibrate
        PCv = [];
        try
            [~, PCv] = imProc.calib.fitPhotCalibTrans(AIvis, ...
                'Verbose', false, Args.CalibArgs{:});
        catch ME
            if Args.Verbose
                fprintf('  [%2d/%2d] %s : fit failed (%s)\n', ...
                    Iv, Nvisits, VName, ME.message);
            end
            continue;
        end

        % Per-visit metadata
        Nimg = numel(PCv);
        PCcell{Iv}        = PCv;
        VisitNameCell{Iv} = repmat({VName}, 1, Nimg);
        AIIndexCell{Iv}   = 1:Nimg;
        SuccessCell{Iv}   = arrayfun(@(pc) pc.Success, PCv);

        if Args.Verbose
            NS = sum(SuccessCell{Iv});
            fprintf('  [%2d/%2d] %s : %d/%d success, %.1f s\n', ...
                Iv, Nvisits, VName, NS, Nimg, toc(Tstart));
        end
    end

    % Flatten across visits
    R.PC        = [PCcell{:}];
    R.VisitName = [VisitNameCell{:}];
    R.AIIndex   = [AIIndexCell{:}];
    R.Success   = [SuccessCell{:}];
    R.VisitDirs = VisitDirs;
    R.FileType  = Args.FileType;
    R.NTotal    = numel(R.PC);
    R.NSuccess  = sum(R.Success);
    R.NFailed   = R.NTotal - R.NSuccess;
    R.Args      = Args;

    if Args.Verbose
        fprintf('batchPhotCalib: total %d PCs (%d success, %d failed) over %d visits\n', ...
            R.NTotal, R.NSuccess, R.NFailed, Nvisits);
    end

    % Save aggregated result if requested
    if ~isempty(Args.OutFile)
        try
            save(char(Args.OutFile), 'R', '-v7.3');
            if Args.Verbose
                fprintf('Saved %s\n', Args.OutFile);
            end
        catch ME
            warning('batchPhotCalib:SaveFailed', ...
                'Failed to save %s: %s', Args.OutFile, ME.message);
        end
    end
end
