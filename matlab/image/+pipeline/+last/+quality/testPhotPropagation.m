function Result = testPhotPropagation(Args)
    % Test propagation of coadd photometric calibration to individual epochs
    % Description: Two branches:
    %
    %   MULTI-EPOCH (default): Loads coadd PhotCalibTrans, MergedMat
    %   MatchedSources, and proc epoch AstroImages from a visit folder.
    %   Propagates coadd calibration to each epoch using
    %   PhotCalibTrans.propagatePhotCalibToEpoch. Computes MAG_AB directly
    %   from MS fluxes (using propagated per-epoch PC + MS X/Y) and injects
    %   into MS.Data. Compares stability of propagated MAG_AB vs relative
    %   photometry.
    %
    %   SINGLE-EPOCH (when EpochAI is set): Applies coadd PC directly to
    %   one epoch's AstroImage catalog. No MergedMat, no DeltaZP — just
    %   adjusts NCoadd=1 + ExpTime from header + addMag (single evaluateZP
    %   pass via AddZP=true). Enhances the epoch catalog and header in place.
    %
    % Input  : * ...,key,val,...
    %          --- Data (multi-epoch) ---
    %            'VisitDir'   - Visit folder containing coadd, proc, and
    %                          MergedMat files. When set, auto-detects all.
    %                          Default is ''.
    %            'CoaddPCFile'- .mat file with PC_all, or PC_all cell,
    %                          PhotCalibTrans array, AstroImage array (1xNcrop),
    %                          or cell of AstroImage (auto-calibrated).
    %                          Default is '' (auto from VisitDir).
    %            'MergedMatDir'- Directory with MergedMat HDF5 files.
    %                          Default is '' (= VisitDir).
    %            'EpochDir'   - Directory with proc epoch FITS files.
    %                          Default is '' (= VisitDir).
    %          --- Data (single-epoch branch) ---
    %            'EpochAI'    - Scalar AstroImage for one crop of one epoch.
    %                          When set, triggers single-epoch branch.
    %                          Default is [].
    %            'CropIdx'    - Crop index into PCarray (and EpochAI if array).
    %                          Default is 1.
    %          --- Propagation ---
    %            'RefEpoch'   - Reference epoch for DeltaZP. Default is 1.
    %            'FluxField'  - Flux field in MS. Default is 'FLUX_APER_3'.
    %          --- Plotting ---
    %            'Ncrop'      - Number of crops. Default is 24.
    %            'CropsToAnalyze' - Crops to process. Default is [] (all).
    %            'Plot'       - Generate plots. Default is true.
    %            'SaveFig'    - Save figures. Default is false.
    %            'OutDir'     - Output directory. Default is pwd.
    %            'Verbose'    - Print progress. Default is true.
    % Output : - Result struct.
    %          Multi-epoch: .MS (with propagated MAG_AB_*), .DeltaZP,
    %                       .PCepochs, .PC_coadd. Saved to OutDir/ResultProp.mat
    %                       (compact: MS + DeltaZP + PC_coadd; PCepochs is
    %                       reconstructible as PC_coadd + DeltaZP).
    %          Single-epoch: .AIepoch (catalog + header enhanced), .PCepoch.
    % Author : D. Kovaleva (Apr 2026)
    % Example: % Multi-epoch — everything in one folder:
    %          R = pipeline.last.quality.testPhotPropagation( ...
    %              'VisitDir', '~/222625v1');
    %
    %          % Multi-epoch — separate inputs:
    %          R = pipeline.last.quality.testPhotPropagation( ...
    %              'CoaddPCFile', '~/results/PC_percrop.mat', ...
    %              'MergedMatDir', '~/222625v1', ...
    %              'EpochDir', '~/222625v1');
    %
    %          % Single-epoch — apply coadd PC to one epoch AstroImage:
    %          AI_coadd = pipeline.last.quality.loadVisitData( ...
    %              'DataDir', '~/222625v1', 'FileType', 'coadd');
    %          AIepoch  = AstroImage('proc_epoch5_crop10.fits');
    %          R = pipeline.last.quality.testPhotPropagation( ...
    %              'CoaddPCFile', AI_coadd{1}, 'EpochAI', AIepoch, ...
    %              'CropIdx', 10);
    %          CatEnhanced = R.AIepoch.CatData;  % has MAG_AB_*, AB_ZP

    arguments
        Args.VisitDir    = ''   % visit folder with everything
        Args.CoaddPCFile = ''   % .mat path, PC_all cell, or PhotCalibTrans array
        Args.MergedMatDir = ''  % directory with MergedMat HDF5 files
        Args.EpochDir    = ''   % directory with proc FITS files
        Args.EpochAI     = []   % single AstroImage (one crop, one epoch) — single-epoch branch
        Args.CropIdx     = 1    % crop index when selecting from PCarray / EpochAI array
        Args.RefEpoch    = 1
        Args.FluxField   = 'FLUX_APER_3'
        Args.Ncrop       = 24
        Args.CropsToAnalyze = []
        Args.Plot logical = true
        Args.SaveFig logical = false
        Args.OutDir      = pwd
        Args.Verbose logical = true
    end

    if isempty(Args.CropsToAnalyze)
        Args.CropsToAnalyze = 1:Args.Ncrop;
    end

    % === Single-epoch branch: apply coadd PC directly to one epoch catalog ===
    % No MergedMat, no DeltaZP — just adjust ExpTime/NCoadd and call addMag.
    if ~isempty(Args.EpochAI)
        Result = runSingleEpoch(Args);
        return;
    end

    % Auto-detect from VisitDir
    if ~isempty(Args.VisitDir)
        if isempty(Args.MergedMatDir)
            Args.MergedMatDir = Args.VisitDir;
        end
        if isempty(Args.EpochDir)
            Args.EpochDir = Args.VisitDir;
        end
    end

    % === Step 1: Load coadd PC ===
    PCarray = [];
    if isempty(Args.CoaddPCFile) && ~isempty(Args.VisitDir)
        % Auto-detect: calibrate coadd from VisitDir
        if Args.Verbose; fprintf('Calibrating coadd from %s\n', Args.VisitDir); end
        AI_coadd = pipeline.last.quality.loadVisitData( ...
            'DataDir', Args.VisitDir, 'Visits', 1, ...
            'FileType', 'coadd', 'Verbose', Args.Verbose);
        if ~isempty(AI_coadd) && ~isempty(AI_coadd{1})
            [~, PCarray] = imProc.calib.fitPhotCalibTrans(AI_coadd{1}, ...
                'Verbose', Args.Verbose);
        end
    elseif ischar(Args.CoaddPCFile) || isstring(Args.CoaddPCFile)
        if Args.Verbose; fprintf('Loading coadd PC from %s\n', Args.CoaddPCFile); end
        S = load(char(Args.CoaddPCFile), 'PC_all');
        if isfield(S, 'PC_all')
            PC_all = S.PC_all;
            if iscell(PC_all)
                FirstValid = find(~cellfun(@isempty, PC_all), 1);
                PCarray = PC_all{FirstValid};
            else
                PCarray = PC_all;
            end
        end
    elseif iscell(Args.CoaddPCFile)
        % Cell of AstroImage (from loadVisitData) or cell of PC arrays
        FirstValid = find(~cellfun(@isempty, Args.CoaddPCFile), 1);
        Item = Args.CoaddPCFile{FirstValid};
        if isa(Item, 'AstroImage')
            if Args.Verbose; fprintf('Calibrating coadd AstroImage\n'); end
            [~, PCarray] = imProc.calib.fitPhotCalibTrans(Item, ...
                'Verbose', Args.Verbose);
        else
            PCarray = Item;
        end
    elseif isa(Args.CoaddPCFile, 'PhotCalibTrans')
        PCarray = Args.CoaddPCFile;
    elseif isa(Args.CoaddPCFile, 'AstroImage')
        if Args.Verbose; fprintf('Calibrating coadd AstroImage\n'); end
        [~, PCarray] = imProc.calib.fitPhotCalibTrans(Args.CoaddPCFile, ...
            'Verbose', Args.Verbose);
    end

    if isempty(PCarray)
        error('testPhotPropagation:NoPC', ...
            'Cannot obtain coadd PC. Provide VisitDir or CoaddPCFile.');
    end

    % === Step 2: Load MergedMat ===
    if isempty(Args.MergedMatDir)
        error('testPhotPropagation:NoMergedMat', ...
            'MergedMatDir is required. Provide VisitDir or MergedMatDir.');
    end
    if Args.Verbose; fprintf('Loading MergedMat from %s\n', Args.MergedMatDir); end
    MSstruct = pipeline.last.quality.loadMergedMat( ...
        'MergedMatDir', Args.MergedMatDir, ...
        'CropsToAnalyze', Args.CropsToAnalyze, ...
        'Ncrop', Args.Ncrop, ...
        'Verbose', Args.Verbose);
    MSall = MSstruct.percrop;

    % === Step 3: Load proc epoch AstroImages ===
    AI = [];
    if ~isempty(Args.EpochDir)
        if Args.Verbose; fprintf('Loading proc epochs from %s\n', Args.EpochDir); end
        AI = pipeline.last.quality.loadVisitData( ...
            'DataDir', Args.EpochDir, 'FileType', 'proc', ...
            'Verbose', Args.Verbose);
    end

    % === Step 4: Propagate per crop ===
    DeltaZPall = cell(1, Args.Ncrop);
    PCepochsAll = cell(1, Args.Ncrop);

    for Ic = Args.CropsToAnalyze
        if Ic > numel(PCarray) || ~PCarray(Ic).Success
            if Args.Verbose
                fprintf('  Crop %02d: coadd PC not available, skipping\n', Ic);
            end
            continue;
        end
        if isempty(MSall{Ic})
            if Args.Verbose
                fprintf('  Crop %02d: no MergedMat data, skipping\n', Ic);
            end
            continue;
        end

        MSobj = MSall{Ic};
        Nepochs = MSobj.Nepoch;

        % Collect per-epoch AstroImages for this crop
        EpochAIcrop = AstroImage.empty(0, Nepochs);
        if ~isempty(AI)
            for Iv = 1:min(Nepochs, numel(AI))
                if isempty(AI{Iv}) || Ic > numel(AI{Iv})
                    EpochAIcrop(Iv) = AstroImage;
                else
                    EpochAIcrop(Iv) = AI{Iv}(Ic);
                end
            end
        end

        if Args.Verbose
            fprintf('  Crop %02d: propagating to %d epochs\n', Ic, Nepochs);
        end

        try
            [EpochAIcrop, PCepochs, DeltaZP] = ...
                PhotCalibTrans.propagatePhotCalibToEpoch( ...
                    PCarray(Ic), MSobj, EpochAIcrop, ...
                    'FluxField', Args.FluxField, ...
                    'RefEpoch', Args.RefEpoch, ...
                    'Verbose', false);

            DeltaZPall{Ic} = DeltaZP;
            PCepochsAll{Ic} = PCepochs;

            % Compute propagated MAG_AB directly from MS fluxes (no catalog row-matching)
            % Uses MS's own cross-matched fluxes/positions and per-epoch PC objects.
            Nsrc = MSobj.Nsrc;
            FluxToMag = {'FLUX_APER_3', 'MAG_AB_APER_3'; ...
                         'FLUX_PSF',    'MAG_AB_PSF'};
            % Pick position fields available in MS
            if isfield(MSobj.Data, 'X1')
                Xfield = 'X1'; Yfield = 'Y1';
            elseif isfield(MSobj.Data, 'X')
                Xfield = 'X';  Yfield = 'Y';
            else
                Xfield = ''; Yfield = '';
            end
            for Imf = 1:size(FluxToMag, 1)
                FF = FluxToMag{Imf, 1};
                MF = FluxToMag{Imf, 2};
                if ~isfield(MSobj.Data, FF); continue; end
                FluxMat = MSobj.Data.(FF);
                MagAB = nan(Nepochs, Nsrc);
                for Iv = 1:min(Nepochs, numel(PCepochs))
                    if isempty(PCepochs(Iv)) || ~PCepochs(Iv).Success; continue; end
                    Flux = FluxMat(Iv, :).';
                    Valid = isfinite(Flux) & Flux > 0;
                    if ~any(Valid); continue; end
                    if ~isempty(Xfield)
                        X = MSobj.Data.(Xfield)(Iv, :).';
                        Y = MSobj.Data.(Yfield)(Iv, :).';
                        try
                            Mag = PCepochs(Iv).evaluateMag(Flux(Valid), ...
                                'X', X(Valid), 'Y', Y(Valid));
                        catch
                            Mag = PCepochs(Iv).evaluateMag(Flux(Valid));
                        end
                    else
                        Mag = PCepochs(Iv).evaluateMag(Flux(Valid));
                    end
                    MagAB(Iv, Valid) = Mag(:).';
                end
                MSobj.Data.(MF) = MagAB;
            end

            MSall{Ic} = MSobj;
            if Args.Verbose
                fprintf('    DeltaZP range: %.4f to %.4f mag\n', ...
                    min(DeltaZP), max(DeltaZP));
            end
        catch ME
            if Args.Verbose
                fprintf('  Crop %02d: propagation failed (%s)\n', Ic, ME.message);
            end
        end
    end

    % === Assemble Result ===
    Result.MS.percrop = MSall;
    Result.DeltaZP    = DeltaZPall;
    Result.PCepochs   = PCepochsAll;
    Result.PC_coadd   = PCarray;

    % === Save Result (compact form: PC_coadd + DeltaZP reconstructs PCepochs) ===
    if ~exist(Args.OutDir, 'dir'); mkdir(Args.OutDir); end
    ResultFile = fullfile(Args.OutDir, 'ResultProp.mat');
    try
        ResultToSave.MS       = Result.MS;
        ResultToSave.DeltaZP  = Result.DeltaZP;
        ResultToSave.PC_coadd = Result.PC_coadd;
        save(ResultFile, 'ResultToSave', '-v7.3');
        if Args.Verbose
            fprintf('Saved %s\n', ResultFile);
        end
    catch ME
        warning('testPhotPropagation:SaveFailed', ...
            'Failed to save %s: %s', ResultFile, ME.message);
    end

    % === Plots ===
    if ~Args.Plot
        return;
    end

    % Scatter: propagated MAG_AB vs relative MAG_APER_3
    pipeline.last.quality.plotPhotScatter(Result.MS, ...
        'Modes', {'percrop'}, ...
        'MagFields', {'MAG_AB_APER_3'}, ...
        'TwoPanels', true, ...
        'CropsToAnalyze', Args.CropsToAnalyze);

    % Std difference: propagated AB vs relative
    pipeline.last.quality.plotPhotStdDiff(Result.MS, ...
        'Modes', {'percrop'}, ...
        'CompareFields', {'MAG_AB_APER_3', 'MAG_APER_3'}, ...
        'CropsToAnalyze', Args.CropsToAnalyze);

    % Save figures
    if Args.SaveFig
        FigDir = fullfile(Args.OutDir, 'figures');
        if ~exist(FigDir, 'dir'); mkdir(FigDir); end
        AllFigs = findall(0, 'Type', 'figure');
        for If = 1:numel(AllFigs)
            Fig = AllFigs(If);
            if ~isvalid(Fig); continue; end
            FigName = matlab.lang.makeValidName(Fig.Name);
            if isempty(FigName); FigName = sprintf('fig_%d', Fig.Number); end
            savefig(Fig, fullfile(FigDir, [FigName '.fig']));
            saveas(Fig, fullfile(FigDir, [FigName '.jpg']));
        end
        if Args.Verbose
            fprintf('Saved %d figures to %s\n', numel(AllFigs), FigDir);
        end
    end
end

% =========================================================================
function Result = runSingleEpoch(Args)
    % Single-epoch branch: apply coadd PC directly to one epoch's catalog.
    % Inputs from Args: CoaddPCFile (or VisitDir), EpochAI, CropIdx.

    % If CoaddPCFile is an AstroImage array, narrow to the requested crop
    % BEFORE calibration so fitPhotCalibTrans runs only on that crop.
    if isa(Args.CoaddPCFile, 'AstroImage') && numel(Args.CoaddPCFile) > 1
        if Args.CropIdx <= numel(Args.CoaddPCFile)
            Args.CoaddPCFile = Args.CoaddPCFile(Args.CropIdx);
        else
            Args.CoaddPCFile = Args.CoaddPCFile(1);
        end
    elseif iscell(Args.CoaddPCFile) && ~isempty(Args.CoaddPCFile)
        FirstValid = find(~cellfun(@isempty, Args.CoaddPCFile), 1);
        Item = Args.CoaddPCFile{FirstValid};
        if isa(Item, 'AstroImage') && numel(Item) > 1
            if Args.CropIdx <= numel(Item)
                Args.CoaddPCFile = Item(Args.CropIdx);
            else
                Args.CoaddPCFile = Item(1);
            end
        end
    end

    % Resolve coadd PC array (now scalar if we narrowed above)
    PCarray = resolveCoaddPC(Args);
    if isempty(PCarray)
        error('testPhotPropagation:NoPC', ...
            'Provide CoaddPCFile (PC/AstroImage) or VisitDir.');
    end

    % Select single PC for the requested crop (index 1 after narrowing)
    if numel(PCarray) == 1
        PCcrop = PCarray(1);
    elseif numel(PCarray) >= Args.CropIdx
        PCcrop = PCarray(Args.CropIdx);
    else
        PCcrop = PCarray(1);
    end
    if ~PCcrop.Success
        error('testPhotPropagation:BadPC', ...
            'Coadd PC for crop %d is not Success.', Args.CropIdx);
    end

    % Select single AstroImage
    AIepoch = Args.EpochAI;
    if numel(AIepoch) > 1
        AIepoch = AIepoch(Args.CropIdx);
    end

    % Copy PC and adapt for single epoch: NCoadd=1, ExpTime from header
    PCe = PCcrop.copy();
    PCe.NCoadd = 1;
    if isa(AIepoch, 'AstroImage') && ~isempty(AIepoch.HeaderData)
        ExpTimeVal = AIepoch.HeaderData.getVal('EXPTIME');
        if ~isempty(ExpTimeVal) && isnumeric(ExpTimeVal)
            PCe.ExpTime = ExpTimeVal;
        end
    end
    PCe.AperCorr         = PCcrop.AperCorr;
    PCe.AperCorrColNames = PCcrop.AperCorrColNames;
    PCe.AperCorrNStars   = PCcrop.AperCorrNStars;

    % Apply addMag (with AddZP to avoid second evaluateZP pass)
    CatObj = AIepoch.CatData;
    HasCat = ~isempty(CatObj) && ~isempty(CatObj.Table) && height(CatObj.Table) > 0;
    if HasCat
        CatObj = PCe.addMag(CatObj, 'MagSystem', 'AB', 'AddZP', true);
        AIepoch.CatData = CatObj;
        if ~isempty(AIepoch.HeaderData)
            PCe.photCalibTransToHeader(AIepoch.HeaderData);
        end
    else
        warning('testPhotPropagation:EmptyCat', ...
            'Epoch catalog is empty — nothing to calibrate.');
    end

    Result.AIepoch = AIepoch;
    Result.PCepoch = PCe;

    if Args.Verbose
        fprintf('Single-epoch: crop %d, ExpTime=%.2f, Nsrc=%d\n', ...
            Args.CropIdx, PCe.ExpTime, height(AIepoch.CatData.Table));
    end
end

% =========================================================================
function PCarray = resolveCoaddPC(Args)
    % Extract PhotCalibTrans array from CoaddPCFile/VisitDir (shared logic).
    PCarray = [];
    if isempty(Args.CoaddPCFile) && ~isempty(Args.VisitDir)
        AI_coadd = pipeline.last.quality.loadVisitData( ...
            'DataDir', Args.VisitDir, 'Visits', 1, ...
            'FileType', 'coadd', 'Verbose', Args.Verbose);
        if ~isempty(AI_coadd) && ~isempty(AI_coadd{1})
            [~, PCarray] = imProc.calib.fitPhotCalibTrans(AI_coadd{1}, ...
                'Verbose', Args.Verbose);
        end
    elseif ischar(Args.CoaddPCFile) || isstring(Args.CoaddPCFile)
        S = load(char(Args.CoaddPCFile), 'PC_all');
        if isfield(S, 'PC_all')
            PC_all = S.PC_all;
            if iscell(PC_all)
                FirstValid = find(~cellfun(@isempty, PC_all), 1);
                PCarray = PC_all{FirstValid};
            else
                PCarray = PC_all;
            end
        end
    elseif iscell(Args.CoaddPCFile)
        FirstValid = find(~cellfun(@isempty, Args.CoaddPCFile), 1);
        Item = Args.CoaddPCFile{FirstValid};
        if isa(Item, 'AstroImage')
            [~, PCarray] = imProc.calib.fitPhotCalibTrans(Item, ...
                'Verbose', Args.Verbose);
        else
            PCarray = Item;
        end
    elseif isa(Args.CoaddPCFile, 'PhotCalibTrans')
        PCarray = Args.CoaddPCFile;
    elseif isa(Args.CoaddPCFile, 'AstroImage')
        [~, PCarray] = imProc.calib.fitPhotCalibTrans(Args.CoaddPCFile, ...
            'Verbose', Args.Verbose);
    end
end
