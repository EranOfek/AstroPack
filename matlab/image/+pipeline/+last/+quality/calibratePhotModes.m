function Result = calibratePhotModes(AI, Args)
    % Calibrate photometric modes and compute FitRMS / center ZP
    % Description: For each requested PhotSys mode, calibrates all visits
    %              using fitPhotCalibTrans. Handles per-epoch modes (percrop,
    %              shapeimage, perimage, perimage_raw) and visit-level modes (perset,
    %              perset_raw). Results are cached in OutDir as PC_<mode>.mat.
    %
    % Input  : - AI cell(Nvisits,1) of AstroImage arrays (from loadVisitData).
    %          * ...,key,val,...
    %            'Modes'    - Cell array of PhotSys modes. Default is
    %                         {'percrop','shapeimage','perimage','perimage_raw'}.
    %            'Visits'   - Visit index vector (for labeling). Default is 1:20.
    %            'RefCrop'  - Reference crop index (0=weighted mean). Default is 10.
    %            'Ncrop'    - Number of crops per visit. Default is 24.
    %            'OutDir'   - Directory for cached .mat files. Default is '' (no caching).
    %            'ForceRecalc' - Recompute even if cached. Default is false.
    %            'CalibArgs'   - Extra key-value args for fitPhotCalibTrans. Default is {}.
    %            'VisitRefZP'  - ZP normalization for perset/perset_raw modes:
    %                            'crop_median'|'crop_mean'|'global_median'|'global_mean'|'epoch'.
    %                            Default is 'epoch'.
    %            'VisitRefZPEpoch' - Epoch index when VisitRefZP='epoch'. Default is 1.
    %            'MagFields' - AB magnitude columns (for visit-level ZP correction).
    %                          Default is {'MAG_AB_PSF','MAG_AB_APER_3'}.
    %            'Verbose'  - Print progress. Default is true.
    % Output : - Result struct with fields:
    %            .PC       - struct with PC_all{Nvisits}(1xNcrop) per mode
    %            .Cats     - struct with Cats_all{Nvisits}(1xNcrop) per mode
    %            .FitRMS   - [Nvisits x Ncrop] matrix (from percrop fit)
    %            .ZPcenter - struct per mode with [Nvisits x Ncrop] center ZP.
    %                        Per-epoch modes share percrop values;
    %                        perset modes store the effective target ZP.
    % Author : D. Kovaleva (Mar 2026)
    % Example: R = pipeline.last.quality.calibratePhotModes(AI);
    %          R = pipeline.last.quality.calibratePhotModes(AI, ...
    %              'Modes', {'percrop','perset'}, 'VisitRefZP', 'crop_median');

    arguments
        AI cell
        Args.Modes          = {'percrop', 'shapeimage', 'perimage', 'perimage_raw'}
        Args.Visits         = 1:20
        Args.RefCrop        = 10
        Args.Ncrop          = 24
        Args.OutDir         = ''
        Args.ForceRecalc logical = false
        Args.CalibArgs cell = {}
        Args.VisitRefZP     = 'epoch'
        Args.VisitRefZPEpoch = 1
        Args.MagFields      = {'MAG_AB_PSF', 'MAG_AB_APER_3'}
        Args.Verbose logical = true
    end

    Nvisits = numel(Args.Visits);

    % Separate derived modes from percrop
    DerivedPerEpochModes = {'shapeimage', 'perimage', 'perimage_raw'};
    VisitModes = {'perset', 'perset_raw', 'shapeset'};
    AllDerived = [DerivedPerEpochModes, VisitModes];

    % Ensure percrop is always calibrated
    NeedPercrop = any(ismember(Args.Modes, AllDerived)) || ismember('percrop', Args.Modes);

    Result.PC   = struct();
    Result.Cats = struct();

    % ================================================================
    % Step 1: Calibrate percrop (the only actual calibration)
    % ================================================================
    if NeedPercrop
        OutFile = '';
        if ~isempty(Args.OutDir)
            OutFile = fullfile(Args.OutDir, 'PC_percrop.mat');
        end

        UseCache = false;
        if ~isempty(OutFile) && exist(OutFile, 'file') && ~Args.ForceRecalc
            try
                S = load(OutFile, 'PC_all', 'Cats_all');
                if isfield(S, 'PC_all') && isfield(S, 'Cats_all') && ...
                   numel(S.PC_all) == Nvisits
                    Result.PC.percrop   = S.PC_all;
                    Result.Cats.percrop = S.Cats_all;
                    UseCache = true;
                    if Args.Verbose
                        fprintf('Loaded cached %s (%d visits)\n', OutFile, numel(S.PC_all));
                    end
                else
                    if Args.Verbose
                        fprintf('Cache %s has %d visits, need %d — recalibrating\n', ...
                            OutFile, numel(S.PC_all), Nvisits);
                    end
                end
            catch ME
                if Args.Verbose
                    fprintf('Cache %s corrupt (%s) — recalibrating\n', OutFile, ME.message);
                end
            end
        end

        if ~UseCache
            if Args.Verbose
                fprintf('\n=== Calibrating: PhotSys = percrop ===\n');
            end
            PC_all   = cell(Nvisits, 1);
            Cats_all = cell(Nvisits, 1);

            for Iv = 1:Nvisits
                if isempty(AI{Iv})
                    continue;
                end
                t0 = tic;

                tcopy = tic;
                AIcopy = AI{Iv}.copy();
                if Args.Verbose
                    fprintf('    copy: %.1f s, ', toc(tcopy));
                end

                [Res, PC_all{Iv}] = imProc.calib.fitPhotCalibTrans(AIcopy, ...
                    'PhotSys', 'percrop', 'RefCrop', Args.RefCrop, ...
                    'Verbose', false, Args.CalibArgs{:});

                Ncrop_v = numel(Res);
                Cats_all{Iv} = AstroCatalog.empty(0, Ncrop_v);
                for Ic = 1:Ncrop_v
                    Cats_all{Iv}(Ic) = Res(Ic).CatData;
                end

                if Args.Verbose
                    Nsuccess = sum([PC_all{Iv}.Success]);
                    fprintf('  Epoch %03d: %d/%d success, %.1f s\n', ...
                        Args.Visits(Iv), Nsuccess, Ncrop_v, toc(t0));
                end
            end

            if ~isempty(OutFile)
                try
                    save(OutFile, 'PC_all', 'Cats_all', '-v7.3');
                    if Args.Verbose
                        fprintf('Saved %s\n', OutFile);
                    end
                catch ME
                    warning('calibratePhotModes:SaveFailed', ...
                        'Failed to save %s: %s', OutFile, ME.message);
                end
            end
            Result.PC.percrop   = PC_all;
            Result.Cats.percrop = Cats_all;
        end
    end

    % ================================================================
    % Step 2: Derived per-epoch modes (reuse percrop PC, recompute mags)
    % ================================================================
    % addMag settings per mode (matching fitPhotCalibTrans logic):
    %   shapeimage:   RefTransParams=RefParamVec, UseRefNorm=false,  NormTran2D=false
    %   perimage:     RefTransParams=RefParamVec, UseRefNorm=true,   NormTran2D=true
    %   perimage_raw: RefTransParams=RefParamVec, UseRefNorm=true,   NormTran2D=false
    DerivedSettings = struct( ...
        'shapeimage',   struct('UseRefNorm', false, 'NormTran2D', false), ...
        'perimage',     struct('UseRefNorm', true,  'NormTran2D', true), ...
        'perimage_raw', struct('UseRefNorm', true,  'NormTran2D', false));

    for Im = 1:numel(Args.Modes)
        Mode = Args.Modes{Im};
        if ~isfield(DerivedSettings, Mode); continue; end

        OutFile = '';
        if ~isempty(Args.OutDir)
            OutFile = fullfile(Args.OutDir, sprintf('PC_%s.mat', Mode));
        end

        UseCache = false;
        if ~isempty(OutFile) && exist(OutFile, 'file') && ~Args.ForceRecalc
            try
                S = load(OutFile, 'Cats_all');
                if isfield(S, 'Cats_all') && numel(S.Cats_all) == Nvisits
                    Result.Cats.(Mode) = S.Cats_all;
                    UseCache = true;
                    if Args.Verbose
                        fprintf('Loaded cached %s (%d visits)\n', OutFile, numel(S.Cats_all));
                    end
                end
            catch ME
                if Args.Verbose
                    fprintf('Cache %s corrupt (%s) — recomputing\n', OutFile, ME.message);
                end
            end
        end

        if ~UseCache
            if Args.Verbose
                fprintf('\n=== Derived mode: %s (reusing percrop PC) ===\n', Mode);
            end

            Sett = DerivedSettings.(Mode);
            PC_all   = Result.PC.percrop;
            Cats_all = cell(Nvisits, 1);

            for Iv = 1:Nvisits
                if isempty(AI{Iv}); continue; end
                if isempty(PC_all{Iv}); continue; end

                % Build RefParamVec per epoch from this epoch's RefCrop
                RefParamVec = buildRefParamVec(PC_all{Iv}, Args.RefCrop);
                if isempty(RefParamVec); continue; end

                AIcopy = AI{Iv}.copy();
                Ncrop_v = numel(PC_all{Iv});
                Cats_all{Iv} = AstroCatalog.empty(0, Ncrop_v);

                for Ic = 1:Ncrop_v
                    if ~PC_all{Iv}(Ic).Success
                        Cats_all{Iv}(Ic) = AstroCatalog;
                        continue;
                    end
                    Cats_all{Iv}(Ic) = PC_all{Iv}(Ic).addMag( ...
                        AIcopy(Ic).CatData, ...
                        'MagSystem', 'AB', ...
                        'RefTransParams', RefParamVec, ...
                        'UseRefNorm', Sett.UseRefNorm, ...
                        'NormTran2D', Sett.NormTran2D);
                end

                if Args.Verbose
                    fprintf('  Epoch %03d: magnitudes recomputed (%s)\n', Args.Visits(Iv), Mode);
                end
            end

            if ~isempty(OutFile)
                try
                    save(OutFile, 'Cats_all', '-v7.3');
                    if Args.Verbose
                        fprintf('Saved %s\n', OutFile);
                    end
                catch ME
                    warning('calibratePhotModes:SaveFailed', ...
                        'Failed to save %s: %s', OutFile, ME.message);
                end
            end
            Result.Cats.(Mode) = Cats_all;
        end

        % PC objects are shared with percrop
        Result.PC.(Mode) = Result.PC.percrop;
    end

    % ================================================================
    % Fit RMS and center ZP (from percrop)
    % ================================================================
    NvisitsPC = numel(Result.PC.percrop);
    Result.FitRMS = nan(NvisitsPC, Args.Ncrop);
    ZPcenterPercrop = nan(NvisitsPC, Args.Ncrop);
    for Iv = 1:NvisitsPC
        if isempty(Result.PC.percrop{Iv}); continue; end
        for Ic = 1:numel(Result.PC.percrop{Iv})
            if Result.PC.percrop{Iv}(Ic).Success
                Result.FitRMS(Iv, Ic) = Result.PC.percrop{Iv}(Ic).TransModel.RMS;
                % ZP at crop center with center-normalized Tran2D (=0 at center):
                % ZP_base includes Norm but not Tran2D; subtract Tran2D(center)
                % to get the ZP where Norm absorbs the Tran2D center offset.
                PC_ic = Result.PC.percrop{Iv}(Ic);
                ZPbase = PC_ic.evaluateZP();
                if ~isempty(PC_ic.TransModel.Tran2DObj) && PC_ic.TransModel.UseTran2D
                    Xc = PC_ic.TransModel.Tran2DObj.ParNX(1);
                    Yc = PC_ic.TransModel.Tran2DObj.ParNY(1);
                    [CenterCorr, ~] = PC_ic.TransModel.Tran2DObj.forward(Xc, Yc, false);
                    ZPbase = ZPbase - CenterCorr;
                end
                ZPcenterPercrop(Iv, Ic) = ZPbase;
            end
        end
    end

    % Store per-mode ZPcenter: percrop + derived per-epoch modes share percrop values
    Result.ZPcenter = struct();
    PerEpochNames = setdiff(Args.Modes, VisitModes, 'stable');
    for Im = 1:numel(PerEpochNames)
        Result.ZPcenter.(PerEpochNames{Im}) = ZPcenterPercrop;
    end

    if Args.Verbose
        fprintf('\n=== Fit RMS Summary ===\n');
        vals = Result.FitRMS(isfinite(Result.FitRMS));
        fprintf('Median: %.4f   Mean: %.4f   Max: %.4f\n', median(vals), mean(vals), max(vals));
    end

    % ================================================================
    % Visit-level modes (perset, perset_raw)
    % ================================================================
    for Im = 1:numel(Args.Modes)
        Mode = Args.Modes{Im};
        if ~ismember(Mode, VisitModes); continue; end

        OutFile = '';
        if ~isempty(Args.OutDir)
            OutFile = fullfile(Args.OutDir, sprintf('PC_%s.mat', Mode));
        end

        UseCache = false;
        if ~isempty(OutFile) && exist(OutFile, 'file') && ~Args.ForceRecalc
            try
                S = load(OutFile, 'PC_all', 'Cats_all');
                if isfield(S, 'PC_all') && isfield(S, 'Cats_all') && ...
                   numel(S.PC_all) == Nvisits
                    Result.PC.(Mode)   = S.PC_all;
                    Result.Cats.(Mode) = S.Cats_all;
                    UseCache = true;
                    if Args.Verbose
                        fprintf('Loaded cached %s (%d visits)\n', OutFile, numel(S.PC_all));
                    end
                else
                    if Args.Verbose
                        fprintf('Cache %s has %d visits, need %d — recalibrating\n', ...
                            OutFile, numel(S.PC_all), Nvisits);
                    end
                end
            catch ME
                if Args.Verbose
                    fprintf('Cache %s corrupt (%s) — recalibrating\n', OutFile, ME.message);
                end
            end
        end

        if ~UseCache
            if Args.Verbose
                fprintf('\n=== Visit-level mode: %s ===\n', Mode);
            end

            % Collect transmission parameters across epochs (weighted mean)
            RefCropIdx = Args.RefCrop;
            AllParams  = [];
            AllWeights = [];
            for Iv = 1:Nvisits
                if isempty(Result.PC.percrop{Iv}); continue; end
                if RefCropIdx == 0
                    CropRange = 1:numel(Result.PC.percrop{Iv});
                else
                    CropRange = RefCropIdx;
                end
                for Ic = CropRange
                    if Ic > numel(Result.PC.percrop{Iv}); continue; end
                    PC_rc = Result.PC.percrop{Iv}(Ic);
                    if PC_rc.Success && PC_rc.TransModel.RMS > 0
                        P = PC_rc.TransModel.getAllFunPar();
                        AllParams  = [AllParams; P.Val(:)'];
                        AllWeights = [AllWeights; 1 ./ PC_rc.TransModel.RMS.^2];
                    end
                end
            end

            if isempty(AllParams)
                warning('calibratePhotModes:NoVisitRef', ...
                    'No successful crops across epochs. Skipping %s.', Mode);
                continue;
            end

            W = AllWeights / sum(AllWeights);
            VisitRefParams = (W' * AllParams)';

            if Args.Verbose
                if RefCropIdx == 0
                    fprintf('  Visit-averaged over all crops from %d fits\n', size(AllParams, 1));
                else
                    fprintf('  Visit-averaged RefCrop=%d from %d epochs\n', ...
                        RefCropIdx, size(AllParams, 1));
                end
            end

            % Identify observational parameter indices — these vary per epoch
            % and should not be averaged; restored from each epoch's percrop fit
            FirstPC = Result.PC.percrop{find(~cellfun(@isempty, Result.PC.percrop), 1)}(1);
            AllFunPar = FirstPC.TransModel.getAllFunPar();
            ObsParNames = {'ZenithAngle_deg', 'Temperature', 'Pressure', 'AirMass'};
            ObsParIdx = find(ismember(AllFunPar.Name, ObsParNames));

            IsShapeset = strcmp(Mode, 'shapeset');
            DoNormTran2D = strcmp(Mode, 'perset');

            % Find Norm index in parameter vector (once)
            AllFunPar = Result.PC.percrop{find(~cellfun(@isempty, Result.PC.percrop), 1)}(1).TransModel.getAllFunPar();
            NormIdx = find(strcmp(AllFunPar.Name, 'Norm'));

            % Compute target ZP per crop (not used for shapeset)
            ZPc = ZPcenterPercrop;
            TargetZP = nan(1, Args.Ncrop);
            if ~IsShapeset
                switch Args.VisitRefZP
                    case 'crop_median'
                        TargetZP = nanmedian(ZPc, 1);
                    case 'crop_mean'
                        TargetZP = nanmean(ZPc, 1);
                    case 'global_median'
                        TargetZP = repmat(nanmedian(ZPc(:)), 1, Args.Ncrop);
                    case 'global_mean'
                        TargetZP = repmat(nanmean(ZPc(:)), 1, Args.Ncrop);
                    case 'epoch'
                        EpIdx = Args.VisitRefZPEpoch;
                        TargetZP = ZPc(EpIdx, :);
                    otherwise
                        error('calibratePhotModes:BadVisitRefZP', ...
                            'Unknown VisitRefZP: %s', Args.VisitRefZP);
                end

                if Args.Verbose
                    fprintf('  VisitRefZP=%s, target ZP range: %.3f..%.3f\n', ...
                        Args.VisitRefZP, nanmin(TargetZP), nanmax(TargetZP));
                end
            end

            % Reuse percrop PC objects; recompute catalogs with visit reference
            PC_all   = Result.PC.percrop;
            Cats_all = cell(Nvisits, 1);

            for Iv = 1:Nvisits
                if isempty(AI{Iv}); continue; end
                if isempty(PC_all{Iv}); continue; end

                AIcopy = AI{Iv}.copy();
                Ncrop_v = numel(PC_all{Iv});
                Cats_all{Iv} = AstroCatalog.empty(0, Ncrop_v);

                for Ic = 1:Ncrop_v
                    if ~PC_all{Iv}(Ic).Success
                        Cats_all{Iv}(Ic) = AstroCatalog;
                        continue;
                    end

                    % Restore observational params from this epoch's percrop fit
                    EpochPar = PC_all{Iv}(Ic).TransModel.getAllFunPar();
                    CropParams = VisitRefParams;
                    CropParams(ObsParIdx) = EpochPar.Val(ObsParIdx);

                    if IsShapeset
                        % shapeset: visit-averaged shape, per-crop Norm and Tran2D
                        Cats_all{Iv}(Ic) = PC_all{Iv}(Ic).addMag( ...
                            AIcopy(Ic).CatData, ...
                            'MagSystem', 'AB', ...
                            'RefTransParams', CropParams, ...
                            'UseRefNorm', false, ...
                            'NormTran2D', false);
                    else
                        % perset/perset_raw: adjusted Norm to target ZP
                        ZPvisitBase = PC_all{Iv}(Ic).evaluateZP( ...
                            'RefTransParams', CropParams, ...
                            'UseRefNorm', true, ...
                            'NormTran2D', DoNormTran2D);

                        if isfinite(ZPvisitBase) && Ic <= numel(TargetZP) && isfinite(TargetZP(Ic))
                            DeltaZP = TargetZP(Ic) - ZPvisitBase;
                            CropParams(NormIdx) = CropParams(NormIdx) * 10^(DeltaZP / 2.5);
                        end

                        Cats_all{Iv}(Ic) = PC_all{Iv}(Ic).addMag( ...
                            AIcopy(Ic).CatData, ...
                            'MagSystem', 'AB', ...
                            'RefTransParams', CropParams, ...
                            'UseRefNorm', true, ...
                            'NormTran2D', DoNormTran2D);
                    end
                end

                if Args.Verbose
                    fprintf('  Epoch %03d: magnitudes recomputed\n', Args.Visits(Iv));
                end
            end

            if ~isempty(OutFile)
                try
                    save(OutFile, 'PC_all', 'Cats_all', '-v7.3');
                    if Args.Verbose
                        fprintf('Saved %s\n', OutFile);
                    end
                catch ME
                    warning('calibratePhotModes:SaveFailed', ...
                        'Failed to save %s: %s', OutFile, ME.message);
                end
            end
            Result.PC.(Mode)   = PC_all;
            Result.Cats.(Mode) = Cats_all;

            if IsShapeset
                % shapeset uses percrop's own Norm — same ZPcenter
                Result.ZPcenter.(Mode) = ZPcenterPercrop;
            else
                % perset: TargetZP replicated across epochs
                Result.ZPcenter.(Mode) = repmat(TargetZP, NvisitsPC, 1);
            end

            % Store visit-level params for ZP mosaic rendering
            Result.PersetInfo.(Mode).VisitRefParams = VisitRefParams;
            Result.PersetInfo.(Mode).NormIdx = NormIdx;
            Result.PersetInfo.(Mode).TargetZP = TargetZP;
            Result.PersetInfo.(Mode).ZPcenterPercrop = ZPcenterPercrop;
            Result.PersetInfo.(Mode).DoNormTran2D = DoNormTran2D;
            Result.PersetInfo.(Mode).IsShapeset = IsShapeset;
        end
    end
end

% =========================================================================
function RefParamVec = buildRefParamVec(PCarray, RefCrop)
    % Build reference parameter vector from a PC array (single epoch)
    % or weighted mean across all successful crops (RefCrop=0).
    % Input  : - PCarray: 1xNcrop PhotCalibTrans array (single epoch)
    %          - RefCrop: reference crop index, or 0 for weighted mean
    % Output : - RefParamVec [Nparams x 1], or [] if no valid data
    RefParamVec = [];
    Nobj = numel(PCarray);

    if RefCrop == 0
        AllParams  = [];
        AllWeights = [];
        for Ic = 1:Nobj
            if PCarray(Ic).Success && PCarray(Ic).TransModel.RMS > 0
                P = PCarray(Ic).TransModel.getAllFunPar();
                AllParams  = [AllParams; P.Val(:)'];
                AllWeights = [AllWeights; 1 ./ PCarray(Ic).TransModel.RMS.^2];
            end
        end
        if ~isempty(AllParams)
            W = AllWeights / sum(AllWeights);
            RefParamVec = (W' * AllParams)';
        end
    elseif RefCrop >= 1 && RefCrop <= Nobj && PCarray(RefCrop).Success
        P = PCarray(RefCrop).TransModel.getAllFunPar();
        RefParamVec = P.Val(:);
    end
end
