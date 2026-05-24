function [Result, PhotCalib, FitRes] = fitPhotCalibTrans(Obj, Args)
    % Transmission-based absolute photometric calibration wrapper
    % Description: Wrapper function for PhotCalibTrans class that performs
    %              transmission-based photometric calibration on a vector of
    %              AstroImages, AstroCatalogs, AstroDiff, or AstroZOGY objects.
    %              For AstroDiff/AstroZOGY input, calibrates the sub-images
    %              specified by DiffCalibProps (default: .New and .Ref) via
    %              recursive calls.
    %              Optionally evaluates legacy LIMMAG/BACKMAG keywords via
    %              evaluateLimMag/evaluateBackMag. Disabled by default while
    %              imProc.calib.fitPhotCalibMag remains the pipeline default
    %              source of those keywords.
    % Input  :  - AstroImage, AstroCatalog, AstroDiff, or AstroZOGY
    %                 object (scalar or vector).
    %          * ...,key,val,...
    %            'Verbose' - Enable verbose output. Default is true.
    %            'AddMagErr' - Add magnitude error columns. Default is false.
    %            'CreateNewObj' - Copy input object. Default is false.
    %            'DiffCalibProps' - Properties to calibrate for AstroDiff.
    %                         Default is {'New', 'Ref'}.
    %            'AddMag' - Add calibrated magnitude columns. Default is true.
    %            'MagSystem' - Magnitude system ('AB' or 'Vega'). Default is 'AB'.
    %            'MagColPrefix' - Prefix for calibrated magnitude column names.
    %                         FLUX_<suffix> -> <prefix><suffix> (e.g.
    %                         FLUX_APER_3 -> MAG_AB_APER_3). Pass 'MAG_' to drop
    %                         the _AB token; the calibrated magnitudes then
    %                         overwrite the instrumental MAG_<suffix> columns in
    %                         place (insertCol deletes the old column first).
    %                         Stamped onto each PC object's MagColPrefix
    %                         property after calibrate; addMag, calcAperCorr,
    %                         evaluateLimMag, applyConstBand and the per-epoch
    %                         applyPhotCalibShifts all read it from there, so
    %                         write/read sides always agree. Does not affect
    %                         AB_ZP. Default is 'MAG_AB_'.
    %            'FluxColName' - Flux column name. Default is 'FLUX_APER_3'.
    %            'AddZP' - Add ZP column. Default is true.
    %            'UpdateHeader' - Update header with results. Default is true.
    %            'CalibArgs' - Cell array of key-value pairs forwarded to
    %                         PhotCalibTrans.calibrate. Build via local
    %                         predefCalibArgs() or manually. Default is {}.
    %            'ApplyConstBand' - Apply constant-band correction after
    %                         computing AB magnitudes. Adds MAG_CB_* columns
    %                         (or overwrites MAG_AB_* if ConstBandOutputMode='replace').
    %                         Default is false.
    %            'ConstBandParams' - Struct or .mat path with global atmospheric
    %                         parameters for constant band. Build via
    %                         PCArray.buildConstBandParams() for aggregate,
    %                         or with 'Source','single' for a single reference
    %                         crop. Required when ApplyConstBand=true.
    %            'ConstBandOutputMode' - 'newcol' or 'replace'. Default is 'newcol'.
    %            'ConstBandPrefix' - Column prefix for newcol mode. Default is 'MAG_CB_'.
    %            'EvaluateLimMag'  - Evaluate limiting magnitude (legacy LIMMAG).
    %                         Default is false (legacy fitPhotCalibMag emits it).
    %                         Uses Args.FluxColName / Args.MagSystem to locate
    %                         FLUXERR_<suffix> and MAG_<system>_<suffix>.
    %            'EvaluateBackMag' - Evaluate sky surface brightness (legacy BACKMAG).
    %                         Default is false; AstroImage input only.
    %            'LimMagMinSN'     - Lower SN bound for LimMag fit. Default is 5.
    %            'LimMagMaxSN'     - Upper SN bound for LimMag fit. Default is 50.
    %            'LimMagSN'        - SN at which to evaluate LimMag. Default is 5.
    % Output : - Result - Input object with updated catalog and header.
    %          - PhotCalib - For AstroImage/AstroCatalog: [1 x Nobj] array.
    %                        For AstroDiff/AstroZOGY: [Nobj x Nprops] array
    %                        (column per property, e.g., PhotCalib(i,1)=New).
    %          - FitRes - For AstroImage/AstroCatalog: [Nobj x 1] struct array.
    %                     For AstroDiff/AstroZOGY: [Nobj x Nprops] struct array.
    %                     Fields: .RMS, .Residuals, .NCalUsed, .NumClipped,
    %                             .Chi2, .StatusLog
    % Author : D. Kovaleva (Jan 2026)
    % Reference: Garrappa et al. 2025, A&A 699, A50.
    % Example: AI = io.files.load2('LAST_image.mat');
    %          % All LAST defaults (no CalibArgs needed):
    %          [Result, PC, FitRes] = imProc.calib.fitPhotCalibTrans(AI);
    %          fprintf('NCalUsed=%d, RMS=%.4f\n', FitRes.NCalUsed, FitRes.RMS);
    %          % Override specific calibrate settings:
    %          [Result, PC, FitRes] = imProc.calib.fitPhotCalibTrans(AI, ...
    %              'CalibArgs', {'UseTran2D', false});
    %          % Reuse config across multiple images:
    %          CalibArgs = {'SearchRadius', 3};
    %          for I = 1:numel(AIvec)
    %              Result(I) = imProc.calib.fitPhotCalibTrans(AIvec(I), 'CalibArgs', CalibArgs, 'Verbose', false);
    %          end
    %          % AstroDiff/AstroZOGY (all LAST defaults):
    %          [Result, PC, FR] = imProc.calib.fitPhotCalibTrans(AD);
    %          % Calibrate only New:
    %          [Result, PC, FR] = imProc.calib.fitPhotCalibTrans(AD, 'DiffCalibProps', {'New'});
    %          % Per-source airmass mode:
    %          [Result, PC, FitRes] = imProc.calib.fitPhotCalibTrans(AI, ...
    %              'CalibArgs', {'PerSourceAirmass', true});
    %          % Emit LIMMAG/BACKMAG from this path (instead of legacy fitPhotCalibMag):
    %          [Result, PC, FitRes] = imProc.calib.fitPhotCalibTrans(AI, ...
    %              'EvaluateLimMag', true, 'EvaluateBackMag', true);

    arguments
        Obj  % AstroImage, AstroCatalog, AstroDiff, or AstroZOGY

        % Calibration config forwarded to calibrate (cell array of key-value pairs)
        Args.CalibArgs cell = {}

        Args.CreateNewObj logical = false
        Args.DiffCalibProps cell = {'New', 'Ref'}
        Args.AddMag logical = true
        Args.MagSystem char = 'AB'
        Args.MagColPrefix = 'MAG_'   % Prefix for calibrated MAG column names ('MAG_' drops _AB, overwrites instrumental MAG_*)
        Args.FluxColName = 'FLUX_APER_3'
        Args.AddZP logical = true
        Args.UpdateHeader logical = true
        Args.AddMagErr logical = false
        Args.CalcAperCorr logical = true
        Args.ApplyAperCorr logical = true
        % Default false because legacy fitPhotCalibMag still writes LIMMAG/BACKMAG.
        % Flip to true (and set WriteLimBackMag=false in fitPhotCalibMag) once this
        % path becomes the pipeline default.
        Args.EvaluateLimMag  logical = false
        Args.EvaluateBackMag logical = false
        Args.LimMagMinSN    double = 5
        Args.LimMagMaxSN    double = 50
        Args.LimMagSN       double = 5
        Args.ApplyConstBand logical = false   % Apply constant-band correction
        Args.ConstBandParams = []             % Struct or .mat path
        Args.ConstBandOutputMode = 'newcol'   % 'newcol' or 'replace'
        Args.ConstBandPrefix = 'MAG_CB_'      % Prefix for constant-band columns
        Args.match_catsHTMArgs = {}        
        Args.Verbose logical = false
    end

    % ====================================================================
    % VALIDATE INPUT AND COPY
    % ====================================================================
 %tic
    if Args.CreateNewObj
        Result = Obj.copy();
    else
        Result = Obj;
    end

    % Apply predefCalibArgs defaults when no CalibArgs provided
    if isempty(Args.CalibArgs)
        Args.CalibArgs = predefCalibArgs();
    end

    % ====================================================================
    % ASTRODIFF / ASTROZOGY: delegate to recursive calls per sub-property
    % ====================================================================

    if isa(Obj, 'AstroDiff')
        Nobj = numel(Result);
        Nprops = numel(Args.DiffCalibProps);

        if Args.Verbose
            fprintf('\n=== TRANSMISSION-BASED PHOTOMETRIC CALIBRATION ===\n');
            fprintf('Input: %s, %d object(s), calibrating: %s\n', ...
                class(Obj), Nobj, strjoin(Args.DiffCalibProps, ', '));
        end

        % Initialize output arrays [Nobj x Nprops]
        PhotCalib = PhotCalibTrans.empty(0);
        FitRes = struct('RMS', cell(Nobj, Nprops), ...
                        'Residuals', cell(Nobj, Nprops), ...
                        'NCalUsed', cell(Nobj, Nprops), ...
                        'NumClipped', cell(Nobj, Nprops), ...
                        'Chi2', cell(Nobj, Nprops), ...
                        'StatusLog', cell(Nobj, Nprops));

        for Iprop = 1:Nprops
            PropName = Args.DiffCalibProps{Iprop};

            % Extract AstroImage array from each element
            Images = AstroImage.empty(0);
            for Iobj = 1:Nobj
                Images(Iobj) = Result(Iobj).(PropName);
            end

            if Args.Verbose
                fprintf('\nCalibrating .%s images...\n', PropName);
            end

            % Recursive call — calibrate as regular AstroImage array
            [Images, PC_prop, FR_prop] = imProc.calib.fitPhotCalibTrans(Images, ...
                'CalibArgs', Args.CalibArgs, ...
                'Verbose', Args.Verbose, 'AddMagErr', Args.AddMagErr, ...
                'AddMag', Args.AddMag, 'MagSystem', Args.MagSystem, ...
                'MagColPrefix', Args.MagColPrefix, ...
                'FluxColName', Args.FluxColName, 'AddZP', Args.AddZP, ...
                'CalcAperCorr', Args.CalcAperCorr, 'ApplyAperCorr', Args.ApplyAperCorr, ...
                'EvaluateLimMag', Args.EvaluateLimMag, 'EvaluateBackMag', Args.EvaluateBackMag, ...
                'LimMagMinSN', Args.LimMagMinSN, 'LimMagMaxSN', Args.LimMagMaxSN, ...
                'LimMagSN', Args.LimMagSN, ...
                'UpdateHeader', Args.UpdateHeader, 'CreateNewObj', false);

            % Store calibrated images back into Result
            for Iobj = 1:Nobj
                Result(Iobj).(PropName) = Images(Iobj);
            end

            % Accumulate outputs: column Iprop
            PhotCalib(1:Nobj, Iprop) = PC_prop(:);
            FitRes(1:Nobj, Iprop) = FR_prop(:);
        end

        % Done — skip the AstroImage/AstroCatalog loop below
    else

    % ====================================================================
    % ASTROIMAGE / ASTROCATALOG: main calibration path
    % ====================================================================

    IsAstroImage = isa(Obj, 'AstroImage');

    Nobj = numel(Result);

    if Args.Verbose
        fprintf('\n=== TRANSMISSION-BASED PHOTOMETRIC CALIBRATION ===\n');
        fprintf('Processing %d object(s)\n', Nobj);
    end

    % Initialize output array of PhotCalibTrans objects
    PhotCalib = PhotCalibTrans.empty(0, Nobj);

    % Initialize output FitRes structure array
    FitRes = struct('RMS', cell(Nobj, 1), ...
                    'Residuals', cell(Nobj, 1), ...
                    'NCalUsed', cell(Nobj, 1), ...
                    'NumClipped', cell(Nobj, 1), ...
                    'Chi2', cell(Nobj, 1), ...
                    'StatusLog', cell(Nobj, 1));
    for Iinit = 1:Nobj
        FitRes(Iinit).RMS = NaN;
        FitRes(Iinit).Residuals = [];
        FitRes(Iinit).NCalUsed = 0;
        FitRes(Iinit).NumClipped = 0;
        FitRes(Iinit).Chi2 = NaN;
        FitRes(Iinit).StatusLog = struct('Function', {}, 'Level', {}, ...
            'Message', {}, 'Identifier', {}, 'Timestamp', {});
    end

    % ====================================================================
    % LOOP OVER OBJECTS
    % ====================================================================

    for Iobj = 1:Nobj
        if Args.Verbose
            fprintf('\n--- Object %d/%d ---\n', Iobj, Nobj);
        end

        % Create new PhotCalibTrans object for this image
        PC = PhotCalibTrans();

        % ----------------------------------------------------------------
        % Perform calibration
        % ----------------------------------------------------------------

        PC = PC.calibrate(Result(Iobj), Args.CalibArgs{:}, ...
            'CalcAperCorr', Args.CalcAperCorr, ...
            'MagSystem', Args.MagSystem,...
            'match_catsHTMArgs',Args.match_catsHTMArgs,...
            'Verbose', Args.Verbose);

        % Stamp the calibrated-magnitude column-naming prefix onto the PC
        % object. Every downstream method (addMag, calcAperCorr, evaluateLimMag,
        % applyConstBand, and the per-epoch applyPhotCalibShifts) reads this
        % property — set once here, no per-call threading.
        PC.MagColPrefix = Args.MagColPrefix;

        % ----------------------------------------------------------------
        % Post-calibration processing
        % ----------------------------------------------------------------

        if ~isempty(PC.TransModel)
            % Add calibrated magnitude (and optionally ZP) columns.
            % AperCorr is NOT yet applied — will be applied below after calcAperCorr.
            if Args.AddMag
                if IsAstroImage
                    Result(Iobj).CatData = PC.addMag(Result(Iobj).CatData, ...
                        'MagSystem', Args.MagSystem, ...
                        'AddMagErr', Args.AddMagErr, ...
                        'AddZP', Args.AddZP, ...
                        'ApplyConstBand', Args.ApplyConstBand, ...
                        'ConstBandParams', Args.ConstBandParams, ...
                        'ConstBandOutputMode', Args.ConstBandOutputMode, ...
                        'ConstBandPrefix', Args.ConstBandPrefix);
                else
                    Result(Iobj) = PC.addMag(Result(Iobj), ...
                        'MagSystem', Args.MagSystem, ...
                        'AddMagErr', Args.AddMagErr, ...
                        'AddZP', Args.AddZP, ...
                        'ApplyConstBand', Args.ApplyConstBand, ...
                        'ConstBandParams', Args.ConstBandParams, ...
                        'ConstBandOutputMode', Args.ConstBandOutputMode, ...
                        'ConstBandPrefix', Args.ConstBandPrefix);
                end
            elseif Args.AddZP
                if IsAstroImage
                    Result(Iobj).CatData = PC.addZP(Result(Iobj).CatData, ...
                        'MagSystem', Args.MagSystem);
                else
                    Result(Iobj) = PC.addZP(Result(Iobj), ...
                        'MagSystem', Args.MagSystem);
                end
            end

            % Compute aperture corrections using freshly-created MAG_AB_* columns,
            % then (if requested) apply them to those same columns in place.
            if Args.CalcAperCorr
                if IsAstroImage
                    CatRef = Result(Iobj).CatData;
                else
                    CatRef = Result(Iobj);
                end
                PC = PC.calcAperCorr(CatRef, 'Verbose', Args.Verbose);

                if Args.ApplyAperCorr && ~isempty(PC.AperCorr) && ~isempty(PC.AperCorrColNames)
                    AllColNamesC = CatRef.Table.Properties.VariableNames;
                    for Iap = 1:numel(PC.AperCorrColNames)
                        ColName = PC.AperCorrColNames{Iap};
                        if ismember(ColName, AllColNamesC) && isfinite(PC.AperCorr(Iap))
                            Vals = CatRef.getCol(ColName) + PC.AperCorr(Iap);
                            CatRef.replaceCol(Vals, ColName);
                        end
                    end
                end
            end

            % Limiting magnitude and sky surface brightness (legacy LIMMAG/BACKMAG)
            % Run after aperture correction so LimMag fits the corrected MAG_AB_* values.
            if Args.EvaluateLimMag
                if IsAstroImage
                    CatLim = Result(Iobj).CatData;
                else
                    CatLim = Result(Iobj);
                end
                PC = PC.evaluateLimMag(CatLim, ...
                    'FluxColName', Args.FluxColName, ...
                    'MagSystem',   Args.MagSystem, ...
                    'MinSN',       Args.LimMagMinSN, ...
                    'MaxSN',       Args.LimMagMaxSN, ...
                    'LimMagSN',    Args.LimMagSN);
            end
            if Args.EvaluateBackMag && IsAstroImage
                PC = PC.evaluateBackMag(Result(Iobj));
            end

            % Update header if requested (always per-crop, for diagnostics)
            if Args.UpdateHeader
                if IsAstroImage
                    PC.photCalibTransToHeader(Result(Iobj).HeaderData);
                else
                    % For AstroCatalog, create new header (not stored)
                    PC.photCalibTransToHeader(AstroHeader());
                end
                if Args.Verbose
                    fprintf('  Header updated with calibration results\n');
                end
            end
        else
            if Args.Verbose
                fprintf('  Calibration unsuccessful - adding NaN-filled columns for uniformity\n');
            end

            % Get catalog reference
            if IsAstroImage
                CatObj = Result(Iobj).CatData;
            else
                CatObj = Result(Iobj);
            end

            % Add NaN-filled columns for uniformity with successful calibrations
            if ~isempty(CatObj) && ~isempty(CatObj.Table) && height(CatObj.Table) > 0
                Nrows = height(CatObj.Table);
                NaNcol = nan(Nrows, 1);

                % Add magnitude columns if requested (NaN-filled for failed calibration)
                if Args.AddMag
                    % Same naming prefix the successful path would have used
                    MagPrefix = Args.MagColPrefix;
                    % Find FLUX columns and create corresponding magnitude columns
                    ColNames = CatObj.Table.Properties.VariableNames;
                    FluxCols = ColNames(startsWith(ColNames, 'FLUX_APER_') | strcmp(ColNames, 'FLUX_PSF'));
                    for iCol = 1:length(FluxCols)
                        NewMagColName = strrep(FluxCols{iCol}, 'FLUX_', MagPrefix);
                        CatObj = CatObj.insertCol(NaNcol, Inf, {NewMagColName});
                        % MAGERR column written only when an error source is
                        % available: matching FLUXERR_<suffix>, or (for
                        % FLUX_PSF specifically) the SN column from which
                        % MagErr = 1.086 / SN.
                        FluxErrColName = strrep(FluxCols{iCol}, 'FLUX_', 'FLUXERR_');
                        HasErrSource = ismember(FluxErrColName, ColNames) || ...
                            (strcmp(FluxCols{iCol}, 'FLUX_PSF') && ismember('SN', ColNames));
                        if HasErrSource
                            MagErrColName = regexprep(NewMagColName, '^MAG_', 'MAGERR_');
                            CatObj = CatObj.insertCol(NaNcol, Inf, {MagErrColName});
                        end
                    end
                end

                % Add ZP column if requested
                if Args.AddZP
                    ZPColName = [Args.MagSystem, '_ZP'];
                    CatObj = CatObj.insertCol(NaNcol, Inf, {ZPColName});
                end

                % Store back
                if IsAstroImage
                    Result(Iobj).CatData = CatObj;
                else
                    Result(Iobj) = CatObj;
                end
            end

            % Write PT_* keywords to header with NaN values for uniformity
            if Args.UpdateHeader && IsAstroImage
                H = Result(Iobj).HeaderData;
                H = H.replaceVal(...
                    {'PT_RMS', 'PT_CHI2', 'PT_DOF', 'PT_NCALIB', 'PT_SUCC', 'PT_AREF', 'PT_SPEC'}, ...
                    {NaN,      NaN,       NaN,      -1,          false,     'SMART v2.9.8', 'GaiaDR3'});

                % NaN fills for legacy LIMMAG/BACKMAG (only when feature enabled)
                if Args.EvaluateLimMag
                    H = H.replaceVal('LIMMAG', NaN);
                end
                if Args.EvaluateBackMag
                    H = H.replaceVal('BACKMAG', NaN);
                end

                % Write function parameters with NaN values and 0 flags
                if ~isempty(PC.TransModel) && ~isempty(PC.TransModel.Funs)
                    Funs = PC.TransModel.Funs;
                    for iFun = 1:length(Funs)
                        Fun = Funs(iFun);
                        % Function reference
                        if iFun == 1 && strcmp(Fun.Desc, 'Normalization')
                            FunRef = '@(Lambda,Par)Par';
                        else
                            FunRef = func2str(Fun.Handle);
                        end
                        H = H.replaceVal(sprintf('PT_%d_N', iFun), FunRef);

                        % Parameters: values = NaN, flags = 0
                        for iPar = 1:length(Fun.Par)
                            H = H.replaceVal(sprintf('PT_%d_V%d', iFun, iPar), NaN);
                            H = H.replaceVal(sprintf('PT_%d_F%d', iFun, iPar), 0);
                        end
                    end
                end

                Result(Iobj).HeaderData = H;
            end
        end

        % Extract fit results from last stage (even if failed)
        if ~isempty(PC.FitResults)
            LastStage = PC.FitResults(end);
            FitRes(Iobj).RMS = LastStage.RMS;
            FitRes(Iobj).Residuals = LastStage.Residuals;
            FitRes(Iobj).NCalUsed = LastStage.NCalUsed;
            FitRes(Iobj).NumClipped = LastStage.NumClipped;
            FitRes(Iobj).Chi2 = LastStage.Chi2;
        elseif PC.NoRADec
            % Special code: RA/Dec columns missing in catalog
            FitRes(Iobj).NCalUsed = -1;
        end

        % Get StatusLog from CompositeFun (TransModel)
        % Note: PhotCalibTrans uses inherited Logger from Component class
        if ~isempty(PC.TransModel) && isprop(PC.TransModel, 'StatusLog') && ~isempty(PC.TransModel.StatusLog)
            FitRes(Iobj).StatusLog = PC.TransModel.StatusLog;
        end

        % Store calibration object
        PhotCalib(Iobj) = PC;
    end
  %toc
    % ====================================================================
    % SUMMARY
    % ====================================================================

    if Args.Verbose
        SuccessMask = arrayfun(@(p) ~isempty(p.TransModel), PhotCalib);
        Nsuccess = sum(SuccessMask);
        fprintf('\n=== CALIBRATION COMPLETE ===\n');
        fprintf('Successful: %d/%d objects\n', Nsuccess, Nobj);
        if Nsuccess > 0
            RMSvals = [FitRes(SuccessMask).RMS];
            fprintf('RMS range: %.4f - %.4f mag\n', min(RMSvals), max(RMSvals));
        end
    end

    end  % end of if/else for AstroDiff vs AstroImage/AstroCatalog
end

function CalibArgs = predefCalibArgs(Args)
    % Predefined calibration workflow arguments for PhotCalibTrans.calibrate
    % Description: Returns a cell array of key-value pairs with default
    %              calibration settings for LAST telescope photometric
    %              calibration. Users can override individual fields,
    %              then pass the cell array as 'CalibArgs' to
    %              fitPhotCalibTrans.
    % Input  : * ...,key,val,...
    %            'Lambda'           - Wavelength grid [Ang]. Default (3000:20:11000)'.
    %            'SearchRadius'     - Calibrator match radius [arcsec]. Default 2.
    %            'MagRange'         - Calibrator mag range [min max]. Default [11.5 15.5].
    %            'FilterNegFlux'    - Remove negative-flux sources. Default true.
    %            'MinSN2'           - Min SN_2 for calibrators (0=skip). Default 10.
    %            'FunListName'      - Transmission function list name. Default 'DefaultLASTFunList'.
    %            'CustomFunList'    - Custom function list (overrides FunListName). Default [].
    %            'OptSeqName'       - Optimization sequence name. Default 'LAST_NormLin'.
    %            'CustomOptSeq'     - Custom opt sequence (overrides OptSeqName). Default [].
    %            'Tran2DType'       - Tran2D polynomial type. Default 'cheby1_4_xt'.
    %            'UseTran2D'        - Enable Tran2D. Default true.
    %            'XPixel'           - Detector X size in pixels (sets Tran2D
    %                                 normalisation, ParNX = [XPixel/2, XPixel/2]).
    %                                 Default 1716.
    %            'YPixel'           - Detector Y size in pixels. Default 1716.
    %            'Tran2DPerturbStd' - Std-dev for one-shot N(0,std) randn-seeding
    %                                 of Tran2D ParX before fit. Coefficients
    %                                 are perturbed once before stage 1 and
    %                                 propagate through pre-FieldCorrection
    %                                 stages (1-3) only; stage 4 overwrites
    %                                 ParX with the linear LS fit. 0 disables.
    %                                 Default 0.
    %            'CalibCatName'     - catsHTM catalog with reference spectra,
    %                                 forwarded to selectCalibrators.
    %                                 Default 'GAIADR3spec'.
    %            'MinSN'            - Lower S/N gate on calibrator candidates.
    %                                 Default 5.
    %            'MaxSN'            - Upper S/N gate. Default 1000.
    %            'FilterBadFlags'   - Apply the FLAGS bitmask filter in
    %                                 selectCalibrators. Default true.
    %            'MagColName'       - Magnitude column used for the MagRange
    %                                 filter and audit delta-mag.
    %                                 Default 'MAG_APER_3'.
    %            'SpFluxCol'        - Spectral flux column indices in
    %                                 CalibCatName as [flux_start, flux_end,
    %                                 err_start, err_end]. Default
    %                                 [7, 349, 350, 692] for GAIADR3spec.
    %            'BadBitNames'      - Cell of bit-name strings flagged as bad
    %                                 (resolved via BitDictionary
    %                                 'BitMask.Image.Default').
    %                                 Default {'Saturated','NaN','Negative',
    %                                 'CR_DeltaHT','NearEdge'}.
    %            'AuditCalibrators' - Toggle the step-0 calibrator audit in
    %                                 selectCalibrators. When false (default)
    %                                 the call path is unchanged.
    %            'AuditCatName'     - Gaia photometric catsHTM catalog used to
    %                                 fetch BP-RP and BP-RP excess factor for
    %                                 the audit. Default 'GAIADR3'.
    %            'AuditBPRPExcessFactorMax' - Reject if Gaia counterpart's
    %                                 phot_bp_rp_excess_factor exceeds this
    %                                 value. Default 1.3.
    %            'AuditBPRPMax'     - Reject if Gaia counterpart's bp_rp exceeds
    %                                 this value. Default 1.5.
    %            'AuditLASTNearestDist' - Reject if the candidate's nearest LAST
    %                                 neighbour (self-excluded) lies within this
    %                                 distance [arcsec]. Default 20.
    %            'AuditLASTDeltaMag' - Reject if the candidate's nearest LAST
    %                                 neighbour has |delta-mag| (using
    %                                 MagColName) below this value. Default 2.
    %            'WeightingMode'    - Weighting mode. Default 'spectral'.
    %            'FluxErrColName'   - Flux error column. Default 'FluxErr'.
    %            'SigmaClipMethod'  - 'median' or 'weighted'. Default 'median'.
    %            'FluxErrorNorm'    - Flux error normalization. Default 0.5.
    %            'AirmassColName'   - Per-source airmass column. Default 'AIRMASS'.
    %            'PerSourceAirmass' - Enable per-source airmass. Default false.
    %            'AperCorrMethod'   - Aperture corr method. Default 'median'.
    %            'AperCorrSNColName'- S/N column for aper corr. Default 'SN'.
    %            'AperCorrMinSN'    - Min S/N for aper corr. Default 30.
    %            'N_ARMS'           - N brightest calibrators for ARMS. Default 20.
    % Output : - Cell array of key-value pairs for PhotCalibTrans.calibrate.
    % Author : D. Kovaleva (Feb 2026)
    % Example: CalibArgs = predefCalibArgs();
    %          CalibArgs = predefCalibArgs('SearchRadius', 3);
    %          CalibArgs = predefCalibArgs('FilterNegFlux', false, 'MinSN2', 0);

    arguments
        % Wavelength grid
        Args.Lambda           = (3000:20:11000)'  % Transmission wavelength grid [Angstrom]

        % Calibrator selection
        Args.SearchRadius     = 2         % arcsec
        Args.MagRange         = [11.5 16.0]
        Args.FilterNegFlux logical = true % Remove sources with negative flux
        Args.MinSN2           = 10        % Minimum SN_2 for calibrators (0 to skip)

        % Transmission model
        Args.FunListName      = 'DefaultLASTFunList'
        Args.CustomFunList    = []
        Args.OptSeqName       = 'LAST_NormLin'
        Args.CustomOptSeq     = []
        Args.Tran2DType       = 'cheby1_4_xt'
        Args.UseTran2D logical = true
        Args.XPixel           = 1716   % Detector X size [pix]; sets Tran2D centre = XPixel/2
        Args.YPixel           = 1716   % Detector Y size [pix]; sets Tran2D centre = YPixel/2
        Args.Tran2DPerturbStd = 0   % Std-dev for randn-seed of Tran2D ParX (one shot before stage 1); 0 disables

        % Calibrator selection (forwarded to selectCalibrators)
        Args.CalibCatName     = 'GAIADR3spec'   % catsHTM catalog with reference spectra
        Args.MinSN            = 5               % Lower S/N gate on calibrator candidates
        Args.MaxSN            = 1000            % Upper S/N gate
        Args.FilterBadFlags logical = true      % Apply FLAGS bitmask filter
        Args.MagColName       = 'MAG_APER_3'    % Mag column for MagRange + audit deltaMag
        Args.SpFluxCol        = [7, 349, 350, 692]  % [flux_start, flux_end, err_start, err_end]
        Args.BadBitNames      = {'Saturated', 'NaN', 'Negative', 'CR_DeltaHT', 'NearEdge'}
        Args.AuditCalibrators logical = false   % Toggle step-0 audit (default: keep status quo)
        Args.AuditCatName     = 'GAIADR3'       % Gaia photometric catalog for the audit
        Args.AuditBPRPExcessFactorMax = 1.3
        Args.AuditBPRPMax     = 1.5
        Args.AuditLASTNearestDist = 20          % arcsec
        Args.AuditLASTDeltaMag = 2              % mag

        % Weighting
        Args.WeightingMode    = 'spectral'  % 'none', 'spectral', 'flux', 'combined'
        Args.FluxErrColName   = 'FluxErr'
        Args.SigmaClipMethod  = 'median'    % 'median' or 'weighted'
        Args.FluxErrorNorm    = 0.5

        % Per-source airmass
        Args.AirmassColName   = 'AIRMASS'   % Column name for per-source airmass
        Args.PerSourceAirmass logical = false  % Enable per-source airmass mode

        % Aperture correction
        Args.AperCorrMethod   = 'median'    % 'median' or 'weighted'
        Args.AperCorrSNColName = 'SN'       % S/N column for filtering
        Args.AperCorrMinSN    = 30          % Minimum S/N for aperture correction stars

        % Bright-star RMS
        Args.N_ARMS           = 20          % N brightest calibrators for ARMS (0=skip)
    end

    CalibArgs = namedargs2cell(Args);
end