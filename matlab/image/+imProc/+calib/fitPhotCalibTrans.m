function [Result, PhotCalib, FitRes] = fitPhotCalibTrans(Obj, Args)
    % Transmission-based absolute photometric calibration wrapper
    % Description: Wrapper function for PhotCalibTrans class that performs
    %              transmission-based photometric calibration on a vector of
    %              AstroImages or AstroCatalogs.
    % Input  :  Obj - AstroImage or AstroCatalog object (scalar or vector).
    %          * ...,key,val,...
    %            Calibrator selection:
    %            'SearchRadius' - Gaia matching radius [arcsec]. Default is 1.5.
    %            'MagRange' - Calibrator magnitude range [min max]. Default is [11.5 15.5].
    %            Transmission model:
    %            'FunListName' - Name of transmission function list. Default is 'DefaultLASTFunList'.
    %            'CustomFunList' - Custom function list (overrides FunListName). Default is [].
    %            'OptSeqName' - Name of optimization sequence. Default is 'LAST_NormLin'.
    %            'CustomOptSeq' - Custom optimization sequence (overrides OptSeqName). Default is [].
    %            'Tran2DType' - Position-dependent correction type. Default is 'cheby1_4_xt'.
    %            'UseTran2D' - Enable position-dependent correction. Default is true.
    %            Weighting:
    %            'WeightingMode' - Weighting mode for fitting. Options:
    %                              'none' - Unweighted least squares
    %                              'spectral' - Weight by Gaia spectral error propagation (default)
    %                              'flux' - Weight by LAST flux errors
    %                              'combined' - Quadrature sum of spectral and flux errors
    %            'FluxErrColName' - Column name for flux errors. Default is 'FluxErr'.
    %            'WeightedClipping' - Use weighted residuals for sigma clipping. Default is true.
    %            'FluxErrorNorm' - Normalization for synthetic flux in error calculation. Default is 0.5.
    %            Catalog update:
    %            'AddMag' - Add calibrated magnitude columns to catalog. Default is true.
    %            'MagSystem' - Magnitude system: 'AB' or 'Vega'.
    %                         Default is 'AB'. Vega is not yet implemented.
    %            'FluxColName' - Flux column for calibration fitting. Default is 'FLUX_APER_3'.
    %            'AddZP' - Add ZP column (position-dependent) to catalog. Default is false.
    %            Header update:
    %            'UpdateHeader' - Update AstroImage header with calibration results. Default is true.
    %            General:
    %            'CreateNewObj' - Copy input object. Default is false.
    %            'Verbose' - Enable verbose output. Default is true.
    % Output : - Result - Input object with updated catalog and header.
    %          - PhotCalib - Array of PhotCalibTrans objects (one per input object).
    %          - FitRes - Struct array [Nobj x 1] with fit results from last stage:
    %                     .RMS - Fit RMS [mag]
    %                     .Residuals - Residuals vector
    %                     .NCalUsed - Number of calibrators used (final, after clipping)
    %                     .NumClipped - Number of clipped outliers
    %                     .Chi2 - Chi-squared value
    %                     .StatusLog - Struct array of status messages (from CompositeFun.StatusLog)
    % Author : D. Kovaleva (Jan 2026)
    % Reference: Garrappa et al. 2025, A&A 699, A50.
    % Example: AI = io.files.load2('LAST_image.mat');
    %          [Result, PC, FitRes] = imProc.calib.fitPhotCalibTrans(AI);
    %          fprintf('NCalUsed=%d, RMS=%.4f\n', FitRes.NCalUsed, FitRes.RMS);
    %          % Without position correction:
    %          [Result, PC, FitRes] = imProc.calib.fitPhotCalibTrans(AI, 'UseTran2D', false);
    %          % With unweighted sigma clipping:
    %          [Result, PC, FitRes] = imProc.calib.fitPhotCalibTrans(AI, 'WeightedClipping', false);

    arguments
        Obj  % AstroImage or AstroCatalog

        % Calibrator selection
        Args.SearchRadius = 1.5  % arcsec
        Args.MagRange = [11.5 15.5]

        % Transmission model
        Args.FunListName = 'DefaultLASTFunList'
        Args.CustomFunList = []
        Args.OptSeqName =   'LAST_NormLin' %'DefaultLASTOptSeq'
        Args.CustomOptSeq = []
        Args.Tran2DType = 'cheby1_4_xt'
        Args.UseTran2D logical = true  % Enable position-dependent correction

        % Weighting options
        Args.WeightingMode = 'spectral'  % 'none', 'spectral', 'flux', 'combined'
        Args.FluxErrColName = 'FluxErr'  % Column name in SourceData for flux errors (relative errors)
        Args.WeightedClipping logical = true  % Use weighted residuals (r/σ) for sigma clipping
        Args.FluxErrorNorm = 0.5  % Normalization for synthetic flux in error calculation

        % Catalog update
        Args.AddMag logical = true
        Args.MagSystem char = 'AB'  % 'AB' or 'Vega' (placeholder)
        Args.FluxColName = 'FLUX_APER_3'
        Args.AddZP logical = true

        % Header update
        Args.UpdateHeader logical = true

        % General
        Args.CreateNewObj logical = false
        Args.Verbose logical = true
    end

    % ====================================================================
    % VALIDATE INPUT
    % ====================================================================
 %tic
    if isa(Obj, 'AstroImage')
        IsAstroImage = true;
    elseif isa(Obj, 'AstroCatalog')
        IsAstroImage = false;
    else
        error('Input must be AstroImage or AstroCatalog object');
    end

    % Copy object if requested
    if Args.CreateNewObj
        Result = Obj.copy();
    else
        Result = Obj;
    end

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
    % BUILD CALIBRATION ARGUMENTS (once, before loop)
    % ====================================================================

    CalibArgs = {...
        'FunListName', Args.FunListName, ...
        'OptSeqName', Args.OptSeqName, ...
        'Tran2DType', Args.Tran2DType, ...
        'UseTran2D', Args.UseTran2D, ...
        'SearchRadius', Args.SearchRadius, ...
        'MagRange', Args.MagRange, ...
        'WeightingMode', Args.WeightingMode, ...
        'FluxErrColName', Args.FluxErrColName, ...
        'WeightedClipping', Args.WeightedClipping, ...
        'FluxErrorNorm', Args.FluxErrorNorm, ...
        'MagSystem', Args.MagSystem, ...
        'Verbose', Args.Verbose};

    % Add custom function list if provided
    if ~isempty(Args.CustomFunList)
        CalibArgs = [CalibArgs, {'CustomFunList', Args.CustomFunList}];
    end

    % Add custom optimization sequence if provided
    if ~isempty(Args.CustomOptSeq)
        CalibArgs = [CalibArgs, {'CustomOptSeq', Args.CustomOptSeq}];
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

        PC = PC.calibrate(Result(Iobj), CalibArgs{:});

        % ----------------------------------------------------------------
        % Post-calibration processing
        % ----------------------------------------------------------------

        if PC.Success
            % Add calibrated magnitude columns if requested
            if Args.AddMag
                if IsAstroImage
                    Result(Iobj).CatData = PC.addMag(Result(Iobj).CatData, ...
                        'MagSystem', Args.MagSystem);
                else
                    Result(Iobj) = PC.addMag(Result(Iobj), ...
                        'MagSystem', Args.MagSystem);
                end
            end

            % Add ZP column if requested
            if Args.AddZP
                if IsAstroImage
                    Result(Iobj).CatData = PC.addZP(Result(Iobj).CatData, ...
                        'MagSystem', Args.MagSystem);
                else
                    Result(Iobj) = PC.addZP(Result(Iobj), ...
                        'MagSystem', Args.MagSystem);
                end
            end

            % Update header if requested
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
                    % Dynamic prefix: MAG_AB_ or MAG_VEGA_
                    MagPrefix = ['MAG_', Args.MagSystem, '_'];
                    % Find FLUX columns and create corresponding magnitude columns
                    ColNames = CatObj.Table.Properties.VariableNames;
                    FluxCols = ColNames(startsWith(ColNames, 'FLUX_APER_') | strcmp(ColNames, 'FLUX_PSF'));
                    for iCol = 1:length(FluxCols)
                        NewMagColName = strrep(FluxCols{iCol}, 'FLUX_', MagPrefix);
                        CatObj = CatObj.insertCol(NaNcol, Inf, {NewMagColName});
                        % Add NaN-filled magnitude error column for uniformity
                        CatObj = CatObj.insertCol(NaNcol, Inf, {[NewMagColName, '_ERR']});
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
                H = H.replaceVal('PT_RMS', NaN);
                H = H.replaceVal('PT_CHI2', NaN);
                H = H.replaceVal('PT_DOF', NaN);
                H = H.replaceVal('PT_NCALIB', -1);  % -1 = not searched (no RA/Dec), 0 = searched but none found
                H = H.replaceVal('PT_SUCC', false);
                H = H.replaceVal('PT_AREF', 'SMART v2.9.8');
                H = H.replaceVal('PT_SREF', 'MLv0.1LAST');
                H = H.replaceVal('PT_SPEC', 'GaiaDR3');

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
        Nsuccess = sum([PhotCalib.Success]);
        fprintf('\n=== CALIBRATION COMPLETE ===\n');
        fprintf('Successful: %d/%d objects\n', Nsuccess, Nobj);
        if Nsuccess > 0
            RMSvals = [FitRes([PhotCalib.Success]).RMS];
            fprintf('RMS range: %.4f - %.4f mag\n', min(RMSvals), max(RMSvals));
        end
    end
end
