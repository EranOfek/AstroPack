function [Result, PhotCalib, FitRes] = fitPhotCalibTrans(Obj, Args)
    % Transmission-based absolute photometric calibration wrapper
    % Description: Wrapper function for PhotCalibTrans class that performs
    %              transmission-based photometric calibration on a vector of
    %              AstroImages, AstroCatalogs, AstroDiff, or AstroZOGY objects.
    %              For AstroDiff/AstroZOGY input, calibrates the sub-images
    %              specified by DiffCalibProps (default: .New and .Ref) via
    %              recursive calls.
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
    %                         PhotCalibTrans.buildConstBandParams(PCArray) for
    %                         aggregate, or with 'Source','single' for a single
    %                         reference crop. Required when ApplyConstBand=true.
    %            'ConstBandOutputMode' - 'newcol' or 'replace'. Default is 'newcol'.
    %            'ConstBandPrefix' - Column prefix for newcol mode. Default is 'MAG_CB_'.
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

    arguments
        Obj  % AstroImage, AstroCatalog, AstroDiff, or AstroZOGY

        % Calibration config forwarded to calibrate (cell array of key-value pairs)
        Args.CalibArgs cell = {}

        Args.CreateNewObj logical = false
        Args.DiffCalibProps cell = {'New', 'Ref'}
        Args.AddMag logical = true
        Args.MagSystem char = 'AB'
        Args.FluxColName = 'FLUX_APER_3'
        Args.AddZP logical = true
        Args.UpdateHeader logical = true
        Args.AddMagErr logical = false
        Args.CalcAperCorr logical = true
        Args.ApplyAperCorr logical = false
        Args.ApplyConstBand logical = false   % Apply constant-band correction
        Args.ConstBandParams = []             % Struct or .mat path
        Args.ConstBandOutputMode = 'newcol'   % 'newcol' or 'replace'
        Args.ConstBandPrefix = 'MAG_CB_'      % Prefix for constant-band columns
        Args.PhotSys = 'percrop'       % 'percrop' | 'refshape' | 'refzp' | 'refzp_raw' | 'global' | 'mean'
        Args.RefCrop = 10              % Reference crop index for non-percrop modes
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
                'FluxColName', Args.FluxColName, 'AddZP', Args.AddZP, ...
                'CalcAperCorr', Args.CalcAperCorr, 'ApplyAperCorr', Args.ApplyAperCorr, ...
                'PhotSys', Args.PhotSys, 'RefCrop', Args.RefCrop, ...
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
            'MagSystem', Args.MagSystem, 'Verbose', Args.Verbose);

        % ----------------------------------------------------------------
        % Post-calibration processing
        % ----------------------------------------------------------------

        if PC.Success
            % Add calibrated magnitude (and optionally ZP) columns
            % For non-percrop modes, addMag is deferred to post-loop pass
            if strcmp(Args.PhotSys, 'percrop')
                if Args.AddMag
                    if IsAstroImage
                        Result(Iobj).CatData = PC.addMag(Result(Iobj).CatData, ...
                            'MagSystem', Args.MagSystem, ...
                            'AddMagErr', Args.AddMagErr, ...
                            'AddZP', Args.AddZP, ...
                            'ApplyAperCorr', Args.ApplyAperCorr, ...
                            'ApplyConstBand', Args.ApplyConstBand, ...
                            'ConstBandParams', Args.ConstBandParams, ...
                            'ConstBandOutputMode', Args.ConstBandOutputMode, ...
                            'ConstBandPrefix', Args.ConstBandPrefix);
                    else
                        Result(Iobj) = PC.addMag(Result(Iobj), ...
                            'MagSystem', Args.MagSystem, ...
                            'AddMagErr', Args.AddMagErr, ...
                            'AddZP', Args.AddZP, ...
                            'ApplyAperCorr', Args.ApplyAperCorr, ...
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
                H = H.replaceVal(...
                    {'PT_RMS', 'PT_CHI2', 'PT_DOF', 'PT_NCALIB', 'PT_SUCC', 'PT_AREF', 'PT_SPEC'}, ...
                    {NaN,      NaN,       NaN,      -1,          false,     'SMART v2.9.8', 'GaiaDR3'});

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

    % ====================================================================
    % POST-LOOP: UNIFORM PHOTOMETRY MODES
    % ====================================================================

    if ~strcmp(Args.PhotSys, 'percrop') && Args.AddMag
        RefIdx = Args.RefCrop;

        if RefIdx == 0
            % Weighted mean of all successful crops' parameters
            % Weight by 1/RMS^2 (inverse variance)
            AllParams = [];
            AllWeights = [];
            for Iobj = 1:Nobj
                if PhotCalib(Iobj).Success && PhotCalib(Iobj).TransModel.RMS > 0
                    P = PhotCalib(Iobj).TransModel.getAllFunPar();
                    AllParams = [AllParams; P.Val(:)'];
                    AllWeights = [AllWeights; 1 ./ PhotCalib(Iobj).TransModel.RMS.^2];
                end
            end
            if isempty(AllParams)
                warning('imProc:calib:fitPhotCalibTrans:NoSuccessCrops', ...
                        'No successful crops for weighted mean. Falling back to percrop.');
            else
                W = AllWeights / sum(AllWeights);  % normalize
                RefParamVec = W' * AllParams;      % [1 x Npar] weighted mean
                RefParamVec = RefParamVec(:);

                if Args.Verbose
                    fprintf('\nUniform photometry mode: %s (weighted mean, %d crops)\n', ...
                        Args.PhotSys, size(AllParams, 1));
                end
            end
        elseif RefIdx < 1 || RefIdx > Nobj
            error('imProc:calib:fitPhotCalibTrans:BadRefCrop', ...
                  'RefCrop=%d is out of range [0, %d] (0=weighted mean).', RefIdx, Nobj);
        elseif ~PhotCalib(RefIdx).Success
            warning('imProc:calib:fitPhotCalibTrans:RefCropFailed', ...
                    'Reference crop %d calibration failed. Falling back to percrop.', RefIdx);
        else
            % Get reference transmission parameters from single crop
            RefTransParams = PhotCalib(RefIdx).TransModel.getAllFunPar();
            RefParamVec = RefTransParams.Val;

            if Args.Verbose
                fprintf('\nUniform photometry mode: %s (RefCrop=%d)\n', Args.PhotSys, RefIdx);
            end
        end

        if exist('RefParamVec', 'var')

            switch Args.PhotSys
                case 'refshape'
                    % Use reference spectral shape, keep per-crop Norm + Tran2D
                    for Iobj = 1:Nobj
                        if PhotCalib(Iobj).Success
                            if IsAstroImage
                                Result(Iobj).CatData = PhotCalib(Iobj).addMag(Result(Iobj).CatData, ...
                                    'MagSystem', Args.MagSystem, ...
                                    'AddMagErr', Args.AddMagErr, ...
                                    'AddZP', Args.AddZP, ...
                                    'ApplyAperCorr', Args.ApplyAperCorr, ...
                                    'RefTransParams', RefParamVec, ...
                                    'ApplyConstBand', Args.ApplyConstBand, ...
                                    'ConstBandParams', Args.ConstBandParams, ...
                                    'ConstBandOutputMode', Args.ConstBandOutputMode, ...
                                    'ConstBandPrefix', Args.ConstBandPrefix);
                            else
                                Result(Iobj) = PhotCalib(Iobj).addMag(Result(Iobj), ...
                                    'MagSystem', Args.MagSystem, ...
                                    'AddMagErr', Args.AddMagErr, ...
                                    'AddZP', Args.AddZP, ...
                                    'ApplyAperCorr', Args.ApplyAperCorr, ...
                                    'RefTransParams', RefParamVec, ...
                                    'ApplyConstBand', Args.ApplyConstBand, ...
                                    'ConstBandParams', Args.ConstBandParams, ...
                                    'ConstBandOutputMode', Args.ConstBandOutputMode, ...
                                    'ConstBandPrefix', Args.ConstBandPrefix);
                            end
                        end
                    end

                case {'refzp', 'refzp_raw'}
                    % Use full reference parameters (including Norm) for all crops.
                    % 'refzp': center-normalize Tran2D (remove Norm/kx0 degeneracy)
                    % 'refzp_raw': no Tran2D normalization
                    DoNormTran2D = strcmp(Args.PhotSys, 'refzp');
                    for Iobj = 1:Nobj
                        if PhotCalib(Iobj).Success
                            if IsAstroImage
                                Result(Iobj).CatData = PhotCalib(Iobj).addMag(Result(Iobj).CatData, ...
                                    'MagSystem', Args.MagSystem, ...
                                    'AddMagErr', Args.AddMagErr, ...
                                    'AddZP', Args.AddZP, ...
                                    'ApplyAperCorr', Args.ApplyAperCorr, ...
                                    'RefTransParams', RefParamVec, ...
                                    'UseRefNorm', true, ...
                                    'NormTran2D', DoNormTran2D, ...
                                    'ApplyConstBand', Args.ApplyConstBand, ...
                                    'ConstBandParams', Args.ConstBandParams, ...
                                    'ConstBandOutputMode', Args.ConstBandOutputMode, ...
                                    'ConstBandPrefix', Args.ConstBandPrefix);
                            else
                                Result(Iobj) = PhotCalib(Iobj).addMag(Result(Iobj), ...
                                    'MagSystem', Args.MagSystem, ...
                                    'AddMagErr', Args.AddMagErr, ...
                                    'AddZP', Args.AddZP, ...
                                    'ApplyAperCorr', Args.ApplyAperCorr, ...
                                    'RefTransParams', RefParamVec, ...
                                    'UseRefNorm', true, ...
                                    'NormTran2D', DoNormTran2D, ...
                                    'ApplyConstBand', Args.ApplyConstBand, ...
                                    'ConstBandParams', Args.ConstBandParams, ...
                                    'ConstBandOutputMode', Args.ConstBandOutputMode, ...
                                    'ConstBandPrefix', Args.ConstBandPrefix);
                            end
                        end
                    end

                case {'global', 'mean'}
                    % Placeholder for future modes
                    warning('imProc:calib:fitPhotCalibTrans:NotImplemented', ...
                            'PhotSys=''%s'' is not yet implemented. Falling back to percrop.', Args.PhotSys);
                    for Iobj = 1:Nobj
                        if PhotCalib(Iobj).Success
                            if IsAstroImage
                                Result(Iobj).CatData = PhotCalib(Iobj).addMag(Result(Iobj).CatData, ...
                                    'MagSystem', Args.MagSystem, ...
                                    'AddMagErr', Args.AddMagErr, ...
                                    'AddZP', Args.AddZP, ...
                                    'ApplyAperCorr', Args.ApplyAperCorr, ...
                            'ApplyConstBand', Args.ApplyConstBand, ...
                            'ConstBandParams', Args.ConstBandParams, ...
                            'ConstBandOutputMode', Args.ConstBandOutputMode, ...
                            'ConstBandPrefix', Args.ConstBandPrefix);
                            else
                                Result(Iobj) = PhotCalib(Iobj).addMag(Result(Iobj), ...
                                    'MagSystem', Args.MagSystem, ...
                                    'AddMagErr', Args.AddMagErr, ...
                                    'AddZP', Args.AddZP, ...
                                    'ApplyAperCorr', Args.ApplyAperCorr, ...
                            'ApplyConstBand', Args.ApplyConstBand, ...
                            'ConstBandParams', Args.ConstBandParams, ...
                            'ConstBandOutputMode', Args.ConstBandOutputMode, ...
                            'ConstBandPrefix', Args.ConstBandPrefix);
                            end
                        end
                    end
            end
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
        Args.MagRange         = [11.5 15.5]
        Args.FilterNegFlux logical = true % Remove sources with negative flux
        Args.MinSN2           = 10        % Minimum SN_2 for calibrators (0 to skip)

        % Transmission model
        Args.FunListName      = 'DefaultLASTFunList'
        Args.CustomFunList    = []
        Args.OptSeqName       = 'LAST_NormLin'
        Args.CustomOptSeq     = []
        Args.Tran2DType       = 'cheby1_4_xt'
        Args.UseTran2D logical = true

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
