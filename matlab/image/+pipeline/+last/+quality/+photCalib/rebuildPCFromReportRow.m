function PC = rebuildPCFromReportRow(Row)
    % Reconstruct a PhotCalibTrans from a batchPhotCalibTrans Report row
    % Description: Rebuilds the TransModel + Tran2DObj + observation
    %              metadata carried by one row of the Report struct array
    %              returned by pipeline.last.quality.photCalib.batchPhotCalibTrans.
    %              The result is a fresh PhotCalibTrans whose TransModel
    %              parameter values, Tran2DObj.ParX and observation
    %              context all match the fit that produced the row — so
    %              downstream evaluation calls (evaluateZP, evaluateMag,
    %              addMag, addZP, ...) behave as if you had held onto the
    %              original PC object.
    %
    %              What is NOT restored: SpecData, SourceData, CalibTrajectory,
    %              Chi2/DOF/RMS scalars, and any log/status arrays. This
    %              rebuild is for prediction/evaluation, not for resuming
    %              a fit.
    % Input  : - Row - one element of Report from batchPhotCalibTrans. Must
    %                  carry .FitParams (or the legacy .FittedParams),
    %                  .FunListName, .Tran2DType,
    %                  .XPixel, .YPixel, .OptSeqName, .ObsMetadata.
    % Output : - PC - fresh PhotCalibTrans object.
    % Author : D. Kovaleva (Jul 2026)
    % Example:
    %   Rep = pipeline.last.quality.photCalib.batchPhotCalibTrans(BaseDir);
    %   PC  = pipeline.last.quality.photCalib.rebuildPCFromReportRow(Rep(1));
    %   ZP  = PC.evaluateZP(X, Y);

    arguments
        Row (1,1) struct
    end

    if isfield(Row, 'CalFound') && ~Row.CalFound
        warning('pipeline:last:quality:photCalib:rebuildPCFromReportRow:UnfittedRow', ...
                'This row was not successfully fitted (CalFound=false). The returned PC will carry the model at initial defaults, not fitted values.');
    end

    % Metadata defaults (used when a field is absent from an older Report).
    Defaults = struct('AirMass', 1.2, 'ExpTime', 1, 'NCoadd', 1, ...
                      'Temp', 15, 'Pressure', 965);

    if isfield(Row, 'ObsMetadata') && isstruct(Row.ObsMetadata) && ...
            ~isempty(fieldnames(Row.ObsMetadata))
        Meta = Row.ObsMetadata;
        % Fill any missing subfield with the safe default.
        F = fieldnames(Defaults);
        for K = 1:numel(F)
            if ~isfield(Meta, F{K}) || isempty(Meta.(F{K}))
                Meta.(F{K}) = Defaults.(F{K});
            end
        end
    else
        Meta = Defaults;
        % Pull AIRMASS from the row's own header-derived field if available —
        % it comes from the same header keyword `calibrate` reads for AirMass.
        if isfield(Row, 'AIRMASS') && isfinite(Row.AIRMASS)
            Meta.AirMass = Row.AIRMASS;
        end
    end

    FunListName = getRowField(Row, 'FunListName', 'DefaultLASTFunList');

    % XPixel / YPixel defaults are RunMode-dependent. Per-crop fits size the
    % Tran2D basis to the 1716x1716 sub-image; joint fits size it to the
    % full 6388x9576 frame. Older Reports (built before these fields were
    % added) omit XPixel/YPixel entirely — infer the pair from RunMode so
    % the rebuilt Tran2DObj centres on the correct pixel range instead of
    % silently landing at the per-crop default for a joint fit.
    RunMode = getRowField(Row, 'RunMode', 'per-crop');
    if strcmpi(RunMode, 'joint')
        DefaultXPixel = 6388;
        DefaultYPixel = 9576;
    else
        DefaultXPixel = 1716;
        DefaultYPixel = 1716;
    end
    XPixel = getRowField(Row, 'XPixel', DefaultXPixel);
    YPixel = getRowField(Row, 'YPixel', DefaultYPixel);

    PC = PhotCalibTrans( ...
        'AirMass',  Meta.AirMass, ...
        'ExpTime',  Meta.ExpTime, ...
        'NCoadd',   Meta.NCoadd, ...
        'Temp',     Meta.Temp, ...
        'Pressure', Meta.Pressure);

    % Replay calibrate's model-construction path (Step 2 + Step 3):
    % predefSeqCompositeFun -> pick FunList by name -> CompositeFun.model.
    ZenithAngle = acosd(1 / max(Meta.AirMass, 1.0));
    [FunCat, StageCat] = imUtil.calib.predefSeqCompositeFun( ...
        'ZenithAngle_deg', ZenithAngle, ...
        'Pressure_mbar',   Meta.Pressure, ...
        'Temperature_C',   Meta.Temp);

    FunList    = FunCat.(FunListName);
    OptSeqName = getRowField(Row, 'OptSeqName', 'LAST_NormLin');
    OptSeq     = StageCat.(OptSeqName);

    Tran2DType = getRowField(Row, 'Tran2DType', 'cheby1_4_xt');

    Model = tools.math.fun.CompositeFun.model(FunList, ...
        'MetadataValues',       {'ZenithAngle_deg', ZenithAngle, ...
                                 'Pressure_mbar',   Meta.Pressure, ...
                                 'Temperature_C',   Meta.Temp}, ...
        'OptimizationSequence', OptSeq, ...
        'UseTran2D',            true, ...
        'Tran2DType',           Tran2DType, ...
        'XPixel',               XPixel, ...
        'YPixel',               YPixel);

    % Push fitted values back onto the model. Match by parameter name so we
    % are robust to differences in parameter ordering between the model
    % that produced Row and the model we just built.
    Info = Model.getAllFunPar();
    % Accept either the new field name (FitParams) or the legacy one
    % (FittedParams) so Reports built before the July 2026 rename still
    % rebuild cleanly.
    if isfield(Row, 'FitParams') && isstruct(Row.FitParams) && ~isempty(fieldnames(Row.FitParams))
        FP = Row.FitParams;
    elseif isfield(Row, 'FittedParams') && isstruct(Row.FittedParams) && ~isempty(fieldnames(Row.FittedParams))
        FP = Row.FittedParams;
    else
        FP = struct();
    end
    for K = 1:numel(Info.Name)
        FieldName = matlab.lang.makeValidName(char(Info.Name{K}));
        if isfield(FP, FieldName)
            Info.Val(K) = FP.(FieldName);
        end
    end
    Model.setAllFunPar(Info);

    % Tran2D coefficients — full 10-vec if the model has a Tran2DObj.
    if isfield(FP, 'Tran2D_ParX') && ~isempty(Model.Tran2DObj)
        Coefs = FP.Tran2D_ParX(:).';
        if numel(Coefs) == numel(Model.Tran2DObj.ParX)
            Model.Tran2DObj.ParX = Coefs;
        else
            warning('pipeline:last:quality:photCalib:rebuildPCFromReportRow:Tran2DMismatch', ...
                    'Tran2D_ParX has %d coeffs but rebuilt Tran2DObj expects %d — leaving Tran2DObj.ParX at zero.', ...
                    numel(Coefs), numel(Model.Tran2DObj.ParX));
        end
    end

    PC.TransModel = Model;
end


function V = getRowField(Row, FieldName, Default)
    % Fetch Row.(FieldName) tolerantly:
    %   - missing field       -> Default
    %   - empty value         -> Default
    %   - numeric NaN scalar  -> Default (so the "row exists but wasn't
    %                            populated" case behaves like missing)
    if ~isfield(Row, FieldName)
        V = Default;
        return;
    end
    V = Row.(FieldName);
    if isempty(V)
        V = Default;
        return;
    end
    if isnumeric(V) && isscalar(V) && ~isfinite(V)
        V = Default;
    end
end
