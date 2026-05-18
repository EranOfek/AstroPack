function Fig = plotPhotSeries(Input, Args)
    % Plot one or more per-crop quantities versus epoch.
    % Description: For each requested quantity, builds a [Nepoch x Ncrop]
    %              matrix and draws per-crop lines against epoch (central
    %              crops bold/solid, peripheral thin/dashed) with the
    %              cross-crop median overlaid. One figure per quantity.
    %
    %              The quantity is resolved, in order, from:
    %                (1) a field of an explicit 'HeaderData' struct
    %                    (per-key [Nepoch x Ncrop] matrices, e.g. from
    %                    pipeline.last.load.extractHeaderData);
    %                (2) a PhotCalibTrans quantity via resolvePCParam
    %                    (fitted parameter / fit-quality / scalar property),
    %                    or 'IntegralT' (integral transmission);
    %                (3) a FITS header keyword of an AstroImage input.
    %
    %              Replaces plotPhotFittedParams, plotPhotAsympRMS,
    %              plotPhotFWHM and the time-series half of plotPhotIntegralT.
    %              (The integral-transmission focal-plane mosaic is produced
    %              by plotPhotParamMosaic with Quantity 'IntegralT'.)
    %
    % Input  : - Input - a PhotCalibTrans array / cell of such arrays / a
    %            struct with a .PC field, or a cell of AstroImage arrays
    %            (anything resolveInput accepts). A Result struct's
    %            .HeaderData is picked up automatically.
    %          * ...,key,val,...
    %            'Quantity'       - Quantity name, or cell of names (one
    %                               figure each). Required.
    %            'HeaderData'     - Struct of per-key [Nepoch x Ncrop]
    %                               matrices. Default [] (auto from a Result
    %                               struct, else unused).
    %            'Overlay'        - Optional second quantity drawn as a grey
    %                               background series (e.g. 'PH_RMS' behind
    %                               'ARMS'). Default '' (none).
    %            'CropsToAnalyze' - Crop indices to draw. Default [] (all).
    %            'Ncrop'          - Crop count. Default [] (inferred).
    %            'TileOrder'      - 'rowmajor' | 'colmajor' central-crop
    %                               classification. Default 'rowmajor'.
    %            'WvlRange'       - [min max] wavelength range [Angstrom] for
    %                               the 'IntegralT' quantity. Default []
    %                               (full range).
    % Output : - Fig - array of created figure handles (one per quantity).
    % Author : photCalib package refactor (2026-05)
    % Example: plotPhotSeries(R.PC, 'Quantity', {'TauAod500','PWV_cm','Norm'});
    %          plotPhotSeries(R, 'Quantity', 'ARMS', 'Overlay', 'PH_RMS');
    %          plotPhotSeries(R.PC, 'Quantity', 'IntegralT');
    %          plotPhotSeries(AIcell, 'Quantity', 'FWHM');

    arguments
        Input
        Args.Quantity
        Args.HeaderData               = []
        Args.Overlay     {mustBeText} = ''
        Args.CropsToAnalyze double    = []
        Args.Ncrop                    = []
        Args.TileOrder   {mustBeTextScalar} = 'rowmajor'
        Args.WvlRange                 = []
    end

    Fig = gobjects(0);

    Cell = resolveInput(Input);
    if isempty(Cell); return; end

    % Auto-pick HeaderData from a Result struct
    if isempty(Args.HeaderData) && isstruct(Input) && isscalar(Input) ...
            && isfield(Input, 'HeaderData')
        Args.HeaderData = Input.HeaderData;
    end

    Nvisit = numel(Cell);

    % --- Crop count ----------------------------------------------------
    if ~isempty(Args.Ncrop)
        Ncrop = Args.Ncrop;
    else
        Ncrop = 0;
        for Iv = 1:Nvisit
            if ~isempty(Cell{Iv}); Ncrop = max(Ncrop, numel(Cell{Iv})); end
        end
        if Ncrop == 0 && isstruct(Args.HeaderData)
            F = fieldnames(Args.HeaderData);
            if ~isempty(F); Ncrop = size(Args.HeaderData.(F{1}), 2); end
        end
        if Ncrop == 0; Ncrop = 24; end
    end

    Quantities = Args.Quantity;
    if ~iscell(Quantities); Quantities = {Quantities}; end

    Central  = centralCrops(Args.TileOrder, Ncrop);
    Crops    = Args.CropsToAnalyze;
    if isempty(Crops); Crops = 1:Ncrop; end
    EpochVec = 1:Nvisit;

    for Iq = 1:numel(Quantities)
        Q   = char(Quantities{Iq});
        Mat = i_buildMatrix(Q, Cell, Args, Nvisit, Ncrop);
        if all(isnan(Mat(:)))
            warning('photCalib:plotPhotSeries:NoData', ...
                'No data resolved for quantity %s.', Q);
            continue;
        end

        F = figure('Name', sprintf('%s vs Epoch', Q), ...
                   'Position', [50 50 800 450]);

        if ~isempty(Args.Overlay)
            OMat = i_buildMatrix(char(Args.Overlay), Cell, Args, Nvisit, Ncrop);
            lineVsEpoch(gca, EpochVec, OMat, 'CropsToUse', Crops, ...
                'CentralCrops', Central, 'Color', [0.7 0.7 0.7], ...
                'MedianWidth', 3);
        end
        lineVsEpoch(gca, EpochVec, Mat, 'CropsToUse', Crops, ...
            'CentralCrops', Central);

        xlabel('Epoch');
        ylabel(strrep(Q, '_', '\_'));
        MedAll = median(median(Mat(:,Crops(Crops<=Ncrop)), 2, 'omitnan'), 'omitnan');
        if ~isempty(Args.Overlay)
            title(sprintf('%s vs epoch (median %.4g; grey = %s)', ...
                strrep(Q,'_','\_'), MedAll, strrep(char(Args.Overlay),'_','\_')));
        else
            title(sprintf('%s vs epoch (%d crops, median %.4g)', ...
                strrep(Q,'_','\_'), numel(Crops), MedAll));
        end

        Fig(end+1) = F; %#ok<AGROW>
    end
end

% =========================================================================
function Mat = i_buildMatrix(Quantity, Cell, Args, Nvisit, Ncrop)
    % Build a [Nvisit x Ncrop] matrix of one quantity from the resolved input.
    Mat = nan(Nvisit, Ncrop);

    % (1) explicit HeaderData struct
    if isstruct(Args.HeaderData)
        HDfield = matlab.lang.makeValidName(Quantity);
        if isfield(Args.HeaderData, HDfield)
            M  = Args.HeaderData.(HDfield);
            Nr = min(size(M,1), Nvisit);
            Nc = min(size(M,2), Ncrop);
            Mat(1:Nr, 1:Nc) = M(1:Nr, 1:Nc);
            return;
        end
    end

    % (2)/(3) per-object resolution
    for Iv = 1:Nvisit
        El = Cell{Iv};
        if isempty(El); continue; end
        for Ic = 1:min(numel(El), Ncrop)
            Obj = El(Ic);
            if isa(Obj, 'PhotCalibTrans')
                if ~Obj.Success; continue; end
                if strcmpi(Quantity, 'IntegralT')
                    try
                        if isempty(Args.WvlRange)
                            Mat(Iv,Ic) = Obj.integralTransmission();
                        else
                            Mat(Iv,Ic) = Obj.integralTransmission( ...
                                'WvlRange', Args.WvlRange);
                        end
                    catch
                    end
                else
                    Mat(Iv,Ic) = pipeline.last.quality.photCalib.resolvePCParam( ...
                        Obj, Quantity);
                end
            elseif isa(Obj, 'AstroImage')
                try
                    V = Obj.HeaderData.getVal(Quantity);
                    if isnumeric(V) && isscalar(V) && isfinite(V)
                        Mat(Iv,Ic) = V;
                    end
                catch
                end
            end
        end
    end
end
