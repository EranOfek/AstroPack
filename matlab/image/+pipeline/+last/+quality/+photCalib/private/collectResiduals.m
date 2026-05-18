function D = collectResiduals(PCcell, Args)
    % Pool calibrator residuals (and an X quantity) across a PC cell.
    % Description: Walks a cell of PhotCalibTrans arrays (one cell per
    %              visit/epoch), and for every successful crop pulls the used
    %              calibrators' residuals from SourceData.Table together with
    %              a chosen X quantity. This is the shared collection loop
    %              behind the residual-vs-X family of plots.
    %
    %              'XField' resolution order:
    %                'instrumental' - -2.5*log10(Flux)
    %                'InvFlux'      - 1./Flux  (background diagnostic)
    %                'A-B'          - difference of two SourceData columns
    %                <column name>  - that SourceData.Table column
    %                <PC property>  - scalar PhotCalibTrans property
    %                                 (e.g. 'AirMass'), broadcast per crop
    %                otherwise      - falls back to instrumental if Flux
    %                                 exists, else the crop is skipped.
    % Input  : - PCcell - cell of PhotCalibTrans arrays.
    %          * ...,key,val,...
    %            'CropsToAnalyze' - Crop indices to include. Default [] (all).
    %            'XField'         - X quantity name (see above).
    %                               Default 'MAG_AB'.
    %            'Normalize'      - Divide residuals by MagErr. Default false.
    % Output : - D - struct with column vectors X, Residual, CropID, EpochID
    %            (one row per pooled calibrator), and scalars NCalib (total
    %            calibrators) and NEpoch (epochs that contributed).
    % Author : photCalib package refactor (2026-05)
    % Example: D = collectResiduals(PCcell, 'XField', 'instrumental');

    arguments
        PCcell                cell
        Args.CropsToAnalyze   double             = []
        Args.XField           {mustBeTextScalar} = 'MAG_AB'
        Args.Normalize        logical            = false
    end

    X = []; Res = []; Crop = []; Epoch = []; NEpoch = 0;

    for Iv = 1:numel(PCcell)
        if isempty(PCcell{Iv}); continue; end
        PCv   = PCcell{Iv};
        Crops = Args.CropsToAnalyze;
        if isempty(Crops); Crops = 1:numel(PCv); end

        EpochContributed = false;
        for Ic = Crops
            if Ic > numel(PCv); continue; end
            PCobj = PCv(Ic);
            if isempty(PCobj) || ~PCobj.Success; continue; end
            if isempty(PCobj.SourceData); continue; end
            Tab = PCobj.SourceData.Table;
            if isempty(Tab); continue; end
            Cols = Tab.Properties.VariableNames;
            if ~ismember('Residuals', Cols); continue; end

            if ismember('Used', Cols)
                Used = logical(Tab.Used);
            else
                Used = true(height(Tab), 1);
            end

            R = Tab.Residuals(Used);
            if Args.Normalize
                if ~ismember('MagErr', Cols); continue; end
                R = R ./ Tab.MagErr(Used);
            end

            Xv = localResolveX(Args.XField, Tab, Cols, Used, PCobj);
            if isempty(Xv); continue; end

            Good  = isfinite(R) & isfinite(Xv);
            X     = [X;     Xv(Good)];          %#ok<AGROW>
            Res   = [Res;   R(Good)];           %#ok<AGROW>
            Crop  = [Crop;  repmat(Ic, nnz(Good), 1)];  %#ok<AGROW>
            Epoch = [Epoch; repmat(Iv, nnz(Good), 1)];  %#ok<AGROW>
            EpochContributed = true;
        end
        if EpochContributed; NEpoch = NEpoch + 1; end
    end

    D = struct('X', X, 'Residual', Res, 'CropID', Crop, 'EpochID', Epoch, ...
               'NCalib', numel(Res), 'NEpoch', NEpoch);
end

% =========================================================================
function Xv = localResolveX(XField, Tab, Cols, Used, PCobj)
    % Resolve the requested X quantity to a column vector over Used rows.
    Xv = [];
    switch XField
        case 'instrumental'
            if ismember('Flux', Cols)
                Xv = -2.5 * log10(Tab.Flux(Used));
            end
        case 'InvFlux'
            if ismember('Flux', Cols)
                Xv = 1 ./ Tab.Flux(Used);
            end
        otherwise
            Tok = strsplit(XField, '-');
            if numel(Tok) == 2
                Ca = strtrim(Tok{1});
                Cb = strtrim(Tok{2});
            else
                Ca = '';
                Cb = '';
            end
            if ~isempty(Ca) && ~isempty(Cb) && ...
                    ismember(Ca, Cols) && ismember(Cb, Cols)
                Xv = Tab.(Ca)(Used) - Tab.(Cb)(Used);
            elseif ismember(XField, Cols)
                Xv = Tab.(XField)(Used);
            elseif isprop(PCobj, XField)
                Sc = PCobj.(XField);
                if isnumeric(Sc) && isscalar(Sc)
                    Xv = repmat(Sc, nnz(Used), 1);
                end
            elseif ismember('Flux', Cols)
                Xv = -2.5 * log10(Tab.Flux(Used));
            end
    end
    Xv = Xv(:);
end
