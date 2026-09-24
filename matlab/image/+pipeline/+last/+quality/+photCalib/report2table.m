function T = report2table(Rep)
    % Flatten a batchPhotCalibTrans Report struct-array into a table.
    % Description: Converts the struct array returned by
    %              pipeline.last.quality.photCalib.batchPhotCalibTrans into a
    %              flat MATLAB table: the scalar per-config columns plus the
    %              Tran2D Chebyshev coefficients expanded into one column each.
    %              Coefficient columns are named by their PHYSICAL meaning
    %              according to each row's Tran2DType (e.g. kx0, kx, ky, kx2,
    %              ky2, kxy, ...); unknown bases fall back to Cheb_01..Cheb_NN.
    %              When the Report mixes Tran2DTypes, the table carries the
    %              union of coefficient names and each row fills only the
    %              coefficients its own basis defines (others are NaN) - so a
    %              given column always holds the same physical coefficient
    %              regardless of the basis's internal parameter ordering.
    %              The heavy CalibTrajectory / FitParams / ObsMetadata fields
    %              are dropped.
    % Input  : - Rep - the struct array from batchPhotCalibTrans (each element
    %                  carries .FitParams.Tran2D_ParX and .Tran2DType).
    % Output : - T   - a table, one row per Report entry. Scalar columns
    %                  (VisitStem, RunMode, OptSeqName, Tran2DType, CropNumber,
    %                  AIRMASS, FWHM, Norm, TauAOD500, PWV_cm, Center_Ang, RMS,
    %                  MedianRMS, ARMS, Chi2, DOF, NCalib, CalFound,
    %                  NSelectedCalibrators, ErrorMessage) followed by the named
    %                  Chebyshev-coefficient columns.
    % Author : D. Kovaleva (Jul 2026)
    % Example:
    %   Rep = pipeline.last.quality.photCalib.batchPhotCalibTrans(BaseDir, ...);
    %   T   = pipeline.last.quality.photCalib.report2table(Rep);
    %   plot(T.AIRMASS, T.kx0, '.');            % DC term vs airmass
    arguments
        Rep struct
    end

    N = numel(Rep);
    if N == 0
        T = table();
        return;
    end
    Fn = fieldnames(Rep);

    % --- Scalar / char columns (only those present, for robustness to older
    %     Report shapes). Built field-by-field so char fields of unequal
    %     length become cellstr columns cleanly. ---
    ScalarFields = {'VisitStem','RunMode','OptSeqName','FunListName','Tran2DType', ...
                    'XPixel','YPixel','CropNumber','AIRMASS','FWHM', ...
                    'Norm','TauAOD500','PWV_cm','Center_Ang', ...
                    'RMS','MedianRMS','ARMS','Chi2','DOF','NCalib', ...
                    'CalFound','NSelectedCalibrators','ErrorMessage'};
    ScalarFields = ScalarFields(ismember(ScalarFields, Fn));

    T = table();
    for I = 1:numel(ScalarFields)
        F = ScalarFields{I};
        Vals = {Rep.(F)};
        if all(cellfun(@(v) ischar(v) || isstring(v), Vals))
            Col = cellfun(@char, Vals, 'UniformOutput', false).';    % cellstr column
        elseif all(cellfun(@(v) (isnumeric(v) || islogical(v)) && (isscalar(v) || isempty(v)), Vals))
            % numeric/logical scalar per row -> double column, NaN for empties
            % (logical values coerce to 0/1).
            Col = nan(N, 1);
            for K = 1:N
                if ~isempty(Vals{K}); Col(K) = double(Vals{K}); end
            end
        else
            % mixed / non-scalar - keep as a cell column rather than error
            Col = Vals.';
        end
        T.(F) = Col;
    end

    % --- Chebyshev coefficient columns, named per Tran2DType ---
    RowNames = cell(N, 1);
    RowVals  = cell(N, 1);
    AllNames = {};
    for K = 1:N
        Type = '';
        if isfield(Rep(K), 'Tran2DType') && ~isempty(Rep(K).Tran2DType)
            Type = char(Rep(K).Tran2DType);
        end
        ParX = [];
        if isfield(Rep(K), 'FitParams') && isstruct(Rep(K).FitParams) ...
                && isfield(Rep(K).FitParams, 'Tran2D_ParX')
            ParX = Rep(K).FitParams.Tran2D_ParX(:).';
        end
        Names = chebNames(Type, numel(ParX));
        RowNames{K} = Names;
        RowVals{K}  = ParX;
        AllNames = union(AllNames, Names, 'stable');
    end

    if ~isempty(AllNames)
        Cheb = nan(N, numel(AllNames));
        for K = 1:N
            for J = 1:numel(RowNames{K})
                Col = find(strcmp(AllNames, RowNames{K}{J}), 1);
                Cheb(K, Col) = RowVals{K}(J);
            end
        end
        T = [T, array2table(Cheb, 'VariableNames', AllNames)];
    end
end


function Names = chebNames(Type, N)
    % Physical coefficient names for a Tran2D basis, in ParX order. Falls back
    % to indexed Cheb_NN names when the type is unknown or the count does not
    % match the basis definition (guards against schema drift).
    switch lower(Type)
        case 'cheby1_2'
            Base = {'kx0','kx','ky','kx2','ky2','kxy'};
        case 'cheby1_4'
            Base = {'kx0','kx','ky','kx2','ky2','kxy', ...
                    'kx3','ky3','kx2y','kxy2','kx4','ky4','kx3y','kxy3','kx2y2'};
        case {'cheby1_4_xt','cheby1_4_xt_constrainedxy'}
            Base = {'kx0','kx','kx2','kx3','kx4','ky','ky2','ky3','ky4','kxy'};
        otherwise
            Base = {};
    end
    if numel(Base) == N
        Names = Base;
    else
        Names = arrayfun(@(k) sprintf('Cheb_%02d', k), 1:N, 'UniformOutput', false);
    end
end
