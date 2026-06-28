function PerCropCell = partitionByCrop(Pool, Ncrops, Args)
    % Inverse of poolCalibCandidates: split a pooled survivor table back
    % into one MATLAB table per crop. The per-crop X, Y are preserved through
    % pooling (the pool never overwrites them — XFULL, YFULL are added as
    % separate columns), so no restoration is needed.
    % Input  : - Pool   - MATLAB table from poolCalibCandidates with a
    %                     CropID column. Typically also carries XFULL, YFULL
    %                     columns inherited from imProc.cat.addXYfull.
    %          - Ncrops - number of crops the pool was built from (empty
    %                     output slots get well-formed empty tables instead
    %                     of being silently dropped).
    %          * Args - struct or key/val with:
    %             .DropPoolColumns - logical, default true. Drops XFULL,
    %                                YFULL, CropID after splitting. Set false
    %                                to preserve them on each per-crop slice
    %                                (joint-fit code wants them through).
    % Output : - PerCropCell - 1xNcrops cell of MATLAB tables. Crop i gets
    %                          the rows of Pool with Pool.CropID == i. Empty
    %                          tables for crops absent from the pool.
    % Author : D. Kovaleva (April 2026)
    % Example: PerCrop = PhotCalibTrans.partitionByCrop(Pool, 24);
    %          % then: PC(i).SourceData = AstroCatalog(PerCrop{i});

    arguments
        Pool
        Ncrops              double {mustBeInteger, mustBePositive}
        Args.DropPoolColumns logical = true
    end

    PerCropCell = cell(1, Ncrops);
    PoolNames = Pool.Properties.VariableNames;

    if isempty(Pool) || height(Pool) == 0 || ~ismember('CropID', PoolNames)
        EmptyTpl = Pool([], :);
        if Args.DropPoolColumns
            EmptyTpl = removeIfPresent(EmptyTpl, {'XFULL','YFULL','CropID'});
        end
        [PerCropCell{:}] = deal(EmptyTpl);
        return;
    end

    for I = 1:Ncrops
        RowMask = Pool.CropID == I;
        Slice = Pool(RowMask, :);
        if Args.DropPoolColumns
            Slice = removeIfPresent(Slice, {'XFULL','YFULL','CropID'});
        end
        PerCropCell{I} = Slice;
    end
end

% =========================================================================
function Tab = removeIfPresent(Tab, ColsToDrop)
    Present = intersect(Tab.Properties.VariableNames, ColsToDrop, 'stable');
    if ~isempty(Present)
        Tab = removevars(Tab, Present);
    end
end
