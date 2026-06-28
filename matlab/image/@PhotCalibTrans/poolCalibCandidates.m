function [Pool, FieldPool, CatHCell] = poolCalibCandidates(CandsCell, FieldTabCell, CatHCellIn, Args)
    % Concatenate per-crop calibrator-candidate tables into a single pooled
    % table for joint-visit selection / audit. Assumes XFULL, YFULL columns
    % have already been added to each per-crop CatData (e.g., via
    % imProc.cat.addXYfull called once on the AstroImage array before
    % findCalibCandidates ran).
    % Input  : - CandsCell    - 1xN cell of MATLAB tables (one per crop)
    %                           from findCalibCandidates. Each must already
    %                           carry XFULL, YFULL columns (alongside the
    %                           local X, Y).
    %          - FieldTabCell - 1xN cell of MATLAB tables (one per crop)
    %                           giving the full source field of each crop.
    %                           Same XFULL/YFULL assumption.
    %          - CatHCellIn   - 1xN cell of AstroCatalog references from
    %                           findCalibCandidates (the matched calibrator
    %                           rows in CalibCatName). Returned unchanged
    %                           so the orchestrator can keep all of
    %                           {Pool, FieldPool, CatHCell} together.
    %          * Args - struct or key/val with:
    %             .ColXfull  - column name (default 'XFULL'). Validated.
    %             .ColYfull  - column name (default 'YFULL'). Validated.
    % Output : - Pool      - one MATLAB table, sum(N_i) rows. Columns: every
    %                        column of CandsCell{i} plus CropID (1..N).
    %                        XFULL, YFULL come through from the input tables.
    %          - FieldPool - one MATLAB table, sum(M_i) rows. Same treatment
    %                        for the field tables.
    %          - CatHCell  - same as input CatHCellIn, returned for caller
    %                        convenience.
    % Author : D. Kovaleva (April 2026)
    % Example: % Up front in the orchestrator:
    %          [~, AI] = imProc.cat.addXYfull(AI);
    %          % then findCalibCandidates per crop, then:
    %          [Pool, FieldPool, CatHCell] = PhotCalibTrans.poolCalibCandidates( ...
    %              CandsCell, FieldTabCell, CatHCell);

    arguments
        CandsCell
        FieldTabCell
        CatHCellIn
        Args.ColXfull = 'XFULL'
        Args.ColYfull = 'YFULL'
    end

    Ncrops = numel(CandsCell);
    assert(numel(FieldTabCell) == Ncrops, ...
        'PhotCalibTrans:poolCalibCandidates: FieldTabCell length mismatch');

    PoolCell      = cell(Ncrops, 1);
    FieldPoolCell = cell(Ncrops, 1);
    for I = 1:Ncrops
        PoolCell{I}      = tagOne(CandsCell{I},    I, Args);
        FieldPoolCell{I} = tagOne(FieldTabCell{I}, I, Args);
    end

    NonEmptyPool      = ~cellfun(@isempty, PoolCell);
    NonEmptyFieldPool = ~cellfun(@isempty, FieldPoolCell);
    if any(NonEmptyPool)
        Pool = vertcat(PoolCell{NonEmptyPool});
    else
        Pool = table.empty;
    end
    if any(NonEmptyFieldPool)
        FieldPool = vertcat(FieldPoolCell{NonEmptyFieldPool});
    else
        FieldPool = table.empty;
    end
    CatHCell = CatHCellIn;
end

% =========================================================================
function Out = tagOne(Tab, CropID, Args)
    if isempty(Tab) || height(Tab) == 0
        Out = Tab;
        return;
    end
    VarNames = Tab.Properties.VariableNames;
    if ~ismember(Args.ColXfull, VarNames) || ~ismember(Args.ColYfull, VarNames)
        error('PhotCalibTrans:poolCalibCandidates:MissingXYfull', ...
              ['Pooled tables must already carry %s and %s columns. ', ...
               'Call imProc.cat.addXYfull on the AstroImage array before ', ...
               'findCalibCandidates so the candidate tables inherit them.'], ...
              Args.ColXfull, Args.ColYfull);
    end
    Out = Tab;
    Out.CropID = repmat(double(CropID), height(Out), 1);
end
