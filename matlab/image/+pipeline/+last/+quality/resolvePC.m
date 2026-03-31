function PCcell = resolvePC(Input)
    % Normalize various PC input formats into cell array of PhotCalibTrans arrays
    % Description: Accepts any of:
    %   - PC struct with .percrop field (from testPhotCalib/testPhotStability)
    %   - Result struct with .PC.percrop field (from Result.mat)
    %   - Raw PhotCalibTrans array (from fitPhotCalibTrans) — single epoch
    %   - Cell array of PhotCalibTrans arrays — multi-epoch
    %   Returns cell(Nvisits,1) where each cell is a PhotCalibTrans array.
    %
    % Input  : - Any of the above formats.
    % Output : - Cell array of PhotCalibTrans arrays, or {} if unresolvable.
    % Author : D. Kovaleva (Mar 2026)
    % Example: PCcell = pipeline.last.quality.resolvePC(R.PC);
    %          PCcell = pipeline.last.quality.resolvePC(PC_array);
    %          PCcell = pipeline.last.quality.resolvePC(R);

    PCcell = {};

    if isstruct(Input)
        if isfield(Input, 'PC') && isstruct(Input.PC) && isfield(Input.PC, 'percrop')
            % Full Result struct
            PCcell = Input.PC.percrop;
        elseif isfield(Input, 'percrop')
            % PC struct
            PCcell = Input.percrop;
        end
    elseif iscell(Input)
        % Cell array of PhotCalibTrans arrays
        PCcell = Input;
    elseif isa(Input, 'PhotCalibTrans')
        % Raw PhotCalibTrans array — wrap as single epoch
        PCcell = {Input};
    end
end
