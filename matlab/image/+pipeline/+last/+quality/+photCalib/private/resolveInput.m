function [Cell, Kind] = resolveInput(Input)
    % Normalise heterogeneous object inputs into a cell of object arrays.
    % Description: Coerces the shapes the photCalib functions accept for
    %              object inputs into a uniform cell array, where each cell
    %              holds one object array (typically one visit/epoch).
    %              Accepts:
    %                - an object array (PhotCalibTrans / MatchedSources /
    %                  AstroImage / AstroCatalog)            -> {array}
    %                - a cell of such object arrays           -> as-is
    %                - a scalar struct with a .PC field       -> recurse(.PC)
    %                - a scalar struct with a .MS field       -> recurse(.MS)
    %              Supersedes the legacy resolvePC. It does NOT load from
    %              disk (use calibrateVisits / matchEpochs / discoverVisits
    %              for path inputs) and does NOT accept the removed
    %              mode-keyed struct format.
    % Input  : - Input - object array, cell of object arrays, or a
    %            Result-like scalar struct with a .PC or .MS field.
    % Output : - Cell - cell array of object arrays ({} if unresolvable).
    %          - Kind - class name of the resolved elements ('' if unknown).
    % Author : photCalib package refactor (2026-05)
    % Example: [PCc, Kind] = resolveInput(R);          % R.PC -> {...}
    %          [MSc, ~]    = resolveInput(MS_array);

    Cell = {};
    Kind = '';
    KnownClasses = {'PhotCalibTrans', 'MatchedSources', 'AstroImage', 'AstroCatalog'};

    if isstruct(Input)
        if isscalar(Input) && isfield(Input, 'PC')
            [Cell, Kind] = resolveInput(Input.PC);
        elseif isscalar(Input) && isfield(Input, 'MS')
            [Cell, Kind] = resolveInput(Input.MS);
        end
        return;
    end

    if iscell(Input)
        Cell = Input(:).';
    elseif isobject(Input)
        Cell = {Input};
    else
        return;   % numeric / empty / unrecognised
    end

    % Determine Kind from the first non-empty element
    for I = 1:numel(Cell)
        El = Cell{I};
        if ~isempty(El)
            for K = 1:numel(KnownClasses)
                if isa(El, KnownClasses{K})
                    Kind = KnownClasses{K};
                    return;
                end
            end
            return;
        end
    end
end
