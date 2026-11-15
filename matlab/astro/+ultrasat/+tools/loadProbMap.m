%==========================================================================
% Project     : ULTRASAT SOC
% Filename    : +ultrasat/+tools/loadProbMap.m
% Author      : Chen Tishler
% Created     : 15/11/2026
% Description : Load LVC probability map from .mat (default) or .csv for TOO planner.
%==========================================================================

function probMapTable = loadProbMap(mapPath)
    % Return UNIQ/PROBDENSITY/RA/DEC table from skymap.mat or skymap.csv.
    %
    % mapPath — full path; extension selects reader (.mat preferred for GW share).

    if nargin < 1 || isempty(mapPath)
        error('loadProbMap:MissingPath', 'mapPath is required');
    end

    MapPath = string(mapPath);
    if ~isfile(MapPath)
        error('loadProbMap:NotFound', 'Probability map not found: %s', MapPath);
    end

    [~, ~, Ext] = fileparts(MapPath);
    Ext = lower(Ext);

    switch Ext
        case '.mat'
            S = load(MapPath);
            required = {'UNIQ', 'PROBDENSITY', 'RA', 'DEC'};
            for K = 1:numel(required)
                if ~isfield(S, required{K})
                    error('loadProbMap:BadMat', ...
                        'Missing %s in %s', required{K}, MapPath);
                end
            end
            % scipy.io.savemat: row vectors; UNIQ often int64. Planner uses log(UNIQ).
            probMapTable = table( ...
                double(S.UNIQ(:)), double(S.PROBDENSITY(:)), ...
                double(S.RA(:)), double(S.DEC(:)), ...
                'VariableNames', required);

        case '.csv'
            probMapTable = readtable(MapPath);

        otherwise
            error('loadProbMap:UnknownExt', ...
                'Unsupported probability map extension: %s', Ext);
    end
end