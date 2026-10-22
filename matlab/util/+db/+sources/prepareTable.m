%% SYNCED from astro-clickhouse — edit SoT, then re-run sync
%==========================================================================
% Project     : Object Matcher Service
% File        : client_matlab/prepareTable.m
% Author      : Chen Tishler
% Created     : 21/10/2026
% Updated     : 21/10/2026
% Description : Shared Parquet table coercion for db.sources clients.
%==========================================================================

function T = prepareTable(srcTable, lowercaseColumns)
% prepareTable  Coerce a detection table for Parquet upload (HTTP or filedrop).
%
%   LAST archive tables from imProc.db.insertCatalog may use uppercase names
%   and uint64 IDs; Parquet and ClickHouse expect lowercase snake_case and
%   safe numeric types. Both SourcesClient and SourcesClientLocal call this.
%
%   T = db.sources.prepareTable(tbl);
%   T = db.sources.prepareTable(tbl, false);

    if nargin < 2
        lowercaseColumns = true;
    end

    T = srcTable;
    if isempty(T) || height(T) == 0
        return;
    end

    % Optional lowercase for ClickHouse-style column names.
    if lowercaseColumns
        T.Properties.VariableNames = lower(T.Properties.VariableNames);
    end

    % Coerce each column to a Parquet-safe MATLAB type.
    VarNames = T.Properties.VariableNames;
    for i = 1:numel(VarNames)
        Col = T.(VarNames{i});
        T.(VarNames{i}) = coerceColumnForParquet_(Col);
    end
end



function Col = coerceColumnForParquet_(Col)
    % coerceColumnForParquet_  Map one column to Parquet-safe storage types.

    if iscell(Col)
        if all(cellfun(@(x) ischar(x) || isstring(x) || (isscalar(x) && isnan(x)), Col))
            Col = string(Col);
        end
        return;
    end

    if isstring(Col) || ischar(Col)
        if ischar(Col)
            Col = string(Col);
        end
        return;
    end

    if isdatetime(Col)
        Col = int64(posixtime(Col));
        return;
    end

    if isduration(Col)
        Col = seconds(Col);
        return;
    end

    if islogical(Col)
        Col = int8(Col);
        return;
    end

    cls = class(Col);
    switch cls
        case 'uint64'
            maxVal = intmax('int64');
            if all(Col <= maxVal, 'all')
                Col = int64(Col);
            else
                Col = double(Col);
            end
        case {'uint32', 'uint16', 'uint8'}
            Col = int64(Col);
        case 'single'
            if any(isinf(Col) | isnan(Col))
                Col = double(Col);
            end
    end
end
