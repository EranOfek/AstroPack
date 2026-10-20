function procMatBuildSidecarLogs(Action, LogCtx, varargin)
% procMatBuildSidecarLogs Incremental sorted sidecar logs for writeArchiveProcCatMat.
%   procMatBuildSidecarLogs('reset', LogCtx)
%   procMatBuildSidecarLogs('recordVisit', LogCtx, DataDir, Nrows)
%   procMatBuildSidecarLogs('mergeRowsMap', LogCtx, RowByPathMap)
%   procMatBuildSidecarLogs('flush', LogCtx)

    persistent RowByPath
    if nargin < 1
        error('procMatBuildSidecarLogs:Action', 'Action is required');
    end
    switch lower(Action)
        case 'reset'
            RowByPath = containers.Map('KeyType', 'char', 'ValueType', 'double');
        case 'recordvisit'
            if isempty(RowByPath)
                RowByPath = containers.Map('KeyType', 'char', 'ValueType', 'double');
            end
            DataDir = varargin{1};
            Nrows = varargin{2};
            if nargin < 4 || isempty(Nrows)
                Nrows = -1;
            end
            RowByPath(char(DataDir)) = Nrows;
        case 'mergerowsmap'
            if isempty(RowByPath)
                RowByPath = containers.Map('KeyType', 'char', 'ValueType', 'double');
            end
            RowMap = varargin{1};
            if ~isa(RowMap, 'containers.Map') || RowMap.Count == 0
                return
            end
            Keys = RowMap.keys;
            for I = 1:numel(Keys)
                Path = Keys{I};
                Nrows = RowMap(Path);
                if ~isKey(RowByPath, Path)
                    RowByPath(Path) = Nrows;
                elseif Nrows >= 0 && (RowByPath(Path) < 0 || RowByPath(Path) ~= Nrows)
                    RowByPath(Path) = Nrows;
                end
            end
        case 'flush'
            if isempty(RowByPath)
                RowByPath = containers.Map('KeyType', 'char', 'ValueType', 'double');
            end
            procMatBuildSidecarFlush_(LogCtx, RowByPath);
        otherwise
            error('procMatBuildSidecarLogs:Action', ...
                'Action must be reset, recordVisit, mergeRowsMap, or flush');
    end
end

function procMatBuildSidecarFlush_(LogCtx, RowByPath)
    VisitsPath = LogCtx.VisitsPath;
    VisitRowsPath = LogCtx.VisitRowsPath;
    if RowByPath.Count == 0
        procMatBuildSidecarWriteFile_(VisitsPath, {});
        procMatBuildSidecarWriteFile_(VisitRowsPath, {});
        return
    end
    Sorted = sort(RowByPath.keys);
    VisitLines = cell(numel(Sorted), 1);
    RowLines = cell(numel(Sorted), 1);
    for I = 1:numel(Sorted)
        Path = Sorted{I};
        VisitLines{I} = Path;
        RowLines{I} = sprintf('%s\t%d', Path, RowByPath(Path));
    end
    procMatBuildSidecarWriteFile_(VisitsPath, VisitLines);
    procMatBuildSidecarWriteFile_(VisitRowsPath, RowLines);
end

function procMatBuildSidecarWriteFile_(Path, Lines)
    BakPath = [Path '.bak'];
    if isfile(Path)
        if isfile(BakPath)
            delete(BakPath);
        end
        movefile(Path, BakPath);
    end
    Fid = fopen(Path, 'w');
    if Fid < 0
        return
    end
    Cleaner = onCleanup(@() fclose(Fid));
    for I = 1:numel(Lines)
        fprintf(Fid, '%s\n', Lines{I});
    end
end
