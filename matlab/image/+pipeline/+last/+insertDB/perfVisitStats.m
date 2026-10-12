function S = perfVisitStats(Action, S, varargin)
% perfVisitStats Per-visit timing aggregates for *_perf insert clones.
%   S = perfVisitStats('init')
%   S = perfVisitStats('endVisit', S, TotalSec, InsertSec, Outcome)
%   perfVisitStats('logSummary', S, FnName)
%
% Outcome 'injected' feeds the fullWork cohort; all endVisit calls feed allTimed.

    switch lower(Action)
        case 'init'
            S = struct( ...
                'AllTotal', [], 'AllInsert', [], 'AllNonDb', [], ...
                'FwTotal', [], 'FwInsert', [], 'FwNonDb', []);
        case 'endvisit'
            TotalSec = varargin{1};
            InsertSec = varargin{2};
            Outcome = varargin{3};
            NonDbSec = TotalSec - InsertSec;
            S.AllTotal(end + 1, 1) = TotalSec;
            S.AllInsert(end + 1, 1) = InsertSec;
            S.AllNonDb(end + 1, 1) = NonDbSec;
            if strcmp(Outcome, 'injected')
                S.FwTotal(end + 1, 1) = TotalSec;
                S.FwInsert(end + 1, 1) = InsertSec;
                S.FwNonDb(end + 1, 1) = NonDbSec;
            end
        case 'logsummary'
            FnName = varargin{1};
            logCohort_(FnName, 'fullWork', S.FwTotal, S.FwInsert, S.FwNonDb);
            logCohort_(FnName, 'allTimed', S.AllTotal, S.AllInsert, S.AllNonDb);
        otherwise
            error('perfVisitStats:UnknownAction', 'Unknown action: %s', Action);
    end
end

function logCohort_(FnName, CohortName, TotalV, InsertV, NonDbV)
    N = numel(TotalV);
    if N == 0
        pipeline.last.insertDB.perfLog(sprintf('%s AVG %s n=0 (no visits)', FnName, CohortName));
        return
    end
    MedTotal = localPercentile_(TotalV, 50);
    P90Total = localPercentile_(TotalV, 90);
    Msg = sprintf('%s AVG %s n=%d visitTotal mean=%.1fs med=%.1fs p90=%.1fs visitInsert mean=%.1fs visitNonDb mean=%.1fs', ...
        FnName, CohortName, N, mean(TotalV), MedTotal, P90Total, mean(InsertV), mean(NonDbV));
    pipeline.last.insertDB.perfLog(Msg);
end

function Val = localPercentile_(V, Pct)
    V = sort(V(:));
    if isempty(V)
        Val = NaN;
        return
    end
    Idx = max(1, min(numel(V), ceil(Pct / 100 * numel(V))));
    Val = V(Idx);
end
