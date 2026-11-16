function tests = test_auditContamColumn
    % Unit tests for the Gaia contamination rule of PhotCalibTrans.auditCalibCandidates
    % when the candidates carry the FCONT column of GAIADR3spec (issue #1454).
    %
    % Synthetic candidate tables only: no catalog or image is read. The LAST
    % nearest-neighbor rule is inactive (FieldTab lacks its columns).

    tests = functiontests(localfunctions);
end

%% Test Functions

function testColumnRejectsAboveMax(testCase)
    % Candidates with FCONT above AuditContamMax are rejected with reason GaiaContam.

    Cand = candTable([0 0.01 0.02 0.03 0.5]);
    [Doubtful, Reason, Fcont] = PhotCalibTrans.auditCalibCandidates(Cand, table(), 'AuditContamMax', 0.02);

    testCase.verifyEqual(Doubtful(:).', [false false false true true]);
    testCase.verifyEqual(string(Reason(4:5)).', ["GaiaContam" "GaiaContam"]);
    testCase.verifyEqual(Fcont, Cand.FCONT);
end

function testNaNMaxDisablesRule(testCase)
    % AuditContamMax NaN disables the rule even when the column is present.

    Cand = candTable([0 0.5 10]);
    [Doubtful, ~, Fcont] = PhotCalibTrans.auditCalibCandidates(Cand, table(), 'AuditContamMax', NaN);

    testCase.verifyFalse(any(Doubtful));
    testCase.verifyTrue(all(isnan(Fcont)));
end

function testInfMaxRecordsOnly(testCase)
    % AuditContamMax Inf returns FCONT without rejecting.

    Cand = candTable([0 0.5 10]);
    [Doubtful, ~, Fcont] = PhotCalibTrans.auditCalibCandidates(Cand, table(), 'AuditContamMax', Inf);

    testCase.verifyFalse(any(Doubtful));
    testCase.verifyEqual(Fcont, Cand.FCONT);
end

function testColumnPreferredOverCone(testCase)
    % With the column present, a Gaia cone given as well is not used.

    Cand = candTable([0 0.03]);
    % a bright neighbor 3" from the first candidate: the cone would reject it
    Cone = table(Cand.RA(1) + 3/3600, Cand.Dec(1), 5, 'VariableNames', {'RA','Dec','phot_g_mean_mag'});
    [Doubtful, ~, Fcont] = PhotCalibTrans.auditCalibCandidates(Cand, table(), 'AuditContamMax', 0.02, ...
        'AuditContamCat', Cone, 'AuditContamFWHM', 5, 'AuditContamPixScale', 1.25);

    testCase.verifyEqual(Doubtful(:).', [false true]);
    testCase.verifyEqual(Fcont, Cand.FCONT);
end

function testNaNColumnValueIsKept(testCase)
    % A candidate without a catalog FCONT value (NaN) is not rejected by the rule.

    Cand = candTable([NaN 0.5]);
    Doubtful = PhotCalibTrans.auditCalibCandidates(Cand, table(), 'AuditContamMax', 0.02);

    testCase.verifyEqual(Doubtful(:).', [false true]);
end

%% Helpers

function T = candTable(Fcont)
    % Candidates that pass the other Gaia rules, with the given FCONT values.
    N = numel(Fcont);
    T = table(150 + (1:N).'*0.01, 20 + zeros(N,1), 100*(1:N).', 100*ones(N,1), 13*ones(N,1), ...
              0.8*ones(N,1), 1.1*ones(N,1), ones(N,1), Fcont(:), ...
              'VariableNames', {'RA','Dec','X','Y','phot_g_mean_mag', ...
                                'bp_rp','phot_bp_rp_excess_factor','classprob_dsc_combmod_star','FCONT'});
end
