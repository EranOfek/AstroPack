function debug_parseIsoDatetime()
% Development and regression test suite for parseIsoDatetime.m
%
%   Run this file (F5) or call it from command window to verify
%   that parseIsoDatetime handles intended ISO 8601 variants correctly.
%
%   Features:
%     - Realistic and edge cases
%     - Pass/fail reporting with simple text
%     - Uses try + assert-like comparison
%     - Shows expected vs actual result on failure
%
%   Author:     Chen Tishler (adapted & extended)
%   Last update: 2026-02-10

    clc;
    disp('============================================================');
    disp('   parseIsoDatetime.m  -  DEBUG / REGRESSION TEST SUITE');
    disp('============================================================');
    disp(' ');

    % Make sure the function exists on path
    if ~exist('parseIsoDatetime', 'file')
        error('Cannot find parseIsoDatetime.m on current path');
    end

    testCases = { ...
        struct('in', '2025-01-01T00:00:00Z', ...
               'exp', datetime(2025,1,1,0,0,0,'TimeZone','UTC')), ...
        struct('in', '2025-01-01T00:00:00.123Z', ...
               'exp', datetime(2025,1,1,0,0,0,123,'TimeZone','UTC')), ...
        struct('in', '2025-01-01T00:00:00+00:00', ...
               'exp', datetime(2025,1,1,0,0,0,'TimeZone','UTC')), ...
        struct('in', '2025-01-01T00:00:00', ...
               'exp', NaT) ...
    };
    
    nTests    = length(testCases);
    nPassed   = 0;
    nFailed   = 0;
    nErrored  = 0;

    fprintf('Running %d test cases ...\n\n', nTests);

    for i = 1:nTests
        tc = testCases{i};

        try
            result = ultrasat.api.utils.DateTimeUtils.parseIsoDateTime(tc.in);

            pass = false;
            if (isnat(result) && isnat(tc.exp))
                pass = true;
            elseif isequal(result, tc.exp) && strcmp(result.TimeZone, 'UTC')
                pass = true;
            end

            if pass
                fprintf('[ OK ]  %3d   %s\n', i, tc.in);
                nPassed = nPassed + 1;
            else
                fprintf('[FAIL]  %3d   %s\n', i, tc.in);
                fprintf('     Expected:  %s\n', char(string(tc.exp)));
                fprintf('     Got:       %s\n', char(string(result)));
                if ~isnat(result) && ~strcmp(result.TimeZone, 'UTC')
                    fprintf('     TimeZone is "%s" (should be "UTC")\n', result.TimeZone);
                end
                nFailed = nFailed + 1;
            end

        catch ME
            fprintf('[ERR ]  %3d   %s\n', i, tc.in);
            fprintf('     %s: %s\n', ME.identifier, ME.message);
            nErrored = nErrored + 1;
        end
    end

    fprintf('\n');
    disp('============================================================');
    disp('  Summary');
    fprintf('  Passed   : %3d\n', nPassed);
    fprintf('  Failed   : %3d\n', nFailed);
    fprintf('  Errored  : %3d\n', nErrored);
    fprintf('  Total    : %3d\n', nTests);
    disp('============================================================');

    if nFailed + nErrored == 0
        disp('ALL TESTS PASSED');
    else
        disp('Some tests FAILED or ERRORED');
    end

    disp(' ');
end