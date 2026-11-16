%==========================================================================
% Project     : ULTRASAT SOC
% Filename    : +ultrasat/+services/+too_planner/+debug/debug_resolveTooStartTime.m
% Created     : 15/11/2026
% Updated     : 15/11/2026
% Author      : Chen Tishler
% Description : Smoke tests for parseUtcDatetime and resolveTooStartTime.
%==========================================================================

function debug_resolveTooStartTime()
    % debug_resolveTooStartTime Assertion-based checks for TOO start-time helpers.

    fprintf('\n========== debug_resolveTooStartTime ==========\n');

    dt1 = ultrasat.tools.parseUtcDatetime('2025-01-18T05:58:32Z');
    assert(~isnat(dt1) && strcmp(dt1.TimeZone, 'UTC'), 'parse Z suffix');

    dt2 = ultrasat.tools.parseUtcDatetime('2025-01-18T05:58:02.474Z');
    assert(~isnat(dt2), 'parse fractional seconds');

    dt3 = ultrasat.tools.parseUtcDatetime('2025-01-18 05:58:32');
    assert(~isnat(dt3), 'parse space separator');

    fixturePath = writeFixtureAlert();
    cleaner = onCleanup(@() deleteFixture(fixturePath)); %#ok<NASGU>

    [startRel, infoRel] = ultrasat.planner.resolveTooStartTime("+00:03:00", fixturePath);
    assert(strcmp(infoRel.mode, 'relative'), 'relative mode');
    expected = datetime('2025-01-18T06:01:32', 'TimeZone', 'UTC');
    assert(abs(seconds(startRel - expected)) < 1, 'relative +3min offset');

    [startAbs, infoAbs] = ultrasat.planner.resolveTooStartTime("2025-06-01T12:00:00Z", "");
    assert(strcmp(infoAbs.mode, 'absolute'), 'absolute mode');
    assert(~isnat(startAbs), 'absolute parse');

    planCfg = struct('TOOStartTime', '2025-01-18T06:01:32Z');
    [startPlan, ~] = ultrasat.planner.resolveTooStartTime(planCfg.TOOStartTime, "");
    assert(~isnat(startPlan), 'plan TOOStartTime absolute');

    [~, infoNow] = ultrasat.planner.resolveTooStartTime("now", "");
    assert(strcmp(infoNow.mode, 'now'), 'now mode');

    try
        ultrasat.planner.resolveTooStartTime("+00:01:00", "");
        error('Expected AlertTimeUnavailable for missing alert_file');
    catch Ex
        assert(strcmp(Ex.identifier, 'ultrasat:planner:AlertTimeUnavailable'), Ex.identifier);
    end

    fprintf('All debug_resolveTooStartTime checks passed.\n');
    fprintf('========== debug_resolveTooStartTime DONE ==========\n\n');
end


function pathOut = writeFixtureAlert()
    % writeFixtureAlert Write minimal LvkParsedAlert JSON to a temp file.

    payload = struct( ...
        'alert_id', 'S_TEST_01', ...
        'superevent_id', 'S_TEST', ...
        'alert_type', 'PRELIMINARY', ...
        'time_created', '2025-01-18T05:58:32', ...
        'event_time', '2025-01-18T05:58:02', ...
        'prob_bns', 0.5);
    pathOut = [tempname() '.json'];
    fid = fopen(pathOut, 'w');
    fwrite(fid, jsonencode(payload));
    fclose(fid);
end


function deleteFixture(pathOut)
    % deleteFixture Remove temp fixture when it still exists.
    if isfile(pathOut)
        delete(pathOut);
    end
end