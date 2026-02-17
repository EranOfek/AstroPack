%==========================================================================
% Project     : ULTRASAT SOC Alert Parser
% File        : +alerts/debug/debug_LvcParsedAlert.m
% Author      : Chen Tishler
% Created     : 09/02/2026
% Updated     : 09/02/2026
% Description : Debug script for ultrasat.alerts.models.LvcParsedAlert
%
% Run by: ultrasat.alerts.debug_LvcParsedAlert()
%==========================================================================

function debug_LvcParsedAlert()
    % Debug script for LvcParsedAlert: constructors, serialization, datetimes, file I/O.

    fprintf('--- Debugging LvcParsedAlert ---\n');

    debugDefaultConstructor();
    debugNameValueConstructor();
    debugToJsonString();
    debugFromJsonStringRoundTrip();
    debugFromStruct();
    debugDatetimeHandling();
    debugSaveLoadJson();
    debugFilterIntegration();
end

% -------------------------------------------------------------------------

function debugDefaultConstructor()
    fprintf('\n--- Default constructor ---\n');
    alert = ultrasat.alerts.models.LvcParsedAlert();
    disp(alert);
    fprintf('alert_id="%s", prob_bns=%s\n', alert.alert_id, num2str(alert.prob_bns));
end

% -------------------------------------------------------------------------

function debugNameValueConstructor()
    fprintf('\n--- Name/value constructor ---\n');
    alert = ultrasat.alerts.models.LvcParsedAlert( ...
        "alert_id", "G12345", ...
        "superevent_id", "S12345", ...
        "prob_bns", 0.7, ...
        "far_per_year", 2.5, ...
        "event_time", "2026-02-09T12:00:00");
    fprintf('alert_id="%s", prob_bns=%.2f, far_per_year=%.2f\n', ...
        alert.alert_id, alert.prob_bns, alert.far_per_year);
    disp(alert.event_time);
end

% -------------------------------------------------------------------------

function debugToJsonString()
    fprintf('\n--- toJsonString ---\n');
    alert = ultrasat.alerts.models.LvcParsedAlert( ...
        "alert_id", "G99", "prob_bns", 0.5, "skymap_path", "/path/to/skymap.fits");
    result = alert.toJsonString();
    disp(result);
end

% -------------------------------------------------------------------------

function debugFromJsonStringRoundTrip()
    fprintf('\n--- fromJsonString round-trip ---\n');
    jsonStr = ['{"alert_id":"G88","prob_bns":0.6,"far_per_year":1.2,', ...
        '"event_time":"2026-02-09T14:30:00","instruments":["H1","L1"]}'];
    alert = ultrasat.alerts.models.LvcParsedAlert.fromJsonString(jsonStr);
    fprintf('After fromJsonString: alert_id="%s", prob_bns=%.2f\n', alert.alert_id, alert.prob_bns);
    back = alert.toJsonString();
    fprintf('Back to JSON (excerpt): %s\n', back);
end

% -------------------------------------------------------------------------

function debugFromStruct()
    fprintf('\n--- fromStruct (alert_id, event_time, prob_bns, instruments, raw_fields) ---\n');
    s = struct();
    s.alert_id = "G77";
    s.event_time = "2026-02-09T10:00:00";
    s.prob_bns = 0.8;
    s.instruments = ["H1", "L1", "V1"];
    s.raw_fields = struct("extra_key", "extra_value");
    alert = ultrasat.alerts.models.LvcParsedAlert.fromStruct(s);
    fprintf('alert_id="%s", prob_bns=%.2f\n', alert.alert_id, alert.prob_bns);
    fprintf('event_time: %s\n', string(alert.event_time));
    fprintf('instruments: %s\n', strjoin(alert.instruments, ", "));
    fprintf('raw_fields.extra_key="%s"\n', alert.raw_fields.extra_key);
end

% -------------------------------------------------------------------------

function debugDatetimeHandling()
    fprintf('\n--- Datetime handling (time_created, event_time as ISO strings) ---\n');
    s = struct();
    s.time_created = "2026-02-09T08:00:00";
    s.event_time = "2026-02-09T09:15:30";
    s.parsed_time = "2026-02-09T09:16:00";
    alert = ultrasat.alerts.models.LvcParsedAlert.fromStruct(s);
    fprintf('time_created: %s\n', string(alert.time_created));
    fprintf('event_time: %s\n', string(alert.event_time));
    fprintf('parsed_time: %s\n', string(alert.parsed_time));
    assert(~isnat(alert.event_time), 'event_time should be parsed as datetime');
end

% -------------------------------------------------------------------------

function debugSaveLoadJson()
    fprintf('\n--- saveToJsonFile / loadFromJsonFile ---\n');
    alert = ultrasat.alerts.models.LvcParsedAlert( ...
        "alert_id", "G66", "prob_bns", 0.55, "far_per_year", 3.0);
    filePath = [tempname(), '.json'];
    alert.saveToJsonFile(filePath);
    loaded = ultrasat.alerts.models.LvcParsedAlert.loadFromJsonFile(filePath);
    delete(filePath);
    assert(loaded.alert_id == alert.alert_id && loaded.prob_bns == alert.prob_bns, ...
        'Save/load round-trip failed');
    fprintf('Save/load round-trip OK: alert_id="%s", prob_bns=%.2f\n', loaded.alert_id, loaded.prob_bns);
end

% -------------------------------------------------------------------------

function debugFilterIntegration()
    fprintf('\n--- Integration: LvcParsedAlert + LvcFilterCriteria + lvc_filter_with_criteria ---\n');
    alert = ultrasat.alerts.models.LvcParsedAlert( ...
        "alert_id", "G55", "prob_bns", 0.6, "prob_nsbh", 0.2, "far_per_year", 5.0);
    criteria = ultrasat.alerts.models.LvcFilterCriteria("bns_min", 0.3, "far_max", 10);
    result = ultrasat.alerts.filters.lvc_filter_with_criteria(alert, criteria, MsgLogger.getSingleton());
    %fprintf('Filter result: score=%.2f, reasons=%s\n', result.score, strjoin(result.reasons, "; "));
end
