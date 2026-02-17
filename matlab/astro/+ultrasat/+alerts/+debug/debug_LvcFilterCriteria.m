%==========================================================================
% Project     : ULTRASAT SOC Alert Parser
% File        : +alerts/debug/debug_LvcFilterCriteria.m
% Author      : Chen Tishler
% Created     : 09/02/2026
% Updated     : 09/02/2026
% Description : Debug script for ultrasat.alerts.models.LvcFilterCriteria
%
% Run by: ultrasat.alerts.debug_LvcFilterCriteria()
%==========================================================================

function debug_LvcFilterCriteria()
    % Debug script for LvcFilterCriteria: constructors, serialization, file I/O.

    fprintf('--- Debugging LvcFilterCriteria ---\n');

    debugDefaultConstructor();
    debugNameValueConstructor();
    debugToJsonString();
    debugFromJsonStringRoundTrip();
    debugFromStruct();
    debugSaveLoadJsonFile();
    debugErrorCases();
end

% -------------------------------------------------------------------------

function debugDefaultConstructor()
    fprintf('\n--- Default constructor ---\n');
    crit = ultrasat.alerts.models.LvcFilterCriteria();
    disp(crit);
    fprintf('bns_min=%.2f, far_max=%.2f\n', crit.bns_min, crit.far_max);
end

% -------------------------------------------------------------------------

function debugNameValueConstructor()
    fprintf('\n--- Name/value constructor ---\n');
    crit = ultrasat.alerts.models.LvcFilterCriteria("bns_min", 0.5, "far_max", 10);
    fprintf('bns_min=%.2f, far_max=%.2f\n', crit.bns_min, crit.far_max);
    disp(crit);
end

% -------------------------------------------------------------------------

function debugToJsonString()
    fprintf('\n--- toJsonString ---\n');
    crit = ultrasat.alerts.models.LvcFilterCriteria("bns_min", 0.3, "terrestrial_max", 0.1);
    result = crit.toJsonString();
    disp(result);
end

% -------------------------------------------------------------------------

function debugFromJsonStringRoundTrip()
    fprintf('\n--- fromJsonString round-trip ---\n');
    jsonStr = '{"bns_min":0.4,"far_max":5.0,"handle_bursts":false}';
    crit = ultrasat.alerts.models.LvcFilterCriteria.fromJsonString(jsonStr);
    fprintf('After fromJsonString: bns_min=%.2f, far_max=%.2f\n', crit.bns_min, crit.far_max);
    back = crit.toJsonString();
    fprintf('Back to JSON (excerpt): %s\n', back);
end

% -------------------------------------------------------------------------

function debugFromStruct()
    fprintf('\n--- fromStruct (subset of fields) ---\n');
    s = struct("bns_min", 0.6, "far_max", 20.0);
    crit = ultrasat.alerts.models.LvcFilterCriteria.fromStruct(s);
    fprintf('bns_min=%.2f (set), far_max=%.2f (set), bbh_min=%.2f (default)\n', ...
        crit.bns_min, crit.far_max, crit.bbh_min);
end

% -------------------------------------------------------------------------

function debugSaveLoadJsonFile()
    fprintf('\n--- saveToJsonFile / loadFromJsonFile ---\n');
    crit = ultrasat.alerts.models.LvcFilterCriteria("bns_min", 0.7, "nsbh_min", 0.2);
    filePath = [tempname(), '.json'];
    crit.saveToJsonFile(filePath);
    loaded = ultrasat.alerts.models.LvcFilterCriteria.loadFromJsonFile(filePath);
    delete(filePath);
    assert(crit.bns_min == loaded.bns_min && crit.nsbh_min == loaded.nsbh_min, ...
        'Save/load round-trip failed');
    fprintf('Save/load round-trip OK: bns_min=%.2f, nsbh_min=%.2f\n', loaded.bns_min, loaded.nsbh_min);
end

% -------------------------------------------------------------------------

function debugErrorCases()
    fprintf('\n--- Error cases ---\n');

    % Odd number of constructor args
    try
        ultrasat.alerts.models.LvcFilterCriteria("bns_min", 0.5, "far_max");
        fprintf('ERROR: expected error for odd args\n');
    catch e
        fprintf('Expected error (odd args): %s\n', e.message);
    end

    % Unknown property
    try
        ultrasat.alerts.models.LvcFilterCriteria("bns_min", 0.5, "unknown_prop", 1);
        fprintf('ERROR: expected error for unknown property\n');
    catch e
        fprintf('Expected error (unknown property): %s\n', e.message);
    end
end
