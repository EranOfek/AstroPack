%==========================================================================
% Project     : ULTRASAT SOC Alert Parser
% File        : +alerts/debug/debug_lvc_filter_with_criteria.m
% Author      : Chen Tishler
% Created     : 09/02/2026
% Updated     : 09/02/2026
% Description : Debug script for ultrasat.alerts.filters.lvc_filter_with_criteria
%
% Run by: ultrasat.alerts.debug_lvc_filter_with_criteria()
%==========================================================================

function debug_lvc_filter_with_criteria()
    % Debug script for lvc_filter_with_criteria: mock logger and several scenarios.

    fprintf('--- Debugging lvc_filter_with_criteria ---\n');

    logger = MsgLogger.getSingleton();

    debugScenarioBNS(logger);
    debugScenarioTerrestrial(logger);
    debugScenarioFAR(logger);
    debugScenarioMinimal(logger);
end

% -------------------------------------------------------------------------

function debugScenarioBNS(logger)
    fprintf('\n--- Scenario 1: BNS/NSBH above thresholds ---\n');
    alert = ultrasat.alerts.models.LvcParsedAlert( ...
        "alert_id", "G-BNS", "prob_bns", 0.6, "prob_nsbh", 0.3, "far_per_year", 5.0);
    criteria = ultrasat.alerts.models.LvcFilterCriteria( ...
        "bns_min", 0.3, "nsbh_min", 0.2, "far_max", 10);
    result = ultrasat.alerts.filters.lvc_filter_with_criteria(alert, criteria, logger);
    fprintf('score=%.2f\n', result.score);
    msg = strjoin(string(result.reasons), "; ");
    fprintf('reasons: %s\n', msg);
    disp(result.flags);
end

% -------------------------------------------------------------------------

function debugScenarioTerrestrial(logger)
    fprintf('\n--- Scenario 2: Terrestrial rejection ---\n');
    alert = ultrasat.alerts.models.LvcParsedAlert( ...
        "alert_id", "G-TERR", "prob_bns", 0.4, "prob_terrestrial", 0.9);
    criteria = ultrasat.alerts.models.LvcFilterCriteria( ...
        "bns_min", 0.2, "terrestrial_max", 0.5);
    result = ultrasat.alerts.filters.lvc_filter_with_criteria(alert, criteria, logger);
    fprintf('score=%.2f\n', result.score);
    fprintf('flags.rejected_terrestrial=%d\n', result.flags.rejected_terrestrial);
    msg = strjoin(string(result.reasons), "; ");
    fprintf('reasons: %s\n', msg);
end

% -------------------------------------------------------------------------

function debugScenarioFAR(logger)
    fprintf('\n--- Scenario 3: FAR rejection ---\n');
    alert = ultrasat.alerts.models.LvcParsedAlert( ...
        "alert_id", "G-FAR", "prob_bns", 0.5, "far_per_year", 100.0);
    criteria = ultrasat.alerts.models.LvcFilterCriteria( ...
        "bns_min", 0.2, "far_max", 10);
    result = ultrasat.alerts.filters.lvc_filter_with_criteria(alert, criteria, logger);
    fprintf('score=%.2f\n', result.score);
    fprintf('flags.rejected_far=%d\n', result.flags.rejected_far);
    msg = strjoin(string(result.reasons), "; ");
    fprintf('reasons: %s\n', msg);
end

% -------------------------------------------------------------------------

function debugScenarioMinimal(logger)
    fprintf('\n--- Scenario 4: Empty / minimal alert ---\n');
    alert = ultrasat.alerts.models.LvcParsedAlert("alert_id", "G-MIN");
    criteria = ultrasat.alerts.models.LvcFilterCriteria();
    result = ultrasat.alerts.filters.lvc_filter_with_criteria(alert, criteria, logger);
    fprintf('score=%.2f\n', result.score);
    msg = strjoin(string(result.reasons), "; ");
    fprintf('reasons: %s\n', msg);
    disp(result.flags);
end
