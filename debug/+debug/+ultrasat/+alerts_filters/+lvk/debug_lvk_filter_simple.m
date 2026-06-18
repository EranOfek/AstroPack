%==========================================================================
% Project     : ULTRASAT Incoming Alerts Filter
% Filename    : +debug/+ultrasat/+alerts_filters/+lvk/debug_lvk_filter_simple.m
% Author      : Chen Tishler
% Created     : 09/02/2026
% Updated     : 24/05/2026
% Description : Debug script for ultrasat.alerts.filters.lvk_filter_simple
%
% Run by      : debug.ultrasat.alerts_filters.lvk.debug_lvk_filter_simple()
%==========================================================================

function debug_lvk_filter_simple()
    % Debug script for lvk_filter_simple: mock logger and several scenarios.

    fprintf('--- Debugging lvk_filter_simple ---\n');
    logger = MsgLogger.getSingleton();

    debugScenarioFullProbs(logger);
    debugScenarioSkymapLowFAR(logger);
    debugScenarioNoSkymapHighFAR(logger);
    debugScenarioMinimal(logger);
end

% -------------------------------------------------------------------------

function debugScenarioFullProbs(logger)
    fprintf('\n--- Scenario 1: Full probs ---\n');
    alert = ultrasat.alerts_filters.lvk.models.LvkParsedAlert( ...
        "alert_id", "G-FULL", ...
        "prob_bns", 0.5, "prob_nsbh", 0.3, "prob_bbh", 0.1, "prob_terrestrial", 0.1);
    result = ultrasat.alerts_filters.lvk.filters.lvk_filter_simple(alert, logger);
    fprintf('score=%.2f\n', result.score);
    disp(result.class_probs);
    msg = strjoin([result.reasons{:}], "; ");
    fprintf('reasons: %s\n', msg);
    disp(result.flags);
end

% -------------------------------------------------------------------------

function debugScenarioSkymapLowFAR(logger)
    fprintf('\n--- Scenario 2: Skymap and low FAR ---\n');
    alert = ultrasat.alerts_filters.lvk.models.LvkParsedAlert( ...
        "alert_id", "G-SKY", "skymap_path", "/path/to/skymap.fits", "far_per_year", 5.0);
    result = ultrasat.alerts_filters.lvk.filters.lvk_filter_simple(alert, logger);
    fprintf('score=%.2f\n', result.score);
    fprintf('flags.has_skymap=%d, flags.low_far=%d\n', ...
        result.flags.has_skymap, result.flags.low_far);
end

% -------------------------------------------------------------------------

function debugScenarioNoSkymapHighFAR(logger)
    fprintf('\n--- Scenario 3: No skymap, high FAR ---\n');
    alert = ultrasat.alerts_filters.lvk.models.LvkParsedAlert( ...
        "alert_id", "G-HIFAR", "skymap_path", "", "far_per_year", 50.0);
    result = ultrasat.alerts_filters.lvk.filters.lvk_filter_simple(alert, logger);
    fprintf('score=%.2f\n', result.score);
    fprintf('flags.has_skymap=%d, flags.low_far=%d\n', ...
        result.flags.has_skymap, result.flags.low_far);
end

% -------------------------------------------------------------------------

function debugScenarioMinimal(logger)
    fprintf('\n--- Scenario 4: Minimal alert ---\n');
    alert = ultrasat.alerts_filters.lvk.models.LvkParsedAlert("alert_id", "G-MIN");
    result = ultrasat.alerts_filters.lvk.filters.lvk_filter_simple(alert, logger);
    fprintf('score=%.2f\n', result.score);
    disp(result.class_probs);
    msg = strjoin(string(result.reasons), "; ");
    fprintf('reasons: %s\n', msg);
    disp(result.flags);
end
