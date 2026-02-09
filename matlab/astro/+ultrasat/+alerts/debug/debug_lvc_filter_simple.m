%==========================================================================
% Project     : ULTRASAT SOC Alert Parser
% File        : +alerts/debug/debug_lvc_filter_simple.m
% Author      : Chen Tishler
% Created     : 09/02/2026
% Updated     : 09/02/2026
% Description : Debug script for ultrasat.alerts.filters.lvc_filter_simple
%
% Run by: ultrasat.alerts.debug_lvc_filter_simple()
%==========================================================================

function debug_lvc_filter_simple()
    % Debug script for lvc_filter_simple: mock logger and several scenarios.

    fprintf('--- Debugging lvc_filter_simple ---\n');

    logger = makeMockLogger();

    debugScenarioFullProbs(logger);
    debugScenarioSkymapLowFAR(logger);
    debugScenarioNoSkymapHighFAR(logger);
    debugScenarioMinimal(logger);
end

% -------------------------------------------------------------------------

function logger = makeMockLogger()
    logger = struct();
    logger.info = @(varargin) fprintf([varargin{1} '\n'], varargin{2:end});
end

% -------------------------------------------------------------------------

function debugScenarioFullProbs(logger)
    fprintf('\n--- Scenario 1: Full probs ---\n');
    alert = ultrasat.alerts.models.LvcParsedAlert( ...
        "alert_id", "G-FULL", ...
        "prob_bns", 0.5, "prob_nsbh", 0.3, "prob_bbh", 0.1, "prob_terrestrial", 0.1);
    result = ultrasat.alerts.filters.lvc_filter_simple(alert, logger);
    fprintf('score=%.2f\n', result.score);
    disp(result.class_probs);
    fprintf('reasons: %s\n', strjoin(result.reasons, "; "));
    disp(result.flags);
end

% -------------------------------------------------------------------------

function debugScenarioSkymapLowFAR(logger)
    fprintf('\n--- Scenario 2: Skymap and low FAR ---\n');
    alert = ultrasat.alerts.models.LvcParsedAlert( ...
        "alert_id", "G-SKY", "skymap_path", "/path/to/skymap.fits", "far_per_year", 5.0);
    result = ultrasat.alerts.filters.lvc_filter_simple(alert, logger);
    fprintf('score=%.2f\n', result.score);
    fprintf('flags.has_skymap=%d, flags.low_far=%d\n', ...
        result.flags.has_skymap, result.flags.low_far);
end

% -------------------------------------------------------------------------

function debugScenarioNoSkymapHighFAR(logger)
    fprintf('\n--- Scenario 3: No skymap, high FAR ---\n');
    alert = ultrasat.alerts.models.LvcParsedAlert( ...
        "alert_id", "G-HIFAR", "skymap_path", "", "far_per_year", 50.0);
    result = ultrasat.alerts.filters.lvc_filter_simple(alert, logger);
    fprintf('score=%.2f\n', result.score);
    fprintf('flags.has_skymap=%d, flags.low_far=%d\n', ...
        result.flags.has_skymap, result.flags.low_far);
end

% -------------------------------------------------------------------------

function debugScenarioMinimal(logger)
    fprintf('\n--- Scenario 4: Minimal alert ---\n');
    alert = ultrasat.alerts.models.LvcParsedAlert("alert_id", "G-MIN");
    result = ultrasat.alerts.filters.lvc_filter_simple(alert, logger);
    fprintf('score=%.2f\n', result.score);
    disp(result.class_probs);
    fprintf('reasons: %s\n', strjoin(result.reasons, "; "));
    disp(result.flags);
end
