%==========================================================================
% Project     : ULTRASAT Observation Planner
% File        : ultrasat.api.ClientBase.m
% Author      : Chen Tishler
% Created     : 01/12/2024
% Updated     : 06/10/2025
% Description : Base class for interacting with REST API services.
%==========================================================================

function result = lvc_filter_simple(alert, logger)
    % Basic LVC filter without criteria
    %
    % Parameters:
    %   alert - LvcParsedAlert object
    %   logger - Logger object
    %
    % Returns:
    %   result - Struct with score, class_probs, flags, and reasons

    % Log the alert
    logger.info("Filtering alert: %s", alert.alert_id);

    % Initialize score and reasons
    score = 0.0;
    reasons = {};

    % Add BNS contribution
    if ~isempty(alert.prob_bns)
        score = score + 2.0 * alert.prob_bns;
        reasons{end+1} = sprintf("BNS contribution: %.3f", alert.prob_bns);
    end

    % Add NSBH contribution
    if ~isempty(alert.prob_nsbh)
        score = score + 1.5 * alert.prob_nsbh;
        reasons{end+1} = sprintf("NSBH contribution: %.3f", alert.prob_nsbh);
    end

    % Add BBH contribution
    if ~isempty(alert.prob_bbh)
        score = score + 0.2 * alert.prob_bbh;
        reasons{end+1} = sprintf("BBH contribution: %.3f", alert.prob_bbh);
    end

    % Add terrestrial penalty
    if ~isempty(alert.prob_terrestrial)
        score = score - 2.0 * alert.prob_terrestrial;
        reasons{end+1} = sprintf("Terrestrial penalty: %.3f", alert.prob_terrestrial);
    end

    % Initialize flags
    flags = struct();
    flags.has_skymap = ~isempty(alert.skymap_path);
    flags.low_far = ~isempty(alert.far_per_year) && alert.far_per_year < 10;

    % Initialize result
    result = struct();
    result.score = max(score, 0.0);
    result.class_probs = struct( ...
        "bns", alert.prob_bns, ...
        "nsbh", alert.prob_nsbh, ...
        "bbh", alert.prob_bbh, ...
        "terrestrial", alert.prob_terrestrial ...
    );
    result.flags = flags;
    result.reasons = reasons;

    % Log the result
    msg = strjoin(string(result.reasons), "; ");
    logger.info("Filter result: score=%.2f, reasons=%s", result.score, msg);

end
