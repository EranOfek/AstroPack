%==========================================================================
% Project     : ULTRASAT Observation Planner
% File        : ultrasat.api.ClientBase.m
% Author      : Chen Tishler
% Created     : 01/12/2024
% Updated     : 06/10/2025
% Description : Base class for interacting with REST API services.
%==========================================================================

function result = lvc_filter_with_criteria(alert, criteria, logger)
    % LVC filter with criteria model
    %
    % Parameters:
    %   alert - LvcParsedAlert object
    %   criteria - LvcFilterCriteria object
    %   logger - Logger object
    %
    % Returns:
    %   result - Struct with score, flags, and reasons

    % Log the alert
    logger.info("Filtering alert: %s", alert.alert_id);

    % Initialize score and reasons
    score = 0.0;
    reasons = {};
    flags = struct();

    % Add BNS contribution
    if ~isempty(alert.prob_bns) && alert.prob_bns > criteria.bns_min
        score = score + 2.0 * alert.prob_bns;
        reasons{end+1} = sprintf("BNS %.2f > %.2f", alert.prob_bns, criteria.bns_min);
    end

    % Add NSBH contribution
    if ~isempty(alert.prob_nsbh) && alert.prob_nsbh > criteria.nsbh_min
        score = score + 1.5 * alert.prob_nsbh;
        reasons{end+1} = sprintf("NSBH %.2f > %.2f", alert.prob_nsbh, criteria.nsbh_min);
    end

    % Add terrestrial penalty
    if ~isempty(alert.prob_terrestrial) && alert.prob_terrestrial > criteria.terrestrial_max
        score = score * 0.1;
        flags.rejected_terrestrial = true;
    end

    % Add far penalty
    if ~isempty(alert.far_per_year) && alert.far_per_year > criteria.far_max
        score = score * 0.1;
        flags.rejected_far = true;
    end

    % Initialize result
    result = struct();
    result.score = max(score, 0.0);
    result.flags = flags;
    result.reasons = reasons;   

    % Log the result
    logger.info("Filter result: score=%.2f, reasons=%s", result.score, strjoin(result.reasons, "; "));
end