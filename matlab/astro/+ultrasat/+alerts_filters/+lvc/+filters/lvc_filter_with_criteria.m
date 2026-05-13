%==========================================================================
% Project     : ULTRASAT Incoming Alerts Filter
% File        : +ultrasat/+alerts_filters/+lvc/+filters/lvc_filter_with_criteria.m
% Author      : Chen Tishler
% Created     : 01/12/2024
% Updated     : 12/05/2026
% Description : LVC filter with criteria implementation.
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
    %   result - LvcFilterResult

    % Log the alert
    logger.info("Filtering alert: %s", alert.alert_id);

    % Initialize result
    result = ultrasat.alerts_filters.lvc.models.LvcFilterResult();
    result.class_probs = struct( ...
        "bns", alert.prob_bns, ...
        "nsbh", alert.prob_nsbh, ...
        "bbh", alert.prob_bbh, ...
        "terrestrial", alert.prob_terrestrial ...
    );
    result.flags = struct();
    result.flags.rejected_terrestrial = false;
    result.flags.rejected_far = false;

    % Add BNS contribution
    if hasFiniteScalar(alert.prob_bns) && alert.prob_bns > criteria.bns_min
        result.score = result.score + 2.0 * alert.prob_bns;
        result.reasons{end+1} = sprintf("BNS %.2f > %.2f", alert.prob_bns, criteria.bns_min);
    end

    % Add NSBH contribution
    if hasFiniteScalar(alert.prob_nsbh) && alert.prob_nsbh > criteria.nsbh_min
        result.score = result.score + 1.5 * alert.prob_nsbh;
        result.reasons{end+1} = sprintf("NSBH %.2f > %.2f", alert.prob_nsbh, criteria.nsbh_min);
    end

    % Add terrestrial penalty
    if hasFiniteScalar(alert.prob_terrestrial) && alert.prob_terrestrial > criteria.terrestrial_max
        result.score = result.score * 0.1;
        result.flags.rejected_terrestrial = true;
        result.reasons{end+1} = sprintf("Terrestrial %.2f > %.2f", ...
            alert.prob_terrestrial, criteria.terrestrial_max);
    end

    % Add far penalty
    if hasFiniteScalar(alert.far_per_year) && alert.far_per_year > criteria.far_max
        result.score = result.score * 0.1;
        result.flags.rejected_far = true;
        result.reasons{end+1} = sprintf("FAR %.2f > %.2f", ...
            alert.far_per_year, criteria.far_max);
    end

    result.score = max(result.score, 0.0);

    % Log the result
    msg = strjoin(string(result.reasons), "; ");
    logger.info("Filter result: score=%.2f, reasons=%s", result.score, msg);
end


function result = hasFiniteScalar(value)
    result = isnumeric(value) && isscalar(value) && isfinite(value);
end