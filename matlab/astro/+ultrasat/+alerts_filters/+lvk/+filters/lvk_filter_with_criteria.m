%==========================================================================
% Project     : ULTRASAT Incoming Alerts Filter
% File        : +ultrasat/+alerts_filters/+lvk/+filters/lvk_filter_with_criteria.m
% Author      : Chen Tishler
% Created     : 01/12/2024
% Updated     : 12/05/2026
% Description : LVK filter with criteria implementation.
%==========================================================================

function result = lvk_filter_with_criteria(alert, criteria, logger)
    % Same hydrated alert fields as simple filter; gates on LvkFilterCriteria thresholds.
    % criteria may come from IPC struct, criteria_file JSON, or defaults.

    % Log the alert
    logger.info("Filtering alert: %s", alert.alert_id);

    % Initialize result
    result = ultrasat.alerts_filters.lvk.models.LvkFilterResult();
    result.class_probs = struct( ...
        "bns", alert.prob_bns, ...
        "nsbh", alert.prob_nsbh, ...
        "bbh", alert.prob_bbh, ...
        "terrestrial", alert.prob_terrestrial ...
    );
    result.flags = struct();
    result.flags.has_skymap = strlength(string(alert.skymap_path)) > 0;
    result.flags.rejected_terrestrial = false;
    result.flags.rejected_far = false;

    % Accept when prob_bns above criteria.bns_min; same pattern for NSBH.
    if hasFiniteScalar(alert.prob_bns) && alert.prob_bns > criteria.bns_min
        result.score = result.score + 2.0 * alert.prob_bns;
        result.reasons{end+1} = sprintf("BNS %.2f > %.2f", alert.prob_bns, criteria.bns_min);
    end

    % Add NSBH contribution
    if hasFiniteScalar(alert.prob_nsbh) && alert.prob_nsbh > criteria.nsbh_min
        result.score = result.score + 1.5 * alert.prob_nsbh;
        result.reasons{end+1} = sprintf("NSBH %.2f > %.2f", alert.prob_nsbh, criteria.nsbh_min);
    end

    % Reject-style penalties vs criteria.terrestrial_max and criteria.far_max.
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

    % Final TOO decision using criteria.score_min and reject flags.
    result = applyTooDecisionWithCriteria(result, criteria);

    % Log the result
    msg = strjoin(string(result.reasons), "; ");
    logger.info("Filter result: score=%.2f, is_too=%d, reasons=%s", ...
        result.score, result.is_too, msg);
end


function result = applyTooDecisionWithCriteria(result, criteria)
    % Set is_too from criteria.score_min, skymap, and rejection flags.

    reasons = {};

    if result.score < criteria.score_min
        reasons{end+1} = sprintf("REJECT: score %.3f < %.3f", result.score, criteria.score_min); %#ok<AGROW>
    end
    if result.flags.rejected_terrestrial
        reasons{end+1} = "REJECT: terrestrial above criteria.terrestrial_max"; %#ok<AGROW>
    end
    if result.flags.rejected_far
        reasons{end+1} = "REJECT: FAR above criteria.far_max"; %#ok<AGROW>
    end
    if ~result.flags.has_skymap
        reasons{end+1} = "REJECT: skymap_path empty"; %#ok<AGROW>
    end

    result.decision_reasons = reasons;
    if isempty(reasons)
        result.is_too = true;
        result.decision = "accept";
        result.decision_reasons = {sprintf( ...
            "ACCEPT: score %.3f >= %.3f, criteria gates passed, skymap present", ...
            result.score, criteria.score_min)};
    else
        result.is_too = false;
        result.decision = "reject";
    end
    result.priority = assignTooPriorityCriteria(result.is_too, result.score);
end


function priority = assignTooPriorityCriteria(is_too, score)
    % Same priority bands as simple filter.

    if ~is_too
        priority = "none";
        return;
    end
    if score >= 1.6
        priority = "high";
    elseif score >= 1.3
        priority = "medium";
    else
        priority = "low";
    end
end


function result = hasFiniteScalar(value)
    result = isnumeric(value) && isscalar(value) && isfinite(value);
end
