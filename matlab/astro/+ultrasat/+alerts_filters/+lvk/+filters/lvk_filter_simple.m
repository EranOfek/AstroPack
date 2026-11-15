%==========================================================================
% Project     : ULTRASAT Incoming Alerts Filter
% File        : +ultrasat/+alerts_filters/+lvk/+filters/lvk_filter_simple.m
% Author      : Chen Tishler
% Created     : 01/12/2024
% Updated     : 12/05/2026
% Description : Simple LVK filter implementation.
%==========================================================================

function result = lvk_filter_simple(alert, logger)
    % Weighted score from hydrated classification probs (alert.event.classification).
    % Uses alert.prob_* and alert.far_per_year for flags; no criteria file.

    % Log the alert
    logger.info("Filtering alert: %s", alert.alert_id);

    % Initialize result
    result = ultrasat.alerts_filters.lvk.models.LvkFilterResult();

    % Score from event.classification (via hydration): BNS weight 2.0.
    if hasFiniteScalar(alert.prob_bns)
        result.score = result.score + 2.0 * alert.prob_bns;
        result.reasons{end+1} = sprintf("BNS contribution: %.3f", alert.prob_bns);
    end

    % Add NSBH contribution
    if hasFiniteScalar(alert.prob_nsbh)
        result.score = result.score + 1.5 * alert.prob_nsbh;
        result.reasons{end+1} = sprintf("NSBH contribution: %.3f", alert.prob_nsbh);
    end

    % Add BBH contribution
    if hasFiniteScalar(alert.prob_bbh)
        result.score = result.score + 0.2 * alert.prob_bbh;
        result.reasons{end+1} = sprintf("BBH contribution: %.3f", alert.prob_bbh);
    end

    % Add terrestrial penalty
    if hasFiniteScalar(alert.prob_terrestrial)
        result.score = result.score - 2.0 * alert.prob_terrestrial;
        result.reasons{end+1} = sprintf("Terrestrial penalty: %.3f", alert.prob_terrestrial);
    end

    result.score = max(result.score, 0.0);

    % Echo classification snapshot; flags use derived skymap_path and far_per_year.
    result.class_probs = struct( ...
        "bns", alert.prob_bns, ...
        "nsbh", alert.prob_nsbh, ...
        "bbh", alert.prob_bbh, ...
        "terrestrial", alert.prob_terrestrial ...
    );
    result.flags = struct();
    result.flags.has_skymap = strlength(string(alert.skymap_path)) > 0;
    result.flags.low_far = hasFiniteScalar(alert.far_per_year) && alert.far_per_year < 10;

    % Final TOO decision (scientist-owned thresholds; edit here without changing score).
    result = applyTooDecisionSimple(result, alert);

    % Log the result
    msg = strjoin(string(result.reasons), "; ");
    logger.info("Filter result: score=%.2f, is_too=%d, reasons=%s", ...
        result.score, result.is_too, msg);

end


function result = applyTooDecisionSimple(result, alert)
    % Set is_too when score, FAR, skymap, and terrestrial meet simple-filter rules.

    scoreMin = 1.0;
    farMax = 10.0;
    terrestrialMax = 0.5;
    reasons = {};

    if result.score < scoreMin
        reasons{end+1} = sprintf("REJECT: score %.3f < %.3f", result.score, scoreMin); %#ok<AGROW>
    end
    if ~hasFiniteScalar(alert.far_per_year) || alert.far_per_year > farMax
        if hasFiniteScalar(alert.far_per_year)
            reasons{end+1} = sprintf("REJECT: far_per_year %.4g > %.1f", alert.far_per_year, farMax); %#ok<AGROW>
        else
            reasons{end+1} = "REJECT: far_per_year missing or non-finite"; %#ok<AGROW>
        end
    end
    if ~result.flags.has_skymap
        reasons{end+1} = "REJECT: skymap_path empty"; %#ok<AGROW>
    end
    if hasFiniteScalar(alert.prob_terrestrial) && alert.prob_terrestrial >= terrestrialMax
        reasons{end+1} = sprintf("REJECT: terrestrial %.3f >= %.1f", alert.prob_terrestrial, terrestrialMax); %#ok<AGROW>
    end

    result.decision_reasons = reasons;
    if isempty(reasons)
        result.is_too = true;
        result.decision = "accept";
        result.decision_reasons = {sprintf( ...
            "ACCEPT: score %.3f >= %.1f, FAR %.4g/yr, skymap present", ...
            result.score, scoreMin, alert.far_per_year)};
    else
        result.is_too = false;
        result.decision = "reject";
    end
    result.priority = assignTooPriority(result.is_too, result.score);
end


function priority = assignTooPriority(is_too, score)
    % Map accepted score to operational priority band.

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
