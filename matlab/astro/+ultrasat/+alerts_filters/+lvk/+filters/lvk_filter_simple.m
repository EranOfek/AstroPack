%==========================================================================
% Project     : ULTRASAT Incoming Alerts Filter
% File        : +ultrasat/+alerts_filters/+lvk/+filters/lvk_filter_simple.m
% Author      : Chen Tishler
% Created     : 01/12/2024
% Updated     : 12/05/2026
% Description : Simple LVK filter implementation.
%==========================================================================

function result = lvk_filter_simple(alert, logger)
    % Simple LVK filter implementation
    %
    % Parameters:
    %   alert - LvkParsedAlert object
    %   logger - Logger object
    %
    % Returns:
    %   result - LvkFilterResult

    % Log the alert
    logger.info("Filtering alert: %s", alert.alert_id);

    % Initialize result
    result = ultrasat.alerts_filters.lvk.models.LvkFilterResult();

    % Add BNS contribution
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
    result.class_probs = struct( ...
        "bns", alert.prob_bns, ...
        "nsbh", alert.prob_nsbh, ...
        "bbh", alert.prob_bbh, ...
        "terrestrial", alert.prob_terrestrial ...
    );
    result.flags = struct();
    result.flags.has_skymap = strlength(string(alert.skymap_path)) > 0;
    result.flags.low_far = hasFiniteScalar(alert.far_per_year) && alert.far_per_year < 10;

    % Log the result
    msg = strjoin(string(result.reasons), "; ");
    logger.info("Filter result: score=%.2f, reasons=%s", result.score, msg);

end


function result = hasFiniteScalar(value)
    result = isnumeric(value) && isscalar(value) && isfinite(value);
end
