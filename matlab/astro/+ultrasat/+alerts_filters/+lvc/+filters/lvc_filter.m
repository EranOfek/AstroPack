%==========================================================================
% Project     : ULTRASAT Incoming Alerts Filter
% File        : +ultrasat/+alerts_filters/+lvc/+filters/lvc_filter.m
% Author      : Chen Tishler
% Created     : 01/12/2024
% Updated     : 12/05/2026
% Description : LVC filter implementation entry point.
%==========================================================================

function result = lvc_filter(Input, logger)
    % LVC filter implementation entry point
    %
    % Parameters:
    %   alert - LvcParsedAlert object
    %   logger - Logger object
    %
    % Returns:
    %   result - Struct with score, class_probs, flags, and reasons

    % Log the alert
    logger.info("Filtering alert: %s", alert.alert_id);


    alert = Input.alert;

    filter_name = "simple";

    % Get the filter name from the input
    if isfield(Input, 'filter')
        filter_name = string(Input.filter);
    end

    logger.info("Filtering alert: %s using filter=%s", alert.alert_id, filter_name);

    % Process the alert using the selected filter
    switch filter_name

        case "simple"
            result = ultrasat.alerts_filters.lvc.filters.lvc_filter_simple(alert, logger);

        otherwise
            error("Unknown LVC filter: %s", filter_name);
    end

    % Log the result
    msg = strjoin(string(result.reasons), "; ");
    logger.info("Filter result: score=%.2f, reasons=%s", result.score, msg);

    % Return the result
    result = struct;
    result.status  = 'ok';
    result.message = 'lvc_filter: OK';
    result.result = result;
end

