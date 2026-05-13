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
    %   Input - Request struct containing an LvcParsedAlert in Input.alert
    %   logger - Logger object
    %
    % Returns:
    %   result - LvcFilterResult

    if nargin < 2 || isempty(logger)
        logger = MsgLogger.getSingleton();
    end

    if ~isfield(Input, 'alert') || isempty(Input.alert)
        error('LvcFilter:MissingAlert', 'Input.alert is required');
    end

    alert = Input.alert;
    if ~isa(alert, 'ultrasat.alerts_filters.lvc.models.LvcParsedAlert')
        error('LvcFilter:InvalidAlert', 'Input.alert must be an LvcParsedAlert');
    end

    filter_name = "simple";

    % Get the filter name from the input
    if isfield(Input, 'filter')
        filter_name = lower(string(Input.filter));
    end

    logger.info("Filtering alert: %s using filter=%s", alert.alert_id, filter_name);

    % Process the alert using the selected filter
    switch filter_name

        case "simple"
            result = ultrasat.alerts_filters.lvc.filters.lvc_filter_simple(alert, logger);

        case {"with_criteria", "criteria"}
            criteria = getCriteria(Input);
            result = ultrasat.alerts_filters.lvc.filters.lvc_filter_with_criteria(alert, criteria, logger);

        otherwise
            error("Unknown LVC filter: %s", filter_name);
    end

    % Log the result
    msg = strjoin(string(result.reasons), "; ");
    logger.info("Filter result: score=%.2f, reasons=%s", result.score, msg);
end


function criteria = getCriteria(Input)
    % Build criteria only when a criteria-based filter is explicitly requested.
    if isfield(Input, 'criteria') && ~isempty(Input.criteria)
        if isa(Input.criteria, 'ultrasat.alerts_filters.lvc.models.LvcFilterCriteria')
            criteria = Input.criteria;
        elseif isstruct(Input.criteria)
            criteria = ultrasat.alerts_filters.lvc.models.LvcFilterCriteria.fromStruct(Input.criteria);
        else
            error('LvcFilter:InvalidCriteria', 'Input.criteria must be an LvcFilterCriteria or struct');
        end
    elseif isfield(Input, 'criteria_file') && ~isempty(Input.criteria_file)
        criteriaFile = char(string(Input.criteria_file));
        if ~isfile(criteriaFile)
            error('LvcFilter:CriteriaFileNotFound', 'Criteria file not found: %s', criteriaFile);
        end
        criteria = ultrasat.alerts_filters.lvc.models.LvcFilterCriteria.loadFromJsonFile(criteriaFile);
    else
        criteria = ultrasat.alerts_filters.lvc.models.LvcFilterCriteria();
    end
end

