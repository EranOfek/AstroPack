%==========================================================================
% Project     : ULTRASAT Incoming Alerts Filter
% File        : +ultrasat/+alerts_filters/+lvk/+filters/lvk_filter.m
% Author      : Chen Tishler
% Created     : 01/12/2024
% Updated     : 12/05/2026
% Description : LVK filter implementation entry point.
%==========================================================================

function result = lvk_filter(Input, logger)
    % LVK filter implementation entry point
    %
    % Parameters:
    %   Input - Request struct containing an LvkParsedAlert in Input.alert
    %   logger - Logger object
    %
    % Returns:
    %   result - LvkFilterResult

    if nargin < 2 || isempty(logger)
        logger = MsgLogger.getSingleton();
    end

    if ~isfield(Input, 'alert') || isempty(Input.alert)
        error('LvkFilter:MissingAlert', 'Input.alert is required');
    end

    alert = Input.alert;
    if ~isa(alert, 'ultrasat.alerts_filters.lvk.models.LvkParsedAlert')
        error('LvkFilter:InvalidAlert', 'Input.alert must be an LvkParsedAlert');
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
            result = ultrasat.alerts_filters.lvk.filters.lvk_filter_simple(alert, logger);

        case {"with_criteria", "criteria"}
            criteria = getCriteria(Input);
            result = ultrasat.alerts_filters.lvk.filters.lvk_filter_with_criteria(alert, criteria, logger);

        otherwise
            error("Unknown LVK filter: %s", filter_name);
    end

    % Log the result
    msg = strjoin(string(result.reasons), "; ");
    logger.info("Filter result: score=%.2f, reasons=%s", result.score, msg);
end


function criteria = getCriteria(Input)
    % Build criteria only when a criteria-based filter is explicitly requested.
    if isfield(Input, 'criteria') && ~isempty(Input.criteria)
        if isa(Input.criteria, 'ultrasat.alerts_filters.lvk.models.LvkFilterCriteria')
            criteria = Input.criteria;
        elseif isstruct(Input.criteria)
            criteria = ultrasat.alerts_filters.lvk.models.LvkFilterCriteria.fromStruct(Input.criteria);
        else
            error('LvkFilter:InvalidCriteria', 'Input.criteria must be an LvkFilterCriteria or struct');
        end
    elseif isfield(Input, 'criteria_file') && ~isempty(Input.criteria_file)
        criteriaFile = char(string(Input.criteria_file));
        if ~isfile(criteriaFile)
            error('LvkFilter:CriteriaFileNotFound', 'Criteria file not found: %s', criteriaFile);
        end
        criteria = ultrasat.alerts_filters.lvk.models.LvkFilterCriteria.loadFromJsonFile(criteriaFile);
    else
        criteria = ultrasat.alerts_filters.lvk.models.LvkFilterCriteria();
    end
end
