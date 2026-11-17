%==========================================================================
% Project     : ULTRASAT Incoming Alerts Filter
% Filename    : ultrasat/+services/+alerts_filter/processRequest.m
% Author      : Chen Tishler
% Created     : 02/11/2025
% Modified    : 15/11/2026
% Description : MATLAB service to process AlertsFilter requests using JsonFileIpc
%==========================================================================

function Output = processRequest(Input)
    % Dispatch health or filter_lvk; filter writes result_<filter>.json when output_folder resolves.

    Output = struct;
    Output.message   = 'processAlertsFilter: started';
    Output.status    = 'error';

    try
        if ~isfield(Input, 'action')
            error('AlertsFilter:MissingAction', 'Missing required field: action');
        end

        action = string(Input.action);

        if action == "health"
            Output.status  = 'ok';
            Output.message = 'health: OK';
        elseif action == "filter_lvk" || action == "filter_lvc"
            Output = processFilterLvk(Input);
        else
            Output.message = sprintf('processRequest: unknown action: %s', action);
            Output.status = 'error';
        end
    catch ex
        Output.message = sprintf("processRequest: error: identifier='%s', message='%s'", ex.identifier, ex.message);
        Output.status = 'error';
        io.msgLog(LogLevel.Error, Output.message);
    end
end


function Output = processFilterLvk(Input)
    % Load parsed_alert.json, run lvk_filter, write result JSON under output_folder.

    logger = MsgLogger.getSingleton();
    Output = struct;
    Output.status = 'error';
    Output.message = 'processFilterLvk: not started';
    Output.result_file = '';
    Output.filter = '';
    Output.alert_id = '';
    Output.superevent_id = '';
    Output.result = [];

    outputFolder = "";
    alertFile = "";

    try
        if ~isfield(Input, 'alert_file') || isempty(Input.alert_file)
            error('AlertsFilter:MissingAlertFile', 'Missing required field: alert_file');
        end

        if ~isfield(Input, 'filter') || isempty(Input.filter)
            Input.filter = 'simple';
        end

        filterName = char(lower(string(Input.filter)));
        alertFile = char(ultrasat.tools.localPathForIo(Input.alert_file));
        if ~isfile(alertFile)
            error('AlertsFilter:AlertFileNotFound', 'Alert file not found: %s', alertFile);
        end

        outputFolder = resolveOutputFolder(Input, alertFile);

        logger.info("Loading LVK alert file: %s", ultrasat.tools.pathForJson(alertFile));
        alert = ultrasat.alerts_filters.lvk.models.LvkParsedAlert.loadFromJsonFile(alertFile);

        Input.alert = alert;
        result = ultrasat.alerts_filters.lvk.filters.lvk_filter(Input, logger);

        Output.status = 'ok';
        Output.message = 'processFilterLvk: OK';
        Output.filter = filterName;
        Output.alert_id = char(alert.alert_id);
        Output.superevent_id = char(alert.superevent_id);
        Output.result = result.toStruct();

        resultFile = fullfile(outputFolder, sprintf('result_%s.json', filterName));
        writeFilterResultFile(resultFile, Output, alertFile, filterName);

        Output.result_file = ultrasat.tools.pathForJson(resultFile);

    catch ex
        Output.status = 'error';
        Output.message = sprintf("processFilterLvk: error: identifier='%s', message='%s'", ex.identifier, ex.message);
        Output.result = defaultErrorResult();
        io.msgLog(LogLevel.Error, Output.message);

        if strlength(outputFolder) > 0 && isfield(Input, 'filter')
            filterName = char(lower(string(Input.filter)));
            try
                if ~isfolder(outputFolder)
                    mkdir(outputFolder);
                end
                resultFile = fullfile(outputFolder, sprintf('result_%s.json', filterName));
                writeFilterResultFile(resultFile, Output, alertFile, filterName);
                Output.result_file = ultrasat.tools.pathForJson(resultFile);
            catch writeEx
                io.msgLog(LogLevel.Warning, 'processFilterLvk: could not write error result: %s', writeEx.message);
            end
        end
    end
end


function outputFolder = resolveOutputFolder(Input, alertFile)
    % Prefer request output_folder; else <parent(alert_file)>/alert_filter.

    if isfield(Input, 'output_folder') && ~isempty(Input.output_folder)
        outputFolder = char(ultrasat.tools.localPathForIo(Input.output_folder));
    else
        outputFolder = fullfile(fileparts(alertFile), 'alert_filter');
    end

    if ~isfolder(outputFolder)
        mkdir(outputFolder);
    end
end


function writeFilterResultFile(resultFile, Output, alertFile, filterName)
    % Persist full envelope JSON for Python to load from the share.

  payload = struct();
  payload.status = Output.status;
  payload.message = Output.message;
  payload.action = 'filter_lvk';
  payload.filter = filterName;
  payload.alert_id = safeField(Output, 'alert_id', '');
  payload.superevent_id = safeField(Output, 'superevent_id', '');
  payload.alert_file = ultrasat.tools.pathForJson(alertFile);
  payload.output_folder = ultrasat.tools.pathForJson(fileparts(resultFile));
  payload.created_time_utc = utcNowIsoZ();
  payload.result = Output.result;

    text = jsonencode(payload, 'PrettyPrint', true);
    fid = fopen(resultFile, 'w', 'n', 'UTF-8');
    if fid < 0
        error('AlertsFilter:WriteFailed', 'Cannot write: %s', resultFile);
    end
    fwrite(fid, text, 'char');
    fclose(fid);
end


function r = defaultErrorResult()
    % Minimal result block when filtering fails before LvkFilterResult is built.
    r = struct( ...
        'score', 0.0, ...
        'is_too', false, ...
        'decision', 'reject', ...
        'priority', 'none', ...
        'decision_reasons', {{'REJECT: filter error'}}, ...
        'class_probs', struct(), ...
        'flags', struct(), ...
        'reasons', {{}} ...
    );
end


function iso = utcNowIsoZ()
    % UTC timestamp string for result envelope.
    iso = char(datetime('now', 'TimeZone', 'UTC', 'Format', "yyyy-MM-dd'T'HH:mm:ss.SSS'Z'"));
end


function val = safeField(s, fieldName, defaultVal)
    % Read struct field or return default when missing or empty.
    if isfield(s, fieldName) && ~isempty(s.(fieldName))
        val = s.(fieldName);
    else
        val = defaultVal;
    end
end