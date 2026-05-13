%==========================================================================
% Project     : ULTRASAT SOC
% Filename    : +ultrasat/+services/+too_planner/processRequest.m
% Author      : Chen Tishler
% Created     : 02/11/2025
% Modified    : 13/05/2026
% Description : MATLAB service to process ToO planner requests (flat JSON API, matching slew_calc pattern)
%==========================================================================

function Output = processRequest(Input)
    % Process request: dispatch on action (flat JSON, no inner json_text).
    %
    % Input  : struct with .action ('health'|'too_planner') and action-specific fields.
    %            For 'too_planner': .planner_name, .csv_filename, .output_folder, .plans[]
    % Output : ApiBaseResponse-style: .status ('ok'|'error'), .message;
    %            for 'too_planner' also .summary_file, .total_plans_*, .plans[]

    Output = struct;
    Output.status  = '?';
    Output.message = 'MATLAB: Exception in processRequest';

    try
        if strcmp(Input.action, 'health')
            Output.status  = 'ok';
            Output.message = 'health: OK';
        elseif strcmp(Input.action, 'too_planner')
            Output = processTooPlanner(Input);
        else
            Output.status  = 'error';
            Output.message = 'processRequest: unknown action';
        end
    catch Ex
        Output.status  = 'error';
        Output.message = sprintf('exception: %s', Ex.message);
    end
end

% ===========================================================================

function Output = processTooPlanner(Input)
    % Process TOO planner request with flat input fields.
    %
    % The Input struct contains the full configuration inline:
    %   .planner_name, .csv_filename, .output_folder, .plans (struct array)
    %
    % We write it to a temp JSON file because TooPlannerRunner.runFromJson
    % expects a file path, then parse the summary.json it produces.

    try
        % Validate required fields
        if ~isfield(Input, 'csv_filename') || isempty(Input.csv_filename)
            error('ToOPlanner:MissingField', 'Missing required field: csv_filename');
        end
        if ~isfield(Input, 'output_folder') || isempty(Input.output_folder)
            error('ToOPlanner:MissingField', 'Missing required field: output_folder');
        end
        if ~isfield(Input, 'plans') || isempty(Input.plans)
            error('ToOPlanner:MissingField', 'Missing required field: plans');
        end

        % Build the config struct for TooPlannerRunner (exclude action and timeout)
        cfg = struct();
        if isfield(Input, 'planner_name')
            cfg.planner_name = Input.planner_name;
        end
        cfg.csv_filename  = Input.csv_filename;
        cfg.output_folder = Input.output_folder;
        cfg.plans         = Input.plans;

        % Write config to a temp JSON file in the output folder
        if ~isfolder(Input.output_folder)
            mkdir(Input.output_folder);
        end
        ts = datetime('now', 'TimeZone', 'UTC');
        tsStr = datestr(ts, 'yyyymmdd_HHMMSS_FFF');
        tempJsonFile = fullfile(Input.output_folder, ['_request_' tsStr '.json']);
        jsonText = jsonencode(cfg, 'PrettyPrint', true);
        fid = fopen(tempJsonFile, 'wt');
        fwrite(fid, jsonText, 'char');
        fclose(fid);

        % Run the planner
        runner = ultrasat.planner.TooPlannerRunner();
        summaryFileName = runner.runFromJson(tempJsonFile);

        % Clean up the temp request file
        try
            if isfile(tempJsonFile)
                delete(tempJsonFile);
            end
        catch
        end

        % Build the output struct
        Output = struct;
        Output.status  = 'ok';
        Output.message = 'TooPlannerRunner: OK';
        Output.summary_file = '';
        Output.total_plans_attempted = 0;
        Output.total_plans_succeeded = 0;
        Output.total_plans_failed = 0;
        Output.plans = [];

        % Parse the summary.json to populate rich response fields
        if ~isempty(summaryFileName) && isfile(summaryFileName)
            Output.summary_file = char(summaryFileName);
            try
                summaryText = fileread(summaryFileName);
                summary = jsondecode(summaryText);

                if isfield(summary, 'total_plans_attempted')
                    Output.total_plans_attempted = summary.total_plans_attempted;
                end
                if isfield(summary, 'total_plans_succeeded')
                    Output.total_plans_succeeded = summary.total_plans_succeeded;
                end
                if isfield(summary, 'total_plans_failed')
                    Output.total_plans_failed = summary.total_plans_failed;
                end
                if isfield(summary, 'plans') && ~isempty(summary.plans)
                    Output.plans = parseSummaryPlans(summary.plans);
                end
            catch Ex
                io.msgLog(LogLevel.Warning, 'processTooPlanner: failed to parse summary.json: %s', Ex.message);
            end
        end

    catch Ex
        Output.status  = 'error';
        Output.message = sprintf("processTooPlanner: error: identifier='%s', message='%s'", Ex.identifier, Ex.message);
        io.msgLog(LogLevel.Error, Output.message);
    end
end

% ===========================================================================

function plans = parseSummaryPlans(rawPlans)
    % Convert summary.plans (cell array or struct array) to a flat struct array
    % with only the fields the Python response model expects.

    plans = struct('run_id', {}, 'json_file', {}, 'mat_file', {}, ...
                   'plan_index', {}, 'status', {}, 'exposures_scheduled', {});

    if iscell(rawPlans)
        for i = 1:numel(rawPlans)
            plans(end+1) = extractPlanFields(rawPlans{i}); %#ok<AGROW>
        end
    elseif isstruct(rawPlans)
        for i = 1:numel(rawPlans)
            plans(end+1) = extractPlanFields(rawPlans(i)); %#ok<AGROW>
        end
    end
end


function p = extractPlanFields(raw)
    p = struct();
    p.run_id = safeField(raw, 'run_id', '');
    p.json_file = safeField(raw, 'json_file', '');
    p.mat_file = safeField(raw, 'mat_file', '');
    p.plan_index = safeField(raw, 'plan_index', 0);
    p.status = safeField(raw, 'status', 'error');
    p.exposures_scheduled = safeField(raw, 'exposures_scheduled', 0);
end


function val = safeField(s, fieldName, defaultVal)
    if isfield(s, fieldName) && ~isempty(s.(fieldName))
        val = s.(fieldName);
    else
        val = defaultVal;
    end
end
