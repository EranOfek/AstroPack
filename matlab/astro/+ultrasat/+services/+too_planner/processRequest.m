%==========================================================================
% Project     : ULTRASAT SOC
% Filename    : +ultrasat/+services/+too_planner/processRequest.m
% Author      : Chen Tishler
% Created     : 02/11/2025
% Modified    : 17/05/2026
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
    % Input.IpcInputJsonFilename (set by JsonFileIpc) is the IPC input file path.
    % That file is copied as-is into output_folder under its original name;
    % TooPlannerRunner.runFromJson uses the copy, which is kept as an artifact.

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
        if ~isfield(Input, 'IpcInputJsonFilename') || isempty(Input.IpcInputJsonFilename)
            error('ToOPlanner:MissingIpcInput', ...
                'Missing IpcInputJsonFilename; too_planner requests must be processed via JsonFileIpc');
        end
        if ~isfile(Input.IpcInputJsonFilename)
            error('ToOPlanner:IpcInputNotFound', ...
                'IPC input file not found: %s', Input.IpcInputJsonFilename);
        end

        if ~isfolder(Input.output_folder)
            mkdir(Input.output_folder);
        end

        [~, name, ext] = fileparts(Input.IpcInputJsonFilename);
        destJson = fullfile(Input.output_folder, [name, ext]);
        copyfile(Input.IpcInputJsonFilename, destJson, 'f');

        runner = ultrasat.planner.TooPlannerRunner();
        summaryFileName = runner.runFromJson(destJson);

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
    % Extract fields from raw plan struct
    % and return a struct with only the fields the Python response model expects.

    p = struct();
    p.run_id = safeField(raw, 'run_id', '');
    p.json_file = safeField(raw, 'json_file', '');
    p.mat_file = safeField(raw, 'mat_file', '');
    p.plan_index = safeField(raw, 'plan_index', 0);
    p.status = safeField(raw, 'status', 'error');
    p.exposures_scheduled = safeField(raw, 'exposures_scheduled', 0);
    p.images = safeField(raw, 'images', struct());
end


function val = safeField(s, fieldName, defaultVal)
    % Get field from struct, return default value if field is empty

    if isfield(s, fieldName) && ~isempty(s.(fieldName))
        val = s.(fieldName);
    else
        val = defaultVal;
    end
end
