%==========================================================================
% Project     : ULTRASAT Observation Planner
% Filename    : ultrasat.api.MissionApiClient.m
% Author      : Chen Tishler
% Created     : 01/12/2024
% Updated     : 21/09/2025
% Description : Client implementation of the MissionApiBase interface.
%==========================================================================

classdef MissionApiClient < ultrasat.api.MissionApiBase
    % Client implementation of the MissionClientBase interface
    % Provides communication with the Mission API server

    properties
        Client          % ultrasat.api.ClientBase instance for HTTP requests
        ApiUrl          % Base URL for API endpoints
    end


    methods
        function obj = MissionApiClient(Args)
            % Constructor for MissionApiClient (FastAPI plans_manager).
            %
            % Parameters:
            %   Args.ApiUrl (string) - Plans manager base URL (e.g. http://host:8321)
            %   Args.Namespace (string) - Namespace header value
            %   Args.ApiKey (string) - API key for authentication
            %   Args.SubUrl (string) - Optional path suffix (default '')
            %
            % Returns:
            %   obj - Initialized MissionApiClient object
            arguments
                Args.ApiUrl = '';
                Args.Namespace = '';
                Args.ApiKey = '';
                Args.SubUrl = '';
                Args.LogFileName = [];
            end

            % Call parent constructor (MissionApiBase accepts only SubUrl, LogFileName)
            baseArgs = struct('SubUrl', Args.SubUrl, 'LogFileName', Args.LogFileName);
            baseArgsCell = namedargs2cell(baseArgs);
            obj@ultrasat.api.MissionApiBase(baseArgsCell{:});

            % Base URL for plans_manager (no trailing slash)
            baseUrl = Args.ApiUrl;
            if isempty(baseUrl)
                baseUrl = getenv('SOC_API_BASE');
            end
            if isempty(baseUrl)
                baseUrl = 'http://localhost:8321';
            end
            obj.ApiUrl = baseUrl;

            % HTTP client with namespace and api-key for FastAPI plans_manager
            apiKey = Args.ApiKey;
            if isempty(apiKey)
                apiKey = getenv('SOC_API_KEY');
            end
            obj.Client = ultrasat.api.ClientBase(...
                'BaseUrl', baseUrl, ...
                'SubUrl', Args.SubUrl, ...
                'ApiKey', apiKey, ...
                'Namespace', Args.Namespace);
        end

        % -------------------------------------------------------------------

        function response = getApprovedTargets(obj, start_time, end_time)
            % Retrieves the list of approved observation targets within a time range.
            %
            % Parameters:
            %   start_time (datetime) - Start time for filtering targets
            %   end_time (datetime) - End time for filtering targets
            %
            % Returns:
            %   response - Structure containing result
            %
            % Notes:
            %   This method updates the ApprovedTargetsStartTime and
            %   ApprovedTargetsEndTime properties.
            obj.msglog('getApprovedTargets: start_time=%s, end_time=%s', datestr(start_time), datestr(end_time));

            % Format dates for API
            if isdatetime(start_time)
                start_str = datestr(start_time, 'yyyy-mm-ddTHH:MM:SS.FFFZ');
            else
                start_str = start_time;
            end

            if isdatetime(end_time)
                end_str = datestr(end_time, 'yyyy-mm-ddTHH:MM:SS.FFFZ');
            else
                end_str = end_time;
            end

            % Store the times
            obj.ApprovedTargetsStartTime = start_time;
            obj.ApprovedTargetsEndTime = end_time;

            % Send request
            params = struct('start_time', start_str, 'end_time', end_str);
            response = obj.Client.postRequest('/get_approved_targets/', params);
            response.ok = isfield(response, 'status') && strcmp(response.status, 'ok');
        end


        function response = validatePlan(obj, Plan)
            % Validates an observation plan against mission constraints.
            %
            % Parameters:
            %   Plan - Array of structs containing observation data
            %
            % Returns:
            %   response - Structure containing validation result
            obj.msglog('validatePlan: Validating plan with pk=%d', obj.PlanData.pk);

            % Convert date/time fields to UTC
            Plan = obj.convertPlanTimesToUtc(Plan);

            % Send request
            params = struct('plan', Plan);
            response = obj.Client.postRequest('/validate_plan/', params);

            % Update response.ok based on status
            response.ok = isfield(response, 'status') && strcmp(response.status, 'ok');

            % Ensure metadata.ValidationResponse exists as a cell array
            if ~isfield(obj.PlanData.metadata, 'ValidationResponse') || isempty(obj.PlanData.metadata.ValidationResponse)
                obj.PlanData.metadata.ValidationResponse = {}; % Initialize as empty cell array
            elseif ~iscell(obj.PlanData.metadata.ValidationResponse)
                obj.PlanData.metadata.ValidationResponse = {obj.PlanData.metadata.ValidationResponse}; % Convert to cell if needed
            end

            % Insert the latest response at the beginning of the array (most recent first)
            obj.PlanData.metadata.ValidationResponse = [{response}, obj.PlanData.metadata.ValidationResponse];

            obj.msglog('Validation status: %s', response.status);
        end


        function response = submitPlan(obj, Plan)
            % Submits the plan by setting status to 'submitted' and saving via save_plan.
            obj.msglog('submitPlan: Submitting plan with pk=%d', obj.PlanData.pk);
            try
                obj.PlanData.status = 'submitted';
                obj.PlanData.addHistory(sprintf('plan submitted by %s', obj.PlanData.created_by));
                response = obj.savePlan();
                if response.ok
                    obj.msglog('Plan %d submitted successfully.', obj.PlanData.pk);
                end
            catch ME
                obj.msglog('submitPlan failed: %s', ME.message);
                response = struct('status', 'error', 'message', ME.message, 'ok', false);
            end
        end


        function response = retractPlan(obj, Plan)
            % Retracts a previously submitted observation plan.
            %
            % Parameters:
            %   Plan - Array of structs identifying the plan to retract
            %
            % Returns:
            %   response - Structure containing retraction result
            obj.msglog('retractPlan: Not implemented yet');

            % Send request
            params = struct('plan', Plan);
            response = obj.Client.postRequest('/retract_plan/', params);

            % Update response.ok based on status
            response.ok = isfield(response, 'status') && strcmp(response.status, 'ok');
        end


        function response = getExposure(obj, table_name, healpix_indices, start_timestamp, end_timestamp, select_all)
            % Retrieves exposure data for specified healpix indices and time range.
            %
            % Parameters:
            %   table_name (string) - Name of the exposure data table
            %   healpix_indices (array) - Array of healpix indices to filter by
            %   start_timestamp (datetime) - Start time for filtering exposures
            %   end_timestamp (datetime) - End time for filtering exposures
            %   select_all (logical) - If true, returns all records regardless of filters
            %
            % Returns:
            %   response - Structure containing result
            obj.msglog('getExposure: table=%s, healpix_indices=%s, start=%s, end=%s, select_all=%d', ...
                       table_name, mat2str(healpix_indices), datestr(start_timestamp), datestr(end_timestamp), select_all);

            % Format dates for API
            if isdatetime(start_timestamp)
                start_str = datestr(start_timestamp, 'yyyy-mm-ddTHH:MM:SS.FFFZ');
            else
                start_str = start_timestamp;
            end

            if isdatetime(end_timestamp)
                end_str = datestr(end_timestamp, 'yyyy-mm-ddTHH:MM:SS.FFFZ');
            else
                end_str = end_timestamp;
            end

            % Send request
            params = struct(...
                'table_name', table_name, ...
                'healpix_indices', healpix_indices, ...
                'start_timestamp', start_str, ...
                'end_timestamp', end_str, ...
                'select_all', select_all ...
            );

            response = obj.Client.postRequest('/get_exposure/', params);
            response.ok = isfield(response, 'status') && strcmp(response.status, 'ok');
        end

        % =================================================================
        %                       Plans Table CRUD
        % =================================================================

        function response = getPlansList(obj, start_timestamp, end_timestamp, title_subtext)
            % Retrieves a list of plans from FastAPI plans_manager (POST /get_plans_list).
            % Returns response.plans for GUI compatibility; API returns response.data.
            obj.msglog('getPlansList: Scanning for plans');

            if nargin < 2, start_timestamp = []; end
            if nargin < 3, end_timestamp = []; end
            if nargin < 4, title_subtext = ''; end

            % Python API: start_time, end_time, status (ISO or null)
            start_str = [];
            end_str = [];
            if ~isempty(start_timestamp) && isdatetime(start_timestamp)
                start_str = datestr(start_timestamp, 'yyyy-mm-ddTHH:MM:SS.FFFZ');
            elseif ~isempty(start_timestamp)
                start_str = start_timestamp;
            end
            if ~isempty(end_timestamp) && isdatetime(end_timestamp)
                end_str = datestr(end_timestamp, 'yyyy-mm-ddTHH:MM:SS.FFFZ');
            elseif ~isempty(end_timestamp)
                end_str = end_timestamp;
            end
            % title_subtext not in API; use as status filter if needed or omit
            status_filter = [];
            if ~isempty(title_subtext)
                status_filter = title_subtext;
            end

            params = struct('start_time', start_str, 'end_time', end_str, 'status', status_filter);
            params = ultrasat.api.ModelBase.removeEmptyFields(params);

            try
                response = obj.Client.postRequest('/get_plans_list', params);
            catch ME
                obj.msglog('getPlansList failed: %s', ME.message);
                response = struct('status', 'error', 'message', ME.message, 'data', [], 'ok', false);
                response.plans = [];
                return;
            end

            response.ok = isfield(response, 'status') && strcmp(response.status, 'ok');
            % Map API response.data -> response.plans for GUI; add create_time, update_time, ast_planner if missing
            if response.ok && isfield(response, 'data') && ~isempty(response.data)
                plansList = response.data;
                if iscell(plansList)
                    for i = 1:numel(plansList)
                        p = plansList{i};
                        if ~isfield(p, 'create_time'), p.create_time = []; end
                        if ~isfield(p, 'update_time')
                            p.update_time = [];
                            if isfield(p, 'updated_time') && ~isempty(p.updated_time), p.update_time = p.updated_time; end
                        end
                        if ~isfield(p, 'ast_planner'), p.ast_planner = ''; end
                        plansList{i} = p;
                    end
                else
                    for i = 1:numel(plansList)
                        if ~isfield(plansList(i), 'create_time'), plansList(i).create_time = []; end
                        if ~isfield(plansList(i), 'update_time')
                            plansList(i).update_time = [];
                            if isfield(plansList(i), 'updated_time') && ~isempty(plansList(i).updated_time)
                                plansList(i).update_time = plansList(i).updated_time;
                            end
                        end
                        if ~isfield(plansList(i), 'ast_planner'), plansList(i).ast_planner = ''; end
                    end
                end
                response.plans = plansList;
            else
                response.plans = [];
            end
        end


        function response = loadPlan(obj, plan_pk)
            % Loads a plan by pk from FastAPI plans_manager (POST /get_plan).
            % Populates obj.PlanData; planner is left [] (no matlab_mat restore).
            obj.msglog('loadPlan: Loading plan with pk=%d', plan_pk);

            params = struct('plan_pk', plan_pk);
            try
                response = obj.Client.postRequest('/get_plan', params);
            catch ME
                obj.msglog('loadPlan failed: %s', ME.message);
                response = struct('status', 'error', 'message', ME.message, 'data', [], 'ok', false);
                return;
            end

            response.ok = isfield(response, 'status') && strcmp(response.status, 'ok');
            if ~response.ok || ~isfield(response, 'data') || isempty(response.data)
                obj.msglog('Failed to load plan %d: %s', plan_pk, getfield(response, 'message', ''));
                return;
            end

            % Convert API PlanData to MATLAB (created_time->create_time, updated_time->update_time, decl->Dec)
            apiPlan = response.data;
            matlabPlan = obj.apiToPlanStruct(apiPlan);
            matlabPlan.planner = [];  % No planner from API
            obj.PlanData = ultrasat.api.PlanData.fromStruct(matlabPlan);
            obj.msglog('Plan %d loaded successfully.', plan_pk);
        end


        function response = savePlan(obj, Args)
            % Saves the current plan to FastAPI plans_manager (POST /save_plan).
            % Sends plan struct with MATLAB->API field mapping; response.data is pk.
            arguments
                obj
                Args.forceSave (1,1) logical = false
            end
            obj.msglog('savePlan: Saving plan with pk=%d', obj.PlanData.pk);

            obj.updateFromPlanner();
            planStruct = obj.PlanData.toStruct();
            planStruct = rmfield(planStruct, 'planner');  % Do not send MATLAB object
            apiPlan = obj.planStructToApi(planStruct);

            params = struct('plan', apiPlan);
            try
                response = obj.Client.postRequest('/save_plan', params);
            catch ME
                obj.msglog('savePlan failed: %s', ME.message);
                response = struct('status', 'error', 'message', ME.message, 'data', [], 'ok', false);
                return;
            end

            response.ok = isfield(response, 'status') && strcmp(response.status, 'ok');
            if response.ok && isfield(response, 'data') && ~isempty(response.data)
                obj.PlanData.pk = response.data;
                if ~isempty(obj.PlanData.planner)
                    obj.PlanData.planner.Pk = response.data;
                end
                obj.msglog('Plan %d saved successfully.', obj.PlanData.pk);
            elseif ~response.ok
                obj.msglog('Failed to save plan: %s', getfield(response, 'message', ''));
            end
        end


        function response = deletePlan(obj, plan_pk)
            % Deletes a plan by calling POST /delete_plan with plan_pk.
            obj.msglog('deletePlan: Deleting plan with pk=%d', plan_pk);
            try
                params = struct('plan_pk', plan_pk);
                response = obj.Client.postRequest('/delete_plan', params);
                response.ok = isfield(response, 'status') && strcmp(response.status, 'ok');
                if response.ok
                    obj.msglog('Plan %d deleted successfully.', plan_pk);
                else
                    obj.msglog('Failed to delete plan %d: %s', plan_pk, getfield(response, 'message', ''));
                end
            catch ME
                obj.msglog('deletePlan failed: %s', ME.message);
                response = struct('status', 'error', 'message', ME.message, 'ok', false);
            end
        end


        function response = getPlanStatus(obj, plan_pk)
            % Not supported when using Plans Manager API (no get_plan_status endpoint).
            obj.msglog('getPlanStatus: Not supported when using Plans Manager API');
            response = struct('status', 'error', 'message', 'Not supported when using Plans Manager API', 'data', [], 'ok', false);
        end

        % -----------------------------------------------------------------
        %                     Health
        % -----------------------------------------------------------------

        function response = health(obj)
            % Calls GET /health on the plans_manager API (no auth headers).
            % Returns response with .status (e.g. 'ok') and .ok (true if healthy).
            obj.msglog('health: GET /health');
            try
                response = obj.Client.getRequest('/health', false);
                response.ok = isfield(response, 'status') && strcmp(response.status, 'ok');
            catch ME
                obj.msglog('health failed: %s', ME.message);
                response = struct('status', 'error', 'message', ME.message, 'ok', false);
            end
        end

        % =================================================================
        %                     Helper methods
        % =================================================================

        function submitData = convertPlanTableToSubmitData(obj, Plan)
            % Converts the uplanner.Plan table to a list of structs for submission.
            %
            % Parameters:
            %   Plan - MATLAB table containing observation plan data.
            %
            % Returns:
            %   submitData - Array of structs with only the required fields for submission.
            %
            % Notes:
            %   This method extracts specific fields needed for the submission API and
            %   performs necessary data type conversions.

            % Initialize the output
            submitData = [];

            % Loop through each row of the table
            for i = 1:height(Plan)
                % Extract required fields and store in a struct
                rowStruct = struct(...
                    'coord_ra', Plan.RA(i), ...
                    'coord_dec', Plan.Dec(i), ...
                    'tiles', Plan.Tiles(i), ...
                    'exposure', seconds(Plan.ExpTime(i)), ... % Convert duration to seconds
                    'image_count', Plan.Nexposures(i), ...
                    'start_time', datestr(Plan.Tstart(i), 'yyyy-mm-ddTHH:MM:SS.FFFZ') ...
                );
                % Append to the list
                submitData = [submitData; rowStruct];
            end
        end


        function Plan = convertPlanTimesToUtc(obj, Plan)
            % Converts start_time and end_time fields of each plan entry to UTC.
            %
            % Parameters:
            %   Plan - Array of structs containing plan data with time fields
            %
            % Returns:
            %   Plan - Same array with time fields converted to UTC format
            %
            % Notes:
            %   This is used to ensure consistent time format for API communication
            for i = 1:numel(Plan)
                % Convert start_time if it's a datetime
                if isfield(Plan(i), 'start_time') && isdatetime(Plan(i).start_time)
                    Plan(i).start_time = datestr(Plan(i).start_time, 'yyyy-mm-ddTHH:MM:SS.FFFZ');
                end

                % Convert end_time if it exists and is a datetime
                if isfield(Plan(i), 'end_time') && isdatetime(Plan(i).end_time)
                    Plan(i).end_time = datestr(Plan(i).end_time, 'yyyy-mm-ddTHH:MM:SS.FFFZ');
                end

                % Convert estimated_end_time if it exists and is a datetime
                if isfield(Plan(i), 'estimated_end_time') && isdatetime(Plan(i).estimated_end_time)
                    Plan(i).estimated_end_time = datestr(Plan(i).estimated_end_time, 'yyyy-mm-ddTHH:MM:SS.FFFZ');
                end
            end
        end


        function updateFromPlanner(obj)
            % Update obj.PlanData with data from uplanner, including targets list
            %
            % Notes:
            %   Called by savePlan() to ensure latest data from planner is saved
            if ~isempty(obj.PlanData) && ~isempty(obj.PlanData.planner)
                obj.PlanData.plan_type = obj.PlanData.planner.Type;
                obj.PlanData.ast_planner = obj.PlanData.planner.AstPlanner;
                obj.PlanData.title = obj.PlanData.planner.Title;
                obj.PlanData.start_time = obj.PlanData.planner.StartTime;
                obj.PlanData.end_time = obj.PlanData.planner.EndTime;
                obj.PlanData.targets = obj.PlanData.planner.planTable2struct();

                % MATLAB cannot have array with single struct item, the
                % only solution is to convert the array to cellarray
                if numel(obj.PlanData.targets) == 1
                    obj.PlanData.targets = {obj.PlanData.targets};
                end
            end
        end
    end

    methods (Access = private)
        function apiStruct = planStructToApi(obj, s)
            % Convert MATLAB plan struct to API (Python) field names.
            % create_time -> created_time, update_time -> updated_time, targets Dec -> decl.
            apiStruct = s;
            if isfield(s, 'create_time')
                apiStruct.created_time = s.create_time;
                apiStruct = rmfield(apiStruct, 'create_time');
            end
            if isfield(apiStruct, 'update_time')
                apiStruct.updated_time = apiStruct.update_time;
                apiStruct = rmfield(apiStruct, 'update_time');
            end
            if isfield(apiStruct, 'targets') && ~isempty(apiStruct.targets)
                t = apiStruct.targets;
                if iscell(t)
                    for i = 1:numel(t)
                        if isfield(t{i}, 'Dec')
                            t{i}.decl = t{i}.Dec;
                            t{i} = rmfield(t{i}, 'Dec');
                        end
                    end
                else
                    for i = 1:numel(t)
                        if isfield(t(i), 'Dec')
                            t(i).decl = t(i).Dec;
                            t(i) = rmfield(t(i), 'Dec');
                        end
                    end
                end
                apiStruct.targets = t;
            end
            % Convert datetimes to ISO strings for JSON (recursive)
            apiStruct = ultrasat.api.ModelBase.convertDatetimeToString(apiStruct);
        end

        function matlabStruct = apiToPlanStruct(obj, apiStruct)
            % Convert API (Python) plan struct to MATLAB field names.
            % created_time -> create_time, updated_time -> update_time, targets decl -> Dec.
            matlabStruct = apiStruct;
            if isfield(apiStruct, 'created_time')
                matlabStruct.create_time = apiStruct.created_time;
                matlabStruct = rmfield(matlabStruct, 'created_time');
            end
            if isfield(matlabStruct, 'updated_time')
                matlabStruct.update_time = matlabStruct.updated_time;
                matlabStruct = rmfield(matlabStruct, 'updated_time');
            end
            if isfield(matlabStruct, 'targets') && ~isempty(matlabStruct.targets)
                t = matlabStruct.targets;
                if iscell(t)
                    for i = 1:numel(t)
                        if isfield(t{i}, 'decl')
                            t{i}.Dec = t{i}.decl;
                            t{i} = rmfield(t{i}, 'decl');
                        end
                    end
                else
                    for i = 1:numel(t)
                        if isfield(t(i), 'decl')
                            t(i).Dec = t(i).decl;
                            t(i) = rmfield(t(i), 'decl');
                        end
                    end
                end
                matlabStruct.targets = t;
            end
        end
    end
end
