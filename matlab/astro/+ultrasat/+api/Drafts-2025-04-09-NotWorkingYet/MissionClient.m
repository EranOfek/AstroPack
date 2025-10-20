%==========================================================================
% ULTRASAT
%
% File:   ultrasat.MissionClient.m
% Author: Chen Tishler
% Created: 01/12/2024
% Updated: 16/02/2025
%
%==========================================================================

classdef MissionClient < ultrasat.api.MissionClientBase
    % Client implementation of the MissionClientBase interface
    % Provides communication with the Mission API server

    properties
        Client          % api.ClientBase instance for HTTP requests
        ApiUrl          % Base URL for API endpoints
    end


    methods
        function obj = MissionClient(Args)
            % Constructor for MissionClient
            %
            % Parameters:
            %   Args.SubUrl (string) - API endpoint path (default: '/mission')
            %   Args.ApiUrl (string) - API base URL (default: from environment)
            %
            % Returns:
            %   obj - Initialized MissionClient object
            arguments
                Args.SubUrl = '/mission';  % planner_backend
                Args.ApiUrl = '';  % Will be fetched from environment if empty
                Args.LogFileName = [];
            end

            % Call parent constructor
            ArgsCell = namedargs2cell(Args);
            obj@ultrasat.api.MissionClientBase(ArgsCell{:});

            % Initialize API client
            obj.Client = api.ClientBase('SubUrl', Args.SubUrl);

            % Set API URL if provided
            if ~isempty(Args.ApiUrl)
                obj.ApiUrl = Args.ApiUrl;
                obj.Client.BaseUrl = Args.ApiUrl;
            end
        end


        % -------------------------------------------------------------------

        function response = login(obj, Params)
            % Login to the mission server
            arguments
                obj
                Params api.ModelBase
            end

            % Create API call
            call = obj.createApiCall('login', Params);

            % Make the call
            response = obj.makeApiCall(call);

            % Update session info if login successful
            if response.ok
                obj.SessionInfo = response.user;
            end
        end


        function response = logout(obj, Params)
            % Logout from the mission server
            arguments
                obj
                Params api.ModelBase
            end

            % Create API call
            call = obj.createApiCall('logout', Params);

            % Make the call
            response = obj.makeApiCall(call);

            % Clear session info if logout successful
            if response.ok
                obj.SessionInfo = [];
            end
        end

        % -------------------------------------------------------------------

        function response = getKeyValue(obj, Params)
            % Get a value from the key-value store
            arguments
                obj
                Params api.ModelBase
            end

            % Create API call
            call = obj.createApiCall('getKeyValue', Params);

            % Make the call
            response = obj.makeApiCall(call);
        end


        function response = setKeyValue(obj, Params)
            % Set a value in the key-value store
            arguments
                obj
                Params api.ModelBase
            end

            % Create API call
            call = obj.createApiCall('setKeyValue', Params);

            % Make the call
            response = obj.makeApiCall(call);
        end

        % -------------------------------------------------------------------

        function response = getApprovedTargets(obj, Params)
            % Get approved observation targets
            arguments
                obj
                Params api.ModelBase
            end

            % Create API call
            call = obj.createApiCall('getApprovedTargets', Params);

            % Make the call
            response = obj.makeApiCall(call);

            % Store time range if successful
            if response.ok
                obj.ApprovedTargetsStartTime = Params.startTime;
                obj.ApprovedTargetsEndTime = Params.endTime;
            end
        end


        function response = validatePlan(obj, Params)
            % Validate a plan
            arguments
                obj
                Params api.ModelBase
            end

            % Create API call
            call = obj.createApiCall('validatePlan', Params);

            % Make the call
            response = obj.makeApiCall(call);
        end


        function response = submitPlan(obj, Params)
            % Submit a plan
            arguments
                obj
                Params api.ModelBase
            end

            % Create API call
            call = obj.createApiCall('submitPlan', Params);

            % Make the call
            response = obj.makeApiCall(call);
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
            % Retrieves a list of all observation plans from the server.
            %
            % Parameters:
            %   start_timestamp (optional) - Start time for filtering plans
            %   end_timestamp (optional) - End time for filtering plans
            %   title_subtext (optional) - Substring to search in plan titles
            %
            % Returns:
            %   response - Structure containing result
            obj.msglog('getPlansList: Scanning for plans');

            % Handle optional arguments
            if nargin < 2
                start_timestamp = [];
            end
            if nargin < 3
                end_timestamp = [];
            end
            if nargin < 4
                title_subtext = '';
            end

            % Format dates for API if present
            if ~isempty(start_timestamp) && isdatetime(start_timestamp)
                start_str = datestr(start_timestamp, 'yyyy-mm-ddTHH:MM:SS.FFFZ');
            else
                start_str = start_timestamp;
            end

            if ~isempty(end_timestamp) && isdatetime(end_timestamp)
                end_str = datestr(end_timestamp, 'yyyy-mm-ddTHH:MM:SS.FFFZ');
            else
                end_str = end_timestamp;
            end

            % Send request
            params = struct(...
                'start_timestamp', start_str, ...
                'end_timestamp', end_str, ...
                'title_subtext', title_subtext ...
            );

            response = obj.Client.postRequest('/get_plans_list/', params);
            response.ok = isfield(response, 'status') && strcmp(response.status, 'ok');
        end


        function response = loadPlan(obj, plan_pk)
            % Loads a specific observation plan by its primary key.
            %
            % Parameters:
            %   plan_pk (integer) - Primary key of the plan to load
            %
            % Returns:
            %   response - Structure containing result
            %
            % Notes:
            %   This method populates the obj.PlanData property.
            obj.msglog('loadPlan: Loading plan with pk=%d', plan_pk);

            % Send request
            params = struct('plan_pk', plan_pk);
            response = obj.Client.postRequest('/load_plan/', params);

            % Update response.ok based on status
            response.ok = isfield(response, 'status') && strcmp(response.status, 'ok');

            % Populate PlanData if plan was returned
            if response.ok && isfield(response, 'plan')
                obj.PlanData = ultrasat.api.PlanData.fromStruct(response.plan);
                obj.msglog('Plan %d loaded successfully.', plan_pk);
            else
                obj.msglog('Failed to load plan %d: %s', plan_pk, response.message);
            end
        end


        function response = savePlan(obj)
            % Saves the current observation plan (from obj.PlanData) to the server.
            %
            % Returns:
            %   response - Structure containing result
            obj.msglog('savePlan: Saving plan with pk=%d', obj.PlanData.pk);

            % Update planData from planner if available
            obj.updateFromPlanner();

            % Send request (empty params since PlanData is on server)
            params = struct();
            response = obj.Client.postRequest('/save_plan/', params);

            % Update response.ok based on status
            response.ok = isfield(response, 'status') && strcmp(response.status, 'ok');

            if response.ok
                obj.msglog('Plan %d saved successfully.', obj.PlanData.pk);
            else
                obj.msglog('Failed to save plan: %s', response.message);
            end
        end


        function response = deletePlan(obj, plan_pk)
            % Deletes a specific observation plan by its primary key.
            %
            % Parameters:
            %   plan_pk (integer) - Primary key of the plan to delete
            %
            % Returns:
            %   response - Structure containing result
            obj.msglog('deletePlan: Deleting plan with pk=%d', plan_pk);

            % Send request
            params = struct('plan_pk', plan_pk);
            response = obj.Client.postRequest('/delete_plan/', params);

            % Update response.ok based on status
            response.ok = isfield(response, 'status') && strcmp(response.status, 'ok');

            if response.ok
                obj.msglog('Plan %d deleted successfully.', plan_pk);
            else
                obj.msglog('Failed to delete plan %d: %s', plan_pk, response.message);
            end
        end


        function response = getPlanStatus(obj, plan_pk)
            % Retrieves the current status of a specific observation plan.
            %
            % Parameters:
            %   plan_pk (integer) - Primary key of the plan to check
            %
            % Returns:
            %   response - Structure containing result
            obj.msglog('getPlanStatus: Fetching status for plan with pk=%d', plan_pk);

            % Send request
            params = struct('plan_pk', plan_pk);
            response = obj.Client.postRequest('/get_plan_status/', params);

            % Update response.ok based on status
            response.ok = isfield(response, 'status') && strcmp(response.status, 'ok');

            if response.ok
                obj.msglog('Plan status fetched successfully for pk=%d', plan_pk);
            else
                obj.msglog('Failed to fetch plan status: %s', response.message);
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
end
