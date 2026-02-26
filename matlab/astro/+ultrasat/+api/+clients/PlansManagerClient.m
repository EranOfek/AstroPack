%==========================================================================
% Project     : ULTRASAT Observation Planner
% Filename    : ultrasat.api.PlansManagerClient.m
% Author      : Chen Tishler
% Created     : 18/02/2026
% Updated     : 23/02/2026
% Description : Client for the Plans Manager FastAPI service.
%               POST /get-plans, /get-plan, /save-plan, /get-matlab-mat, /save-matlab-mat.
%==========================================================================


classdef PlansManagerClient < ultrasat.api.clients.ClientBase
    % Client for the Plans Manager FastAPI service.
    % Uses ClientBase.postRequest; returns struct from JSON via JsonUtils.json2struct.
    % Requires namespace header.
    %
    % Typical Usage:
    %   factory = ultrasat.api.clients.ClientFactory();
    %   baseUrl = factory.getServiceBaseUrl('plans_manager');
    %   client = ultrasat.api.clients.PlansManagerClient(baseUrl);
    %   obj.setNamespace('dev');  % or set client.Namespace before calls
    %   response = client.getPlansList([], [], [], []);


    methods
        function obj = PlansManagerClient(BaseUrl)
            % Constructor
            %
            % :param BaseUrl: Base URL of the Plans Manager API (e.g. from ClientFactory.getServiceBaseUrl('plans_manager')).
            obj@ultrasat.api.clients.ClientBase('BaseUrl', BaseUrl);
            obj.LogPrefix = 'PlansManagerClient';
            obj.msglog('PlansManagerClient constructor started');
        end

        % -------------------------------------------------------------------

        function response = getPlansList(obj, StartTime, EndTime, Status, Mode)
            % POST /get-plans. Returns list of plans.
            %
            % :param StartTime: optional range start (datetime).
            % :param EndTime: optional range end (datetime).
            % :param Status: optional status filter.
            % :param Mode: optional mode ('submitted' | 'history' | custom).
            % :return: struct with plans, status, ok.
            if nargin < 2, StartTime = []; end
            if nargin < 3, EndTime = []; end
            if nargin < 4, Status = []; end
            if nargin < 5, Mode = []; end
            obj.msglog('getPlansList');
            params = struct();
            if ~isempty(StartTime), params.start_time = StartTime; end
            if ~isempty(EndTime), params.end_time = EndTime; end
            if ~isempty(Status), params.status = Status; end
            if ~isempty(Mode), params.mode = Mode; end
            response = obj.postRequest('/get-plans', params);
            response.ok = strcmp(response.status, 'ok');
        end

        % -------------------------------------------------------------------

        function response = getPlan(obj, PlanPk)
            % POST /get-plan. Fetch a single plan by primary key.
            %
            % :param PlanPk: plan primary key.
            % :return: struct with data (plan), status, ok.
            obj.msglog('getPlan: pk=%d', PlanPk);
            params = struct('plan_pk', PlanPk);
            response = obj.postRequest('/get-plan', params);
            response.ok = strcmp(response.status, 'ok');
        end

        % -------------------------------------------------------------------

        function response = savePlan(obj, PlanStruct)
            % POST /save-plan. Insert or update a plan.
            %
            % :param PlanStruct: plan struct (MATLAB format; converted to API format internally).
            % :return: struct with data (saved pk), status, ok.
            obj.msglog('savePlan');
            apiPlan = obj.planStructToApi(PlanStruct);
            params = struct('plan', apiPlan);
            response = obj.postRequest('/save-plan', params);
            response.ok = strcmp(response.status, 'ok');
        end

        % -------------------------------------------------------------------

        function response = getMatlabMat(obj, PlanPk)
            % POST /get-matlab-mat. Get matlab_mat (base64) for a plan.
            %
            % :param PlanPk: plan primary key.
            % :return: struct with data (base64 str or empty), status, ok.
            obj.msglog('getMatlabMat: pk=%d', PlanPk);
            params = struct('plan_pk', PlanPk);
            response = obj.postRequest('/get-matlab-mat', params);
            response.ok = strcmp(response.status, 'ok');
        end

        % -------------------------------------------------------------------

        function response = saveMatlabMat(obj, PlanPk, MatlabMatBase64)
            % POST /save-matlab-mat. Save matlab_mat (base64) for a plan.
            %
            % :param PlanPk: plan primary key.
            % :param MatlabMatBase64: base64-encoded binary data.
            % :return: struct with data, status, ok.
            obj.msglog('saveMatlabMat: pk=%d', PlanPk);
            params = struct('plan_pk', PlanPk, 'matlab_mat', MatlabMatBase64);
            response = obj.postRequest('/save-matlab-mat', params);
            response.ok = strcmp(response.status, 'ok');
        end
    end

    methods (Access = private)
        function apiStruct = planStructToApi(obj, s)
            % Convert MATLAB plan struct to API (Python PlanData/PlanTarget) format.
            apiStruct = s;

            % Remove planner field - we do not send the planner object to the backend
            % with save-plan endpoint, there is separate save-matlab-mat call
            if isfield(apiStruct, 'planner')
                apiStruct = rmfield(apiStruct, 'planner');
            end

            % PlanData-level type coercion for API compatibility
            if isfield(apiStruct, 'allow_edit') && ~islogical(apiStruct.allow_edit)
                apiStruct.allow_edit = logical(apiStruct.allow_edit);
            end
            if isfield(apiStruct, 'deleted') && ~islogical(apiStruct.deleted)
                apiStruct.deleted = logical(apiStruct.deleted);
            end

            % Convert targets to API PlanTarget format
            if isfield(apiStruct, 'targets') && ~isempty(apiStruct.targets)

                % Struct arrays only encode as JSON arrays when they have more than one element. 
                % Cell arrays always encode as JSON arrays regardless of size.
                t = num2cell(apiStruct.targets);

                useCell = iscell(t);
                for i = 1:numel(t)
                    if useCell
                        t{i} = obj.convertTargetToApi(t{i});
                    else
                        t(i) = obj.convertTargetToApi(t(i));
                    end
                end
                apiStruct.targets = t;
            end

            apiStruct = ultrasat.api.utils.DateTimeUtils.convertDatetimeToString(apiStruct);
        end


        function t = convertTargetToApi(obj, t)
            % Convert single target struct to API PlanTarget format.
            % Handles duration->int seconds, slew_time_before->slew_before, type coercion.
            %
            % :param t: Single target struct (from planTable2struct or similar).
            % :return: Target struct with API-compatible field types and names.
            if isfield(t, 'exposure')
                if isduration(t.exposure)
                    t.exposure = round(seconds(t.exposure));
                else
                    t.exposure = round(double(t.exposure));
                end
            end
            if isfield(t, 'total_seconds')
                if isduration(t.total_seconds)
                    t.total_seconds = int32(round(seconds(t.total_seconds)));
                else
                    t.total_seconds = int32(round(double(t.total_seconds)));
                end
            end
            if isfield(t, 'total_duration') && isduration(t.total_duration)
                t.total_duration = round(seconds(t.total_duration));
            end
            if isfield(t, 'slew_time_before')
                if isduration(t.slew_time_before)
                    t.slew_before = round(seconds(t.slew_time_before));
                else
                    t.slew_before = round(double(t.slew_time_before));
                end
                t = rmfield(t, 'slew_time_before');
            end

            % Coerce float fields (ra, decl, roll) to double
            if isfield(t, 'ra'), t.ra = double(t.ra); end
            if isfield(t, 'decl'), t.decl = double(t.decl); end
            if isfield(t, 'roll'), t.roll = double(t.roll); end
            % Coerce int fields (API expects int)
            if isfield(t, 'image_count'), t.image_count = int32(round(double(t.image_count))); end
            if isfield(t, 'exposure'), t.exposure = int32(round(double(t.exposure))); end
        end
    end

end
