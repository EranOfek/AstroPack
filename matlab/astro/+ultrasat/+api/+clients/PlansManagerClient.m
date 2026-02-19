%==========================================================================
% Project     : ULTRASAT Observation Planner
% Filename    : ultrasat.api.PlansManagerClient.m
% Author      : Chen Tishler
% Created     : 18/02/2026
% Updated     : 18/02/2026
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
            params = ultrasat.api.utils.JsonUtils.removeEmptyFields(params);
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
            % Convert MATLAB plan struct to API (Python) field names.
            apiStruct = s;
            if isfield(apiStruct, 'planner')
                apiStruct = rmfield(apiStruct, 'planner');
            end
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
            apiStruct = ultrasat.api.utils.DateTimeUtils.convertDatetimeToString(apiStruct);
        end
    end

end
