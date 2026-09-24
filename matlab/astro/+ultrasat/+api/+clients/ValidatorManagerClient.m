%==========================================================================
% Project     : ULTRASAT Observation Planner
% Filename    : ultrasat.api.ValidatorManagerClient.m
% Author      : Chen Tishler
% Created     : 19/02/2026
% Updated     : 19/02/2026
% Description : Client for the Validator Manager FastAPI service.
%               Sends list of targets for validation. Replaces ValidatorSim
%               when connecting to real validator backend.
%==========================================================================
%
% TODO: Confirm backend endpoint path (e.g. /validate-targets, /validate_targets)
%       and request body key (targets vs plan). Adjust if backend differs.
% TODO: Confirm service name in services.json (validator_manager vs validators_manager).
%

classdef ValidatorManagerClient < ultrasat.api.clients.ClientBase
    % Client for the Validator Manager FastAPI service.
    % Uses ClientBase.postRequest; returns struct from JSON via JsonUtils.json2struct.
    % Response shape matches ValidatorSim for drop-in replacement.
    %
    % Typical Usage:
    %   factory = ultrasat.api.clients.ClientFactory();
    %   baseUrl = factory.getServiceBaseUrl('validator_manager');
    %   client = ultrasat.api.clients.ValidatorManagerClient(baseUrl);
    %   client.Namespace = 'dev';  % if backend requires namespace
    %   response = client.validateTargets(targets);


    methods
        function obj = ValidatorManagerClient(BaseUrl)
            % Constructor
            %
            % :param BaseUrl: Base URL of the Validator Manager API
            %   (e.g. from ClientFactory.getServiceBaseUrl('validator_manager')).
            obj@ultrasat.api.clients.ClientBase('BaseUrl', BaseUrl);
            obj.LogPrefix = 'ValidatorManagerClient';
            obj.msglog('ValidatorManagerClient constructor started');
        end

        % -------------------------------------------------------------------

        function Result = validateTargets(obj, targets)
            % POST /validate-targets. Sends targets to validator backend.
            %
            % Mirrors ValidatorSim.validateTargets interface for drop-in replacement.
            %
            % :param targets: Array of target structs (coord_ra, coord_dec, tiles,
            %   exposure, image_count, start_time, etc.). Same format as ValidatorSim.
            % :return: struct with validation_time, status, task (task_id,
            %   start_time, estimated_end_time, targets, ...). Same shape as ValidatorSim.
            obj.msglog('validateTargets: %d targets', numel(targets));

            try
                % Convert targets to API format (datetime -> ISO string, duration -> seconds)
                apiTargets = obj.targetsToApi(targets);
                params = struct('targets', apiTargets);

                response = obj.postRequest('/validate-targets', params);

                % Ensure response.ok for compatibility with MissionApiSim.validatePlan
                if isfield(response, 'status')
                    response.ok = strcmp(response.status, 'approved') || strcmp(response.status, 'ok');
                else
                    response.ok = false;
                end

                Result = response;
                obj.msglog('Validation completed: status=%s', Result.status);
            catch ME
                obj.msglog('validateTargets error: %s', ME.message);
                Result = struct(...
                    'validation_time', ultrasat.api.utils.DateTimeUtils.nowUtc(), ...
                    'status', 'error', ...
                    'message', ME.message, ...
                    'task', struct(), ...
                    'ok', false);
            end
        end
    end

    methods (Access = private)
        function apiTargets = targetsToApi(obj, targets)
            % Convert MATLAB target structs to API format.
            % - Datetime fields -> ISO strings
            % - exposure (duration) -> seconds (numeric)
            apiTargets = targets;
            for i = 1:numel(apiTargets)
                t = apiTargets(i);
                if isfield(t, 'exposure') && isduration(t.exposure)
                    t.exposure = seconds(t.exposure);
                end
                if isfield(t, 'total_duration') && isduration(t.total_duration)
                    t.total_duration = seconds(t.total_duration);
                end
                if isfield(t, 'slew_time_before') && isduration(t.slew_time_before)
                    t.slew_time_before = seconds(t.slew_time_before);
                end
                apiTargets(i) = t;
            end
            apiTargets = ultrasat.api.utils.DateTimeUtils.convertDatetimeToString(apiTargets);
        end
    end

end
