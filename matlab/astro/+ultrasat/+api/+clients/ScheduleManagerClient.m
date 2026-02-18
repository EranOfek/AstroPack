%==========================================================================
% Project     : ULTRASAT Observation Planner
% Filename    : ultrasat.api.ScheduleManagerClient.m
% Author      : Chen Tishler
% Created     : 18/02/2026
% Updated     : 18/02/2026
% Description : Client for the Schedule Manager FastAPI service.
%               POST /get-targets; returns struct from JSON via JsonUtils.
%==========================================================================


classdef ScheduleManagerClient < ultrasat.api.clients.ClientBase
    % Client for the Schedule Manager FastAPI service.
    % Uses ClientBase.postRequest; returns struct from JSON via JsonUtils.json2struct.
    % Requires namespace header.
    %
    % Typical Usage:
    %   factory = ultrasat.api.clients.ClientFactory();
    %   baseUrl = factory.getServiceBaseUrl('schedule_manager');
    %   apiKey = factory.getApiKey();
    %   client = ultrasat.api.clients.ScheduleManagerClient(baseUrl, 'OPER', apiKey);
    %   response = client.getTargets();


    methods
        function obj = ScheduleManagerClient(BaseUrl, Namespace, ApiKey)
            % Constructor
            %
            % :param BaseUrl: Base URL of the Schedule Manager API (e.g. from ClientFactory.getServiceBaseUrl('schedule_manager')).
            % :param Namespace: Namespace header value (e.g. 'OPER').
            % :param ApiKey: API key (optional; defaults to SOC_API_KEY env var).
            if nargin < 3 || isempty(ApiKey)
                ApiKey = getenv('SOC_API_KEY');
            end
            obj@ultrasat.api.clients.ClientBase(...
                'BaseUrl', BaseUrl, ...
                'ApiKey', ApiKey, ...
                'Namespace', Namespace, ...
                'Timeout', 30);
            obj.LogPrefix = 'ScheduleManagerClient';
            obj.msglog('ScheduleManagerClient constructor started');
        end

        % -------------------------------------------------------------------

        function response = getTargets(obj, Start, End, Limit)
            % POST /get-targets. Query scheduled targets by time range.
            %
            % :param Start: range start (datetime, optional). If empty, uses 2020-01-01 00:00:00 UTC.
            % :param End: range end (datetime, optional). If empty, uses 2040-12-31 23:59:59 UTC.
            % :param Limit: optional max rows to return.
            % :return: struct with targets (from JSON via JsonUtils.json2struct).
            if nargin < 2 || isempty(Start)
                Start = datetime(2020, 1, 1, 'TimeZone', 'UTC');
            end
            if nargin < 3 || isempty(End)
                End = datetime(2040, 12, 31, 23, 59, 59, 'TimeZone', 'UTC');
            end
            if nargin < 4
                Limit = [];
            end
            obj.msglog('getTargets: start=%s end=%s', char(Start), char(End));
            params = struct('start_time', Start, 'end_time', End);
            if ~isempty(Limit)
                params.limit = Limit;
            end
            response = obj.postRequest('/get-targets', params);
        end
    end

end
