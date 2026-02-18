%==========================================================================
% Project     : ULTRASAT Observation Planner
% Filename    : ultrasat.api.UserManagerClient.m
% Author      : Chen Tishler
% Created     : 01/12/2024
% Updated     : 18/02/2026
% Description : Client for the User Manager FastAPI service.
%               POST /login, /logout; returns struct from JSON via JsonUtils.
%==========================================================================


classdef UserManagerClient < ultrasat.api.clients.ClientBase
    % Client for the User Manager FastAPI service.
    % Uses ClientBase.postRequest; returns struct from JSON via JsonUtils.json2struct.
    %
    % Typical Usage:
    %   factory = ultrasat.api.clients.ClientFactory();
    %   baseUrl = factory.getServiceBaseUrl('user_manager');
    %   apiKey = factory.getApiKey();
    %   client = ultrasat.api.clients.UserManagerClient(baseUrl, apiKey);
    %   response = client.login('chen', '123', 'OPER');
    %   response = client.logout('chen');


    methods
        function obj = UserManagerClient(BaseUrl)
            % Constructor
            %
            % :param BaseUrl: Base URL of the User Manager API (e.g. from ClientFactory.getServiceBaseUrl('user_manager')).
            % :param ApiKey: API key (optional; defaults to SOC_API_KEY env var).
            if nargin < 2 || isempty(ApiKey)
                ApiKey = getenv('SOC_API_KEY');
            end
            obj@ultrasat.api.clients.ClientBase('BaseUrl', BaseUrl);
            obj.LogPrefix = 'UserManagerClient';
            obj.msglog('UserManagerClient constructor started');
        end

        % -------------------------------------------------------------------

        function response = login(obj, UserName, Password, Namespace)
            % POST /login. Validate credentials; return user info on success.
            %
            % :param UserName: username
            % :param Password: plaintext password
            % :param Namespace: kept for interface compatibility with MainModule; not sent to API.
            % :return: struct with status, message, data, ok (from JSON via JsonUtils.json2struct).
            obj.msglog('login: user=%s', UserName);
            params = struct('username', UserName, 'password', Password);
            response = obj.postRequest('/login', params);
            response.ok = strcmp(response.status, 'ok');
        end

        % -------------------------------------------------------------------

        function response = logout(obj, UserName)
            % POST /logout. Stateless no-op; always returns success.
            %
            % :param UserName: username to log out
            % :return: struct with status, message, ok (from JSON via JsonUtils.json2struct).
            obj.msglog('logout: User %s', UserName);
            params = struct('username', UserName);
            response = obj.postRequest('/logout', params);
            response.ok = strcmp(response.status, 'ok');
        end
    end

end
