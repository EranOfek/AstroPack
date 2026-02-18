%==========================================================================
% Project     : ULTRASAT Observation Planner
% Filename    : ultrasat.api.NamespaceManagerClient.m
% Author      : Chen Tishler
% Created     : 18/02/2026
% Updated     : 18/02/2026
% Description : Client for the Namespace Manager FastAPI service.
%==========================================================================


classdef NamespaceManagerClient < ultrasat.api.clients.ClientBase
    % Client for the Namespace Manager FastAPI service.
    % Uses ClientBase.postRequest; returns struct from JSON via JsonUtils.json2struct.
    %
    % Typical Usage:
    %   factory = ultrasat.api.clients.ClientFactory();
    %   baseUrl = factory.getServiceBaseUrl('namespace_manager');
    %   apiKey = factory.getApiKey();
    %   client = ultrasat.api.clients.NamespaceManagerClient(baseUrl, apiKey);
    %   response = client.getNamespaceList();


    methods
        function obj = NamespaceManagerClient(BaseUrl)
            % Constructor
            %
            % :param BaseUrl: Base URL of the Namespace Manager API (e.g. from ClientFactory.getServiceBaseUrl('namespace_manager')).
            obj@ultrasat.api.clients.ClientBase('BaseUrl', BaseUrl);
            obj.LogPrefix = 'NamespaceManagerClient';
            obj.msglog('NamespaceManagerClient constructor started');
        end

        % -------------------------------------------------------------------

        function response = getNamespaceList(obj)
            % POST /get-namespaces. List namespaces, optionally filtered by is_active.
            %
            % :return: struct with status, message, namespaces (from JSON via JsonUtils.json2struct).
            obj.msglog('getNamespaceList: Getting list of namespaces');
            params = struct();
            response = obj.postRequest('/get-namespaces', params);
        end
    end

end
