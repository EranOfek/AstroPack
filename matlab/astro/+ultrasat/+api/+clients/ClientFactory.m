%==========================================================================
% Project     : ULTRASAT Observation Planner
% File        : ultrasat.api.ClientFactory.m
% Author      : Chen Tishler
% Created     : 18/02/2026
% Updated     : 18/02/2026
% Description : Client factory that loads SOC config/services.json once and
%               returns service base URLs (direct/nginx) + API key.
%==========================================================================

classdef ClientFactory < handle

    properties (Access=private)
        ServicesCfg = []   % cached struct from services.json
        ApiKey = ''        % cached SOC_API_KEY
    end

    methods

        function obj = ClientFactory()
            % Constructor: lazy-load (nothing here on purpose)
        end

        % ---------------------------------------------------------------

        function cfg = loadServicesCfg(obj)
            % Load and cache services.json (once)
            if ~isempty(obj.ServicesCfg)
                cfg = obj.ServicesCfg;
                return;
            end

            socPath = getenv('SOC_PATH');
            if isempty(socPath)
                error('ClientFactory:MissingEnv', 'SOC_PATH environment variable is not set');
            end

            cfgPath = fullfile(socPath, 'config', 'services.json');
            if ~exist(cfgPath, 'file')
                error('ClientFactory:MissingFile', 'Services configuration file not found at %s', cfgPath);
            end

            try
                txt = fileread(cfgPath);
                obj.ServicesCfg = jsondecode(txt);
            catch ME
                error('ClientFactory:BadJson', 'Failed to read/parse %s: %s', cfgPath, ME.message);
            end

            cfg = obj.ServicesCfg;
        end

        % ---------------------------------------------------------------

        function apiKey = getApiKey(obj)
            % Get and cache API key (once)
            if ~isempty(obj.ApiKey)
                apiKey = obj.ApiKey;
                return;
            end

            obj.ApiKey = getenv('SOC_API_KEY');
            if isempty(obj.ApiKey)
                error('ClientFactory:MissingEnv', 'SOC_API_KEY environment variable is not set');
            end
            apiKey = obj.ApiKey;
        end

        % ---------------------------------------------------------------

        function baseUrl = getServiceBaseUrl(obj, serviceName, mode)
            % Get base URL for a given service (direct/nginx)
            %
            % :param serviceName: e.g. 'plans_manager', 'schedule_manager'
            % :param mode: 'direct' or 'nginx' (optional). If omitted, uses cfg.mode

            if nargin < 2 || isempty(serviceName)
                error('ClientFactory:BadArgs', 'Service name is required');
            end

            cfg = obj.loadServicesCfg();

            if nargin < 3 || isempty(mode)
                if isfield(cfg, 'mode') && ~isempty(cfg.mode)
                    mode = cfg.mode;
                else
                    mode = 'direct'; % safe default if not present
                end
            end

            mode = char(string(mode));
            if ~ismember(mode, {'direct','nginx'})
                error('ClientFactory:BadMode', 'Invalid mode: must be ''direct'' or ''nginx'', got: %s', mode);
            end

            if strcmp(mode, 'nginx')
                services = cfg.services_nginx;
            else
                services = cfg.services_direct;
            end

            if ~isfield(services, serviceName)
                error('ClientFactory:UnknownService', 'Service "%s" not found in services configuration', serviceName);
            end

            val = services.(serviceName);

            if strcmp(mode, 'nginx')
                if ~isfield(cfg, 'base_api_url') || isempty(cfg.base_api_url)
                    error('ClientFactory:MissingCfg', 'cfg.base_api_url is missing (required for nginx mode)');
                end

                % Remove trailing '/' from base_api_url if exists
                baseApiUrl = char(string(cfg.base_api_url));
                if endsWith(baseApiUrl, '/')
                    baseApiUrl = baseApiUrl(1:end-1);
                end
                baseUrl = [baseApiUrl, char(string(val))];
            else
                % Remove trailing '/' from service url
                baseUrl = char(string(val));
                if endsWith(baseUrl, '/')
                    baseUrl = baseUrl(1:end-1);
                end
            end
        end
        
    end
end
