%==========================================================================
% Project     : ULTRASAT Observation Planner
% File        : ultrasat.api.debug.clients.debug_ClientFactory.m
% Author      : Chen Tishler
% Created     : 18/02/2026
% Updated     : 18/02/2026
% Description : Debug function for ultrasat.api.clients.ClientFactory class.
%==========================================================================

function debug_ClientFactory()
    % debug_ClientFactory - Minimal sanity check for ClientFactory
    %
    % Checks:
    %   1. SOC_PATH is set
    %   2. services.json is readable
    %   3. API key is accessible
    %   4. service base URL resolution works
    
    fprintf('ClientFactory sanity check\n');


    % Create factory
    factory = ultrasat.api.clients.ClientFactory();

    % Test API key
    apiKey = factory.getApiKey();
    fprintf('API key loaded: %s\n', apiKey);

    % Test service URL (change service name if needed)

    % Check several services: 'namespace_manager', 'user_manager', 'plans_manager', 'schedule_manager'
    serviceNames = {'namespace_manager', 'user_manager', 'plans_manager', 'schedule_manager'};
    for i = 1:numel(serviceNames)
        url = factory.getServiceBaseUrl(serviceNames{i});
        fprintf('Service "%s" URL resolved: %s\n', serviceNames{i}, url);
    end

end
