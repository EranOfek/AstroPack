%==========================================================================
% Project     : ULTRASAT Observation Planner
% File        : ultrasat.api.debug.clients.debug_ScheduleManagerClient.m
% Author      : Chen Tishler
% Created     : 18/02/2026
% Updated     : 18/02/2026
% Description : Debug function for ScheduleManagerClient.
%               Uses ClientFactory for baseUrl and apiKey.
%==========================================================================

function debug_ScheduleManagerClient()
    factory = ultrasat.api.clients.ClientFactory();
    baseUrl = factory.getServiceBaseUrl('schedule_manager');
    apiKey = factory.getApiKey();
    namespace = 'OPER';
    client = ultrasat.api.clients.ScheduleManagerClient(baseUrl, namespace, apiKey);

    fprintf('Testing getTargets() with default range (2020-01-01 to 2040-12-31)...\n');
    response = client.getTargets();
    disp(response);

    if isfield(response, 'targets') && ~isempty(response.targets)
        fprintf('\nTargets count: %d\n', numel(response.targets));
    end
end
