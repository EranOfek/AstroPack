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
    client = ultrasat.api.clients.ScheduleManagerClient(baseUrl);
    client.Namespace = 'dev';

    hc = client.healthCheck();
    fprintf('healthCheck: %s\n', string(hc));

    % Get the list of targets
    fprintf('Testing getTargets() with default range (2020-01-01 to 2040-12-31)...\n');
    response = client.getTargets();
    disp(response);

    if isfield(response, 'targets') && ~isempty(response.targets)
        fprintf('\nTargets count: %d\n', numel(response.targets));

        % Print all targets
        for i = 1:numel(response.targets)
            tgt = response.targets(i);
            fprintf('Target %d:\n', i);
            disp(tgt);
        end
    end
end
