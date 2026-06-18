%==========================================================================
% Project     : ULTRASAT Observation Planner
% Filename    : +debug/+ultrasat/+api/+clients/debug_ScheduleManagerClient.m
% Author      : Chen Tishler
% Created     : 18/02/2026
% Updated     : 06/05/2026
% Description : Debug function for ScheduleManagerClient.
%               Uses ClientFactory for baseUrl and apiKey.
%
% Run by      : debug.ultrasat.api.clients.debug_ScheduleManagerClient()
%==========================================================================

function debug_ScheduleManagerClient(Namespace, Limit)
    % Debug ScheduleManagerClient.getTargets().
    %
    % Usage:
    %   debug.ultrasat.api.clients.debug_ScheduleManagerClient()
    %   debug.ultrasat.api.clients.debug_ScheduleManagerClient('dev', 100)
    arguments
        Namespace = 'dev'
        Limit = []
    end
    factory = ultrasat.api.clients.ClientFactory();
    baseUrl = factory.getServiceBaseUrl('schedule_manager');
    client = ultrasat.api.clients.ScheduleManagerClient(baseUrl);
    client.Namespace = Namespace;

    hc = client.healthCheck();
    fprintf('healthCheck: %s\n', string(hc));

    % Get the list of targets
    fprintf('Testing getTargets() with default range (2020-01-01 to 2040-12-31)...\n');
    response = client.getTargets([], [], Limit);

    if isfield(response, 'targets') && ~isempty(response.targets)
        fprintf('\nTargets count: %d\n', numel(response.targets));
        for i = 1:numel(response.targets)
            tgt = response.targets(i);
            if isfield(tgt, 'target_id'); tid = string(tgt.target_id); else; tid = "<no_target_id>"; end
            if isfield(tgt, 'name'); nm = string(tgt.name); else; nm = "<no_name>"; end
            if isfield(tgt, 'start_time'); st = string(tgt.start_time); else; st = "<no_start_time>"; end
            if isfield(tgt, 'end_time'); en = string(tgt.end_time); else; en = "<no_end_time>"; end
            fprintf('  target: %s %s %s %s\n', tid, nm, st, en);
        end
    else
        disp(response);
    end
end
