%==========================================================================
% ULTRASAT
%
% File:        debug_ValidatorManagerClient.m
% Author:      Chen Tishler
% Created:     19/02/2026
% Updated:     19/02/2026
% Description: Debug function for ValidatorManagerClient.
%              Uses ClientFactory for baseUrl. Requires validator_manager
%              in services.json and backend running.
% Run by:      ultrasat.api.debug.clients.debug_ValidatorManagerClient()
%==========================================================================

function debug_ValidatorManagerClient()
    fprintf('========== DEBUG VALIDATOR MANAGER CLIENT ==========\n');

    try
        factory = ultrasat.api.clients.ClientFactory();
        baseUrl = factory.getServiceBaseUrl('validator_manager');
    catch ME
        fprintf('Failed to get validator_manager URL (check services.json): %s\n', ME.message);
        fprintf('Falling back to ValidatorSim for local testing...\n');
        debug_validateTargetsWithSim();
        return;
    end

    client = ultrasat.api.clients.ValidatorManagerClient(baseUrl);
    client.Namespace = 'dev';

    debug_healthCheck(client);
    debug_validateTargets(client);

    fprintf('========== DEBUG VALIDATOR MANAGER CLIENT DONE ==========\n');
end


function debug_healthCheck(client)
    fprintf('\n--- debug_healthCheck ---\n');
    try
        result = client.healthCheck();
        fprintf('healthCheck: %s\n', string(result));
    catch ME
        fprintf('healthCheck failed: %s\n', ME.message);
    end
end


function debug_validateTargets(client)
    fprintf('\n--- debug_validateTargets ---\n');
    targets = createSampleTargets();
    try
        response = client.validateTargets(targets);
        fprintf('ok=%d, status=%s\n', response.ok, debug_getStatus(response));
        if isfield(response, 'task')
            fprintf('task_id=%s, target_count=%d\n', ...
                debug_getField(response.task, 'task_id', ''), ...
                debug_getField(response.task, 'target_count', 0));
        end
        if isfield(response, 'message') && ~isempty(response.message)
            fprintf('message: %s\n', response.message);
        end
    catch ME
        fprintf('validateTargets failed: %s\n', ME.message);
    end
end


function debug_validateTargetsWithSim()
    % Fallback: test with ValidatorSim when validator_manager service unavailable
    fprintf('\n--- debug_validateTargets (ValidatorSim fallback) ---\n');
    validator = ultrasat.api.clients.ValidatorSim('./sim/debug_validator.json');
    targets = createSampleTargets();
    response = validator.validateTargets(targets);
    fprintf('ValidatorSim ok, status=%s\n', response.status);
    if isfield(response, 'task')
        fprintf('task_id=%s, target_count=%d\n', ...
            debug_getField(response.task, 'task_id', ''), ...
            debug_getField(response.task, 'target_count', 0));
    end
end


function targets = createSampleTargets()
    % Creates a sample list of targets (same format as debug_ValidatorSim).
    targets = struct('coord_ra', {}, 'coord_dec', {}, 'tiles', {}, 'exposure', {}, 'image_count', {}, 'start_time', {});

    for i = 1:3
        targets(i) = struct(...
            'coord_ra', 10 + i, ...
            'coord_dec', 20 + i, ...
            'tiles', '1,2,3,4', ...
            'exposure', seconds(300), ...
            'image_count', 2, ...
            'start_time', datetime('2028-01-01 00:00:00', 'TimeZone', 'UTC') + hours(i-1) ...
        );
    end
end


function s = debug_getStatus(response)
    if isfield(response, 'status')
        s = response.status;
    else
        s = '';
    end
end


function v = debug_getField(s, fld, default)
    if isstruct(s) && isfield(s, fld)
        v = s.(fld);
    else
        v = default;
    end
end
