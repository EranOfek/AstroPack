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
    debug_validateTargetsInvalidCoords(client);
    debug_validateTargetsEmpty(client);
    debug_validateTargetsNegativeExposure(client);

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


function debug_validateTargetsInvalidCoords(client)
    fprintf('\n--- debug_validateTargetsInvalidCoords ---\n');
    targets = createInvalidCoordTargets();
    try
        response = client.validateTargets(targets);
        fprintf('ok=%d, status=%s\n', response.ok, debug_getStatus(response));
        if isfield(response, 'message') && ~isempty(response.message)
            fprintf('message: %s\n', response.message);
        end
        if ~response.ok
            fprintf('Validation failed as expected (invalid RA/Dec).\n');
        end
    catch ME
        fprintf('validateTargets failed: %s\n', ME.message);
        fprintf('Exception expected for invalid coordinates.\n');
    end
end


function debug_validateTargetsEmpty(client)
    fprintf('\n--- debug_validateTargetsEmpty ---\n');
    targets = struct('coord_ra', {}, 'coord_dec', {}, 'tiles', {}, 'exposure', {}, 'image_count', {}, 'start_time', {});
    try
        response = client.validateTargets(targets);
        fprintf('ok=%d, status=%s\n', response.ok, debug_getStatus(response));
        if isfield(response, 'message') && ~isempty(response.message)
            fprintf('message: %s\n', response.message);
        end
        if ~response.ok
            fprintf('Validation failed as expected (empty targets).\n');
        end
    catch ME
        fprintf('validateTargets failed: %s\n', ME.message);
        fprintf('Exception expected for empty targets.\n');
    end
end


function debug_validateTargetsNegativeExposure(client)
    fprintf('\n--- debug_validateTargetsNegativeExposure ---\n');
    targets = createNegativeExposureTargets();
    try
        response = client.validateTargets(targets);
        fprintf('ok=%d, status=%s\n', response.ok, debug_getStatus(response));
        if isfield(response, 'message') && ~isempty(response.message)
            fprintf('message: %s\n', response.message);
        end
        if ~response.ok
            fprintf('Validation failed as expected (negative exposure).\n');
        end
    catch ME
        fprintf('validateTargets failed: %s\n', ME.message);
        fprintf('Exception expected for negative exposure.\n');
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


function targets = createInvalidCoordTargets()
    % Targets with out-of-bounds RA (0-360) and Dec (-90 to 90).
    targets = struct('coord_ra', {}, 'coord_dec', {}, 'tiles', {}, 'exposure', {}, 'image_count', {}, 'start_time', {});
    targets(1) = struct(...
        'coord_ra', 400, ...
        'coord_dec', 95, ...
        'tiles', '1,2,3,4', ...
        'exposure', seconds(300), ...
        'image_count', 2, ...
        'start_time', datetime('2028-01-01 00:00:00', 'TimeZone', 'UTC') ...
    );
end


function targets = createNegativeExposureTargets()
    % Targets with negative exposure (invalid).
    targets = struct('coord_ra', {}, 'coord_dec', {}, 'tiles', {}, 'exposure', {}, 'image_count', {}, 'start_time', {});
    targets(1) = struct(...
        'coord_ra', 100, ...
        'coord_dec', 50, ...
        'tiles', '1,2,3,4', ...
        'exposure', seconds(-100), ...
        'image_count', 2, ...
        'start_time', datetime('2028-01-01 00:00:00', 'TimeZone', 'UTC') ...
    );
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
