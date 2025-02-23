
function debug_ValidatorSim()
    % Main debug function to test ValidatorSim functionality.
    clc;
    fprintf('=== Debugging ValidatorSim ===\n');

    validator = ultrasat.api.ValidatorSim('./sim/debug_validator.json');

    % Create a sample list of targets
    targets = createSampleTargets();

    % Call the validateTargets method
    fprintf('\n--- Debugging validateTargets ---\n');
    response = validator.validateTargets(targets);

    % Display the validation response
    if isfield(response, 'task')
        fprintf('Validation task completed:\n');
        disp(response.task);
    else
        fprintf('Validation failed.\n');
    end
end


function targets = createSampleTargets()
    % Creates a sample list of targets for validation.
    targets = struct('coord_ra', {}, 'coord_dec', {}, 'tiles', {}, 'exposure', {}, 'image_count', {}, 'start_time', {});

    for i = 1:3
        targets(i) = struct(...
            'coord_ra', 10 + i, ...
            'coord_dec', 20 + i, ...
            'tiles', '1,2,3,4', ...
            'exposure', seconds(300), ...
            'image_count', 2, ...
            'start_time', datetime('2028-01-01 00:00:00') + hours(i-1) ...
        );
    end
end
