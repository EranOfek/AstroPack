
function debug_MissionModels()
    % Main function to debug ultrasat.api.models.MissionModels functionality.
    fprintf('--- Debugging MissionModels ---\n');

    % Test newImagingTarget
    debugNewImagingTarget();

    % Test newEmptyPlanStruct
    debugNewEmptyPlanStruct();

    % Test toUtc
    debugToUtc();
end

% ------------------------------------------------------------------------

function debugNewImagingTarget()
    fprintf('\n--- Testing newImagingTarget() ---\n');
    target = ultrasat.api.models.MissionModels.newImagingTarget();
    fprintf('Created ImagingTarget model (struct):\n');
    disp(target);
end

% ------------------------------------------------------------------------

function debugNewEmptyPlanStruct()
    fprintf('\n--- Testing newEmptyPlanStruct() ---\n');
    planStruct = ultrasat.api.models.MissionModels.newEmptyPlanStruct();
    fprintf('Created Empty Plan Struct:\n');
    disp(planStruct);
end

% ------------------------------------------------------------------------

function debugToUtc()
    fprintf('\n--- Testing toUtc() ---\n');

    dt_str = '2028-01-01T12:00:00.000Z';
    dt_obj = datetime('now', 'TimeZone', 'local');

    fprintf('Converting string to UTC:\n');
    dt_converted = ultrasat.api.utils.DateTimeUtils.toUtc(dt_str);
    disp(dt_converted);

    fprintf('Converting datetime to UTC:\n');
    dt_converted = ultrasat.api.utils.DateTimeUtils.toUtc(dt_obj);
    disp(dt_converted);
end
