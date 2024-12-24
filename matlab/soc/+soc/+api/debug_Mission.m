
function debug_Mission()
    debugGetApprovedTargets();
end


function debugGetApprovedTargets()
    % debugGetApprovedTargets
    
    % Define API URL and create client (will be fetched from Env)
    apiUrl = 'http://localhost:8215';
    apiKey = 'ULTRASOC-2024-10-17';  
    timeout = 30; % Request timeout in seconds
    client = soc.api.MissionClient(apiUrl, apiKey, timeout);
    
    % Prepare function parameters
    fprintf('Testing getApprovedTargets...\n');
    start_time = datetime(2024, 1, 1, 0, 0, 0);
    end_time = datetime(2025, 12, 31, 0, 0, 0);

    % Call the API function
    response = client.getApprovedTargets(start_time, end_time);
    disp(response);

    % Iterate targets in array of structs
    for i=1:length(response.targets)
        disp(response.targets(i));
    end

    % Convert array of structs to Table
    TargetsTable = struct2table(response.targets);
    disp(TargetsTable);

    % Convert Table to array of struct
    TargetsArray = table2struct(TargetsTable);
    disp(TargetsArray);
   
end


function debugMissionModels()
    % debugVirtualTimeModels - Debug function for VirtualTimeModels.
    % Creates sample parameters, serializes them to JSON, and prints the results.

    import soc.api.MissionModels;

    % Create StartParams
    startParams = VirtualTimeModels.StartParams('vtm1', 2, []);  %, posixtime(datetime('now')));
    fprintf('StartParams JSON:\n%s\n\n', startParams.toJson());
    startParams.show();

    % Create PauseParams
    pauseParams = VirtualTimeModels.PauseParams('vtm1');
    fprintf('PauseParams JSON:\n%s\n\n', pauseParams.toJson());
    pauseParams.show();

end

