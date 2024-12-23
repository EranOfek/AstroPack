
function debug_Mission()
    debugGetApprovedTargets();
end


function debugGetApprovedTargets()
    % debugGetApprovedTargets
    
    % Define API URL and create client
    apiUrl = 'http://localhost:8215';
    apiKey = 'ULTRASOC-2024-10-17';  % Assuming no API key is needed for localhost
    timeout = 30; % Request timeout in seconds
    client = soc.api.MissionClient(apiUrl, apiKey, timeout);
    
    % Test StartParams
    fprintf('Testing getApprovedTargets...\n');
    start_time = datetime(2024, 1, 1, 0, 0, 0);
    end_time = datetime(2025, 12, 31, 0, 0, 0);
    response = client.getApprovedTargets(start_time, end_time);
    disp(response);
    for i=1:length(response.targets)
        disp(response.targets(i));
    end

    targets = struct2table(response.targets);
    disp(targets);

    reverse = table2struct(targets);
    disp(reverse);

    %fprintf('getApprovedTargets response: %s\n', mat2str(success));

   
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

