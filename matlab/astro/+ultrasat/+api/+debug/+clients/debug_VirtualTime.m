
function debug_VirtualTime()
    %debugVirtualTimeModels();
    debugVirtualTimeClient();
end


function debugVirtualTimeClient()
    % debugVirtualTimeClient - Test the VirtualTimeClient functionality

    % Define API URL and create client
    apiUrl = 'http://localhost:8203';
    apiKey = 'ULTRASOC-2024-10-17';  % Assuming no API key is needed for localhost
    timeout = 30; % Request timeout in seconds
    client = ultrasat.api.clients.VirtualTimeClient(apiUrl, apiKey, timeout, 'vtm1');

    % Test StartParams
    fprintf('Testing start...\n');
    success = client.start(2, []);  %posixtime(datetime('now')));
    fprintf('Start response: %s\n', mat2str(success));

    % Test PauseParams
    fprintf('Testing pause...\n');
    success = client.pause();
    fprintf('Pause response: %s\n', mat2str(success));

    % Test Get State
    fprintf('Testing getState...\n');
    state = client.getState();
    fprintf('Current state: %s\n', state);
end


function debugVirtualTimeModels()
    % debugVirtualTimeModels - Debug function for VirtualTimeModels.
    % Creates sample parameters, serializes them to JSON, and prints the results.

    import soc.api.VirtualTimeModels;

    % Create StartParams
    startParams = ultrasat.api.models.VirtualTimeModels.StartParams('vtm1', 2, []);  %, posixtime(datetime('now')));
    fprintf('StartParams JSON:\n%s\n\n', startParams.toJson());
    startParams.show();

    % Create PauseParams
    pauseParams = ultrasat.api.models.VirtualTimeModels.PauseParams('vtm1');
    fprintf('PauseParams JSON:\n%s\n\n', pauseParams.toJson());
    pauseParams.show();

end

