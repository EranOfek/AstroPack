
function debug_MissionClient()
    debugGetApprovedTargets();
end


function debugGetApprovedTargets()
    % debugGetApprovedTargets

    % Define API URL and create client (will be fetched from Env)
    client = ultrasat.api.MissionClient();
    client.ApiUrl = 'http://localhost:8215';

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

    save('c:\temp\api_response.mat', 'response');
    target1 = response.targets(1);
    save('c:\temp\api_target1.mat', 'target1');

    % Convert array of structs to Table
    TargetsTable = struct2table(response.targets);
    disp(TargetsTable);

    % Convert Table to array of struct
    TargetsArray = table2struct(TargetsTable);
    disp(TargetsArray);

end

