function debug_PlanData()
    import ultrasat.planner.gui.PlanData;

    % Test all functions of the MainModule class
    % import utlrasat.planner.gui;
    fprintf('Testing MainModule class...\n');

    % Create an instance of MainModule
    mainModule = MainModule();
    fprintf('MainModule instance created successfully.\n');

    %debugApiClient(mainModule);

    debugUtils(mainModule);

    debugFieldValueGetters(mainModule);

end


function debugApiClient(mainModule)   
    
    % Test constructor and properties
    assert(~isempty(mainModule.ApiClient), 'ApiClient not initialized.');
    assert(strcmp(mainModule.ApiClient.ApiUrl, 'http://localhost:8215'), 'ApiUrl not set correctly.');
    assert(strcmp(mainModule.DebugPath, 'C:/Temp/_planner'), 'DebugPath not set correctly.');
    fprintf('Constructor test passed.\n');
    
    % Test login method
    result = mainModule.login('test_user', 'test_password');
    fprintf('Login result: %d\n', result);
    assert(result == true || result == false, 'Login returned an invalid result.');
    
    % Test logout method
    result = mainModule.logout();
    fprintf('Logout result: %d\n', result);
    assert(result == true || result == false, 'Logout returned an invalid result.');
    
    % Test setPlanner method
    plannerMock = struct('Type', 'HCS'); % Mock planner object
    mainModule.setPlanner(plannerMock);
    assert(strcmp(mainModule.PlanType, 'HCS'), 'PlanType not set correctly.');
    fprintf('setPlanner test passed.\n');
end

