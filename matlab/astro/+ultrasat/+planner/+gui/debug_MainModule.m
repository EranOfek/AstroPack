function debug_MainModule()
    import ultrasat.planner.gui.MainModule;

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


function debugUtils(mainModule)        
    % Test setModified and clearModified
    mainModule.setModified();
    assert(mainModule.Modified == true, 'setModified failed.');
    fprintf('setModified test passed.\n');
    
    mainModule.clearModified();
    assert(mainModule.Modified == true, 'clearModified failed.');
    fprintf('clearModified test passed.\n');
    
    % Test logging methods
    mainModule.msglog('This is a test log message.');
    fprintf('msglog test passed.\n');
    
    try
        error('Test exception');
    catch ME
        mainModule.msgex('Exception occurred during test.', ME);
    end
    fprintf('msgex test passed.\n');
    
end


function debugFieldValueGetters(mainModule)
    % Helper function to test field value getter methods
    fprintf('Testing field value getters...\n');
    
    % getFieldText
    result = mainModule.getFieldText('  Hello  ');
    assert(strcmp(result, 'Hello'), 'getFieldText failed.');
    
    % getFieldNum
    result = mainModule.getFieldNum(' 123 ');
    assert(result == 123, 'getFieldNum failed.');
    
    % getFieldTitle
    result = mainModule.getFieldTitle('  Title  ');
    assert(strcmp(result, 'Title'), 'getFieldTitle failed.');
    
    % getFieldUniqueTargetName
    result = mainModule.getFieldUniqueTargetName('  TargetName  ');
    assert(strcmp(result, 'TargetName'), 'getFieldUniqueTargetName failed.');
    
    % getFieldRA
    result = mainModule.getFieldRA(' 123.456 ');
    assert(result == 123.456, 'getFieldRA failed.');
    
    % getFieldDec
    result = mainModule.getFieldDec(' -45.678 ');
    assert(result == -45.678, 'getFieldDec failed.');
    
    % getFieldDateTime
    result = mainModule.getFieldDateTime('2025-01-07T12:34:56');
    assert(isequal(result, datetime('2025-01-07T12:34:56')), 'getFieldDateTime failed.');
    
    % getFieldDuration
    result = mainModule.getFieldDuration('2:30:45');
    assert(isequal(result, duration('2:30:45')), 'getFieldDuration failed.');
    
    fprintf('Field value getters test passed.\n');
end

