function debug_Preferences()
    import ultrasat.planner.gui.Preferences;

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

