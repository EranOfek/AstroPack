function debug_Config()

    % debug_Config - Tests the configuration of the API.
    fprintf('--- Testing Configuration ---\n');
    config = ultrasat.api.Config.getApiConfig();
    fprintf('  [SUCCESS] Configuration returned.\n');

    disp(config);

    fprintf('----------------------------------------\n\n');
end

