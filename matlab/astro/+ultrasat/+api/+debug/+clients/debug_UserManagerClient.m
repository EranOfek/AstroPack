%==========================================================================
% Project     : ULTRASAT Observation Planner
% File        : ultrasat.api.debug.clients.debug_UserManagerClient.m
% Author      : Chen Tishler
% Created     : 18/02/2026
% Updated     : 18/02/2026
% Description : Debug function for UserManagerClient.
%               Uses ClientFactory for baseUrl and apiKey.
%==========================================================================

function debug_UserManagerClient()
    factory = ultrasat.api.clients.ClientFactory();
    baseUrl = factory.getServiceBaseUrl('user_manager');
    client = ultrasat.api.clients.UserManagerClient(baseUrl);

    fprintf('Testing login (user: chen, password: 123)...\n');
    loginResponse = client.login('chen', '123', 'OPER');
    disp(loginResponse);

    fprintf('\nTesting logout (user: chen)...\n');
    logoutResponse = client.logout('chen');
    disp(logoutResponse);

    fprintf('Testing login (user: chen, password: 123)...\n');
    loginResponse = client.login('chen', 'wrong', 'OPER');
    disp(loginResponse);    
end
