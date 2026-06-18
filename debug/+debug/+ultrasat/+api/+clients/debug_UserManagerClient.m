%==========================================================================
% Project     : ULTRASAT Observation Planner
% Filename    : +debug/+ultrasat/+api/+clients/debug_UserManagerClient.m
% Author      : Chen Tishler
% Created     : 18/02/2026
% Updated     : 18/02/2026
% Description : Debug function for UserManagerClient.
%               Uses ClientFactory for baseUrl and apiKey.
%
% Run by      : debug.ultrasat.api.clients.debug_UserManagerClient()
%==========================================================================

function debug_UserManagerClient()
    factory = ultrasat.api.clients.ClientFactory();
    baseUrl = factory.getServiceBaseUrl('user_manager', 'nginx');
    client = ultrasat.api.clients.UserManagerClient(baseUrl);

    fprintf('Testing login (user: chen, password: 123)...\n');
    loginResponse = client.login('chen', '123', 'OPER');
    disp(loginResponse);
    if isfield(loginResponse, 'data') && isstruct(loginResponse.data) && ~isempty(loginResponse.data)
        fprintf('  data (PlatformUser):\n');
        disp(loginResponse.data);
    end

    fprintf('\nTesting logout (user: chen)...\n');
    logoutResponse = client.logout('chen');
    disp(logoutResponse);

    fprintf('Testing login (user: chen, password: wrong)...\n');
    loginResponse = client.login('chen', 'wrong', 'OPER');
    disp(loginResponse);
end
