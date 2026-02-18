%==========================================================================
% Project     : ULTRASAT Observation Planner
% File        : ultrasat.api.ClientFactory.m
% Author      : Chen Tishler
% Created     : 18/02/2026
% Updated     : 18/02/2026
% Description : Client factory that loads SOC config/services.json once and
%               returns service base URLs (direct/nginx) + API key.
%==========================================================================

function debug_UserManagerClient()
    % Create a UserManagerClient instance
    userManager = ultrasat.api.clients.UserManagerClient();
    % Get the list of users
    users = userManager.getUserList();
    % Print the list of users
    disp(users);
end
