%==========================================================================
% Project     : ULTRASAT Observation Planner
% File        : ultrasat.api.ClientFactory.m
% Author      : Chen Tishler
% Created     : 18/02/2026
% Updated     : 18/02/2026
% Description : Client factory that loads SOC config/services.json once and
%               returns service base URLs (direct/nginx) + API key.
%==========================================================================

function debug_NamespaceManagerClient()
    % Create a NamespaceManagerClient instance
    namespaceManager = ultrasat.api.clients.NamespaceManagerClient();
    % Get the list of namespaces
    namespaces = namespaceManager.getNamespaceList();
    % Print the list of namespaces
    disp(namespaces);
end