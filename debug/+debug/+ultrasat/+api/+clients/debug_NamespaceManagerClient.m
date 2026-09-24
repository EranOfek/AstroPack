%==========================================================================
% Project     : ULTRASAT Observation Planner
% Filename    : +debug/+ultrasat/+api/+clients/debug_NamespaceManagerClient.m
% Author      : Chen Tishler
% Created     : 18/02/2026
% Updated     : 03/05/2026
% Description : Debug function for NamespaceManagerClient.
%
% Run by      : debug.ultrasat.api.clients.debug_NamespaceManagerClient()
%==========================================================================

function debug_NamespaceManagerClient()
    % Fetch namespace list via ClientFactory + NamespaceManagerClient.

    % Create a factory and get the base URL for the namespace manager service
    factory = ultrasat.api.clients.ClientFactory();
    baseUrl = factory.getServiceBaseUrl('namespace_manager', 'nginx');

    % Create a namespace manager client and get the namespace list
    client = ultrasat.api.clients.NamespaceManagerClient(baseUrl);
    response = client.getNamespaceList();
    disp(response);
    fprintf('Namespaces:\n');

    % Check if the response contains a 'namespaces' field and it is not empty
    if isfield(response, 'namespaces') && ~isempty(response.namespaces)
        % Convert the list of namespaces (likely struct array) to a table and display all fields
        namespacesTable = struct2table(response.namespaces);
        disp(namespacesTable);
    else
        disp('No namespaces found or response does not contain a "namespaces" field.');
    end
    disp(response.display_list);
end
