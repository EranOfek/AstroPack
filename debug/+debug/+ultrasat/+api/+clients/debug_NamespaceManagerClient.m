%==========================================================================
% Project     : ULTRASAT Observation Planner
% Filename    : +debug/+ultrasat/+api/+clients/debug_NamespaceManagerClient.m
% Author      : Chen Tishler
% Created     : 18/02/2026
% Updated     : 18/02/2026
% Description : Debug function for NamespaceManagerClient.
%
% Run by      : debug.ultrasat.api.clients.debug_NamespaceManagerClient()
%==========================================================================

function debug_NamespaceManagerClient()
    factory = ultrasat.api.clients.ClientFactory();
    baseUrl = factory.getServiceBaseUrl('namespace_manager', 'nginx');
    client = ultrasat.api.clients.NamespaceManagerClient(baseUrl);
    response = client.getNamespaceList();
    disp(response);
    fprintf('Namespaces:\n');
    if isfield(response, 'namespaces') && ~isempty(response.namespaces)
        % Convert the list of namespaces (likely struct array) to a table and display all fields
        namespacesTable = struct2table(response.namespaces);
        disp(namespacesTable);
    else
        disp('No namespaces found or response does not contain a "namespaces" field.');
    end
    disp(response.display_list);
end
