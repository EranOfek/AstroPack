%==========================================================================
% Project     : ULTRASAT Observation Planner
% Filename    : ultrasat.api.NamespaceManagerClient.m
% Author      : Chen Tishler
% Created     : 01/12/2024
% Updated     : 18/02/2026
% Description : Client implementation of the NamespaceManagerBase interface.
%==========================================================================


classdef NamespaceManagerClient < ultrasat.api.clients.ClientBase
    % Simulator implementation of the UserManagerBase interface.
    % This class provides methods to interact with the NamespaceManagerBase interface.
    % It is a subclass of ultrasat.api.ClientBase.
    %
    % Typical Usage:
    %   namespaceManager = ultrasat.api.NamespaceManagerClient();
    %   response = namespaceManager.getNamespaceList();

    properties
        DbPath          % Path to simulator data files
        Validator       % instance of ultrasat.api.ValidatorSim()
        ApiSimProvider  % instance of ultrasat.api.ApiSimProvider()
    end


    methods
        function obj = NamespaceManagerClient()
            % Call the base class constructor with the Args
            % ArgsCell = namedargs2cell(Args);
            obj@ultrasat.api.clients.ClientBase();
            obj.msglog('NamespaceManagerClient constructor started');

            % Initialize the logger
            obj.LogPrefix = 'NamespaceManagerClient';

        end

        % -------------------------------------------------------------------

        function response = getNamespaceList(obj)
            % Returns the list of namespace_id values from namespaces.json
            obj.msglog('getNamespaceList: Getting list of namespaces');

            response = struct();
    end
end
