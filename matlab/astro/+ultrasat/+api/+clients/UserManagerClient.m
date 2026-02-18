%==========================================================================
% Project     : ULTRASAT Observation Planner
% Filename    : ultrasat.api.UserManagerClient.m
% Author      : Chen Tishler
% Created     : 01/12/2024
% Updated     : 18/02/2026
% Description : Client implementation of the UserManagerBase interface.
%==========================================================================


classdef UserManagerClient < ultrasat.api.clients.ClientBase
    % Client implementation of the UserManagerBase interface.
    % This class provides methods to interact with the UserManagerBase interface.
    % It is a subclass of ultrasat.api.ClientBase.
    %
    % Typical Usage:
    %   userManager = ultrasat.api.UserManagerClient();
    %   response = userManager.login('chen', '123', 'OPER');
    %   response = userManager.IsAllowed('MissionControl.Planner.Run', 'any_plan', true);
    %   response = userManager.logout('chen');


    methods
        function obj = UserManagerClient()
            % Call the base class constructor with the Args
            % ArgsCell = namedargs2cell(Args);
            obj@ultrasat.api.clients.UserManagerBase();
            obj.msglog('UserManagerClient constructor started');

            % Initialize the logger
            obj.LogPrefix = 'UserManagerClient';

        end

        % -------------------------------------------------------------------

        function response = login(obj, UserName, Password, Namespace)
            % Login using username, password and device ID
            % Loads users, roles, permissions and updates session

            response = struct();
            obj.msglog('login: user=%s', UserName);
            response.ok = true;
            response.status = 'ok';
            response.message = 'Login successful';
            response.session_id = sessionId;
            response.user = UserName;
        end

        % -------------------------------------------------------------------

        function response = logout(obj, UserName)
            % Simulate logout by clearing current_user.json

            obj.msglog('logout: User %s logged out successfully.', UserName);
            response.status = 'ok';
            response.ok = true;
        end

        % -------------------------------------------------------------------

    end
end
