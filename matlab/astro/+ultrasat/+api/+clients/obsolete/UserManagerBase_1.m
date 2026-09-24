%==========================================================================
% Project     : ULTRASAT Observation Planner
% Filename    : ultrasat.api.UserManagerBase.m
% Author      : Chen Tishler
% Created     : 14/09/2025
% Updated     : 21/09/2025
% Description : Base class for user management
%==========================================================================

classdef UserManagerBase < ultrasat.api.core.Loggable
    % Base class for user management

    methods
        function obj = UserManagerBase() %Args)
            % Constructor for the UserManagerBase class.
   
            obj@ultrasat.api.core.Loggable();
            obj.msglog('UserManagerBase constructor started');

            obj.LogPrefix = 'UserManagerBase';
        end

        % -------------------------------------------------------------------

        function response = login(obj, UserName, Password, Namespace)
            % Authenticates a user with the mission control API
            %
            % Parameters:
            %   UserName (string) - User name for authentication
            %   Password (string) - Password for authentication
            %
            % Returns:
            %   response - Structure containing authentication result with fields:
            %     .status - Status of the operation ('ok' or 'error')
            %     .message - Description message
            %     .ok - Boolean indicating success (true) or failure (false)
            %     .user - (if successful) User information structure
        end


        function response = logout(obj, UserName)
            % Logs out the current user from the mission control API
            %
            % Parameters:
            %   UserName (string) - Currently logged-in user name to verify
            %
            % Returns:
            %   response - Structure containing logout result with fields:
            %     .status - Status of the operation ('ok' or 'error')
            %     .message - Description message
            %     .ok - Boolean indicating success (true) or failure (false)
        end

    end

end

