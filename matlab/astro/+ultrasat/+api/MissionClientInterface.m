%==========================================================================
% Project     : ULTRASAT Observation Planner
% Filename    : ultrasat.api.MissionClientInterface.m
% Author      : Chen Tishler
% Created     : 01/12/2024
% Updated     : 21/09/2025
% Description : Interface class for mission control API clients.
%               Created in MainModule.ApiInterface, used by 'uplanner'.
%==========================================================================

classdef MissionClientInterface < ultrasat.api.Loggable
    % This class serves as the interface between MissionClient/MissionClientSim
    % and the uplanner class.
    %
    % Functions:
    %   getApprovedTargets()   - Retrieves the list of approved observation targets within a time range.
    %   validatePlan()         - Validates an observation plan against mission constraints.
    %   submitPlan()           - Submits an observation plan to the mission control system.
    %   retractPlan()          - Retracts a previously submitted observation plan.
    %   getExposure()          - Retrieves exposure data for specified healpix indices and time range.
    %   msglog()               - Logs a formatted message to the console.
    %
    % Related files:
    %   MissionClientBase.m    - Base class for Mission client API calls.
    %   MissionClientSim.m     - Simulation class for Mission client API calls.
    %   MissionClient.m        - Mission client that communicates with the server.
    %   uplanner.m             - MATLAB class for the observation planner.

    properties
        ApiClient                       % Instance of MissionClient/MissionClientSim

        % Updated by getApprovedTargets() in derived class
        ApprovedTargetsStartTime        % Updated by getApprovedTargets()
        ApprovedTargetsEndTime          % Updated by getApprovedTargets()

        % Instance of ultrasat.api.PlanData containing current plan information,
        % required because uplanner has only partial data (this is Yossi design that we need to meet).
        PlanData                        % Instance of ultrasat.api.PlanData
    end


    methods
        function obj = MissionClientInterface(ApiClient)
            % Constructor for the MissionClientInterface class.
            %
            % Parameters:
            %   ApiClient - Instance of MissionClientBase
            %
            % Returns:
            %   obj - Initialized MissionClientInterface object
            arguments
                ApiClient
            end

            obj.ApiClient = ApiClient;
        end

        % =================================================================
        %                       Called by uplanner
        % =================================================================
        function response = getApprovedTargets(obj, start_time, end_time)
            % Retrieves the list of approved observation targets within a time range.
            %
            % Parameters:
            %   start_time (datetime) - Start time for filtering targets
            %   end_time (datetime) - End time for filtering targets
            %
            % Returns:
            %   response - Structure containing result with fields:
            %     .status - Status of the operation ('ok' or 'error')
            %     .message - Description message (if error)
            %     .targets - Array of target structures (if successful)
            %     .ok - Boolean indicating success (true) or failure (false)
            %
            % Notes:
            %   This method updates the ApprovedTargetsStartTime and
            %   ApprovedTargetsEndTime properties.
            arguments
                obj
                start_time % (datetime)
                end_time % (datetime)
            end
            Params = struct('start_time', start_time, 'end_time', end_time);
            response = obj.ApiClient.getApprovedTargets(Params);

            % Store the times, will be displayed to the user in GUI
            obj.ApprovedTargetsStartTime = start_time;
            obj.ApprovedTargetsEndTime = end_time;
        end


        function response = validatePlan(obj, Plan)
            % Validates an observation plan against mission constraints.
            %
            % Parameters:
            %   Plan - Array of structs containing observation data
            %
            % Returns:
            %   response - Structure containing validation result with fields:
            %     .status - Status of the operation ('ok' or 'error')
            %     .message - Description message
            %     .ok - Boolean indicating success (true) or failure (false)
            %     .validations - Detailed validation results (if available)
            %
            % Notes:
            %   Called from uplanner, Plan is array of structs
            arguments
                obj
                Plan
            end
            Params = struct('plan', Plan);
            response = obj.ApiClient.validatePlan(Params);
        end


        function response = submitPlan(obj, Plan)
            % Submits an observation plan to the mission control system.
            %
            % Parameters:
            %   Plan - Array of structs containing observation data
            %
            % Returns:
            %   response - Structure containing submission result with fields:
            %     .status - Status of the operation ('ok' or 'error')
            %     .message - Description message
            %     .ok - Boolean indicating success (true) or failure (false)
            %
            % Notes:
            %   Called from uplanner
            arguments
                obj
                Plan
            end
            Params = struct('plan', Plan);
            response = obj.ApiClient.submitPlan(Params);
        end


        function response = retractPlan(obj, Plan)
            % Retracts a previously submitted observation plan.
            %
            % Parameters:
            %   Plan - Array of structs identifying the plan to retract
            %
            % Returns:
            %   response - Structure containing retraction result with fields:
            %     .status - Status of the operation ('ok' or 'error')
            %     .message - Description message
            %     .ok - Boolean indicating success (true) or failure (false)
            %
            % Notes:
            %   Called from uplanner
        end


        function response = getExposure(obj, table_name, healpix_indices, start_timestamp, end_timestamp, select_all)
            % Retrieves exposure data for specified healpix indices and time range.
            %
            % Parameters:
            %   table_name (string) - Name of the exposure data table
            %   healpix_indices (array) - Array of healpix indices to filter by
            %   start_timestamp (datetime) - Start time for filtering exposures
            %   end_timestamp (datetime) - End time for filtering exposures
            %   select_all (logical) - If true, returns all records regardless of filters
            %
            % Returns:
            %   response - Structure containing result with fields:
            %     .status - Status of the operation ('ok' or 'error')
            %     .message - Description message (if error)
            %     .data - Array of exposure data structures (if successful)
            %     .ok - Boolean indicating success (true) or failure (false)
        end

    end
end
