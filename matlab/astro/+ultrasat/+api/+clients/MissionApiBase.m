%==========================================================================
% Project     : ULTRASAT Observation Planner
% Filename    : ultrasat.api.MissionApiBase.m
% Author      : Chen Tishler
% Created     : 01/12/2024
% Updated     : 21/09/2025
% Description : Base class for Mission client API calls.
%==========================================================================

classdef MissionApiBase < ultrasat.api.core.Loggable
    % Base class for Mission client API calls.
    % Provides the foundation for communication between MATLAB-based ULTRASAT
    % observation planner and the backend server. This class is used by both
    % the GUI AppDesigner app and the uplanner backend matlab class.

    properties
        ApiUrl          % Base URL of the mission control API
        PlanData        % Instance of ultrasat.api.PlanData containing current plan information
        LogFileName     % Path to the log file for storing client operations

        % Updated by getApprovedTargets() in derived class
        ApprovedTargetsStartTime        % Updated by getApprovedTargets()
        ApprovedTargetsEndTime          % Updated by getApprovedTargets()
    end


    methods
        function obj = MissionApiBase(Args)
            % Constructor for the MissionClientBase class.
            %
            % Parameters:
            %   Args.SubUrl (string) - Endpoint path to append to the base API URL (default: '/mission')
            %   Args.LogFileName (string) - Path to log file (default: same folder as this file)
            %
            % Returns:
            %   obj - Initialized MissionClientBase object
            arguments
                Args.SubUrl = '/mission';  % planner_backend
                Args.LogFileName = []
            end

            % Initialize the logger
            obj.LogPrefix = 'MissionClientBase';

            if isempty(Args.LogFileName)
                srcFile = mfilename('fullpath');  srcFolder = fileparts(srcFile);
                obj.LogFileName = fullfile(srcFolder, [mfilename, '.log']);
            else
                obj.LogFileName = Args.LogFileName;
            end

            % Call parent class constructor
            %ArgsCell = namedargs2cell(Args);
            %obj@api.ClientBase(ArgsCell{:});  % Args);  % , 'SubUrl', '/mission');
        end

        % -------------------------------------------------------------------

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
            %   Called from uplanner, Plan is array of struct
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

        % =================================================================
        %                       Plans Table CRUD
        % =================================================================

        function response = getPlansList(obj)
            % Retrieves a list of all observation plans from the server.
            %
            % Returns:
            %   response - Structure containing result with fields:
            %     .status - Status of the operation ('ok' or 'error')
            %     .message - Description message (if error)
            %     .plans - Array of plan summary structures (if successful)
            %     .ok - Boolean indicating success (true) or failure (false)
        end


        function response = loadPlan(obj, plan_pk)
            % Loads a specific observation plan by its primary key.
            %
            % Parameters:
            %   plan_pk (integer) - Primary key of the plan to load
            %
            % Returns:
            %   response - Structure containing result with fields:
            %     .status - Status of the operation ('ok' or 'error')
            %     .message - Description message
            %     .plan - Complete plan data structure (if successful)
            %     .ok - Boolean indicating success (true) or failure (false)
            %
            % Notes:
            %   This method populates the obj.PlanData property.
        end


        function response = savePlan(obj)
            % Saves the current observation plan (from obj.PlanData) to the server.
            %
            % Returns:
            %   response - Structure containing result with fields:
            %     .status - Status of the operation ('ok' or 'error')
            %     .message - Description message
            %     .ok - Boolean indicating success (true) or failure (false)
        end


        function response = deletePlan(obj, plan_pk)
            % Deletes a specific observation plan by its primary key.
            %
            % Parameters:
            %   plan_pk (integer) - Primary key of the plan to delete
            %
            % Returns:
            %   response - Structure containing result with fields:
            %     .status - Status of the operation ('ok' or 'error')
            %     .message - Description message
            %     .ok - Boolean indicating success (true) or failure (false)
        end


        function response = getPlanStatus(obj, plan_pk)
            % Retrieves the current status of a specific observation plan.
            %
            % Parameters:
            %   plan_pk (integer) - Primary key of the plan to check
            %
            % Returns:
            %   response - Structure containing result with fields:
            %     .status - Status of the operation ('ok' or 'error')
            %     .message - Description message (if error)
            %     .data - Structure with plan status information (if successful)
            %     .ok - Boolean indicating success (true) or failure (false)
        end

        % =================================================================

        % =================================================================

        function submitData = convertPlanTableToSubmitData(obj, Plan)
            % Converts the uplanner.Plan table to a list of structs for submission.
            %
            % Parameters:
            %   Plan - MATLAB table containing observation plan data.
            %
            % Returns:
            %   submitData - Array of structs with only the required fields for submission.
            %
            % Notes:
            %   This method extracts specific fields needed for the submission API and
            %   performs necessary data type conversions.

            % Initialize the output
            submitData = [];

            % Loop through each row of the table
            for i = 1:height(Plan)
                % Extract required fields and store in a struct
                rowStruct = struct(...
                    'coord_ra', Plan.RA(i), ...
                    'coord_dec', Plan.Dec(i), ...
                    'tiles', Plan.Tiles(i), ...
                    'exposure', seconds(Plan.ExpTime(i)), ... % Convert duration to seconds
                    'image_count', Plan.Nexposures(i), ...
                    'start_time', datestr(Plan.Tstart(i), 'yyyy-mm-ddTHH:MM:SS.FFFZ'), ...
                    'jd_start', Plan.JDstart(i), ...
                    'jd_end', Plan.JDend(i), ...
                    'total_duration', seconds(Plan.TotalDuration(i)), ...
                    'slew_time_before', seconds(Plan.SlewTimeBefore(i)) ...
                    );
                
                % Append to the list
                submitData = [submitData; rowStruct];
            end
        end


        function Plan = convertPlanTimesToUtc(obj, Plan)
            % Converts start_time and end_time fields of each plan entry to UTC.
            %
            % Parameters:
            %   Plan - Array of structs containing plan data with time fields
            %
            % Returns:
            %   Plan - Same array with time fields converted to UTC format
            %
            % Notes:
            %   This is used to ensure consistent time format for API communication
            for i = 1:numel(Plan)
                Plan(i).start_time = ultrasat.api.utils.DateTimeUtils.toUtc(Plan(i).start_time);
                Plan(i).end_time = ultrasat.api.utils.DateTimeUtils.toUtc(Plan(i).end_time);
            end
        end

    end
end

