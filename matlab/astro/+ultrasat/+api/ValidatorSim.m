%==========================================================================
% ULTRASAT 
%
% File:   ValidatorSim.m
% Author: Chen Tishler
% Created: 17/02/2025
% Updated: 17/03/2025
%
%==========================================================================
% https://chatgpt.com/c/67b1bc9e-869c-8012-b527-debac46e0d95

classdef ValidatorSim < handle
    % ValidatorSim - Simulates an external validation system for observation targets
    %
    % This class handles the simulation of target validation, including random delays,
    % generating validation results, and storing validation history in a JSON database.
    % It's designed for testing observation planning systems without connecting to 
    % actual validation services.

    properties
        DbFilePath          % JSON file to store validation history
        LogFileName         % Log file for messages
        Response            % Struct to store the latest validation response
    end

    methods
        function obj = ValidatorSim(DbFilePath, LogFileName)
            % Constructor for ValidatorSim
            %
            % Arguments:
            %   - DbFilePath Path to JSON file for storing validation history
            %   - LogFileName Path to log file for messages

            obj.DbFilePath = DbFilePath;
            obj.LogFileName = LogFileName;

            % Ensure the JSON file exists
            if ~isfile(obj.DbFilePath)
                fid = fopen(obj.DbFilePath, 'w');
                fwrite(fid, jsonencode(struct('validations', {}), 'PrettyPrint', true), 'char');
                fclose(fid);
            end            
        end
        

        function Result = validateTargets(obj, targets)
            % Simulates external validation for a set of targets with random delay
            %
            % Arguments:
            %   - targets Array of target structs to validate
            %
            % Return:
            %   - Result Validation response struct with results for all targets

            obj.msglog('Starting validation for %d targets...', numel(targets));
            pause(rand() * 0.5 + 2.5);  % Random delay between 0.5 to 3 seconds
            
            % Create a new response struct and update the Response property
            obj.Response = obj.newResponse();

            % Generate validation results for targets
            target_results = obj.generateTargetResults(targets);

            % Populate the task part of the response
            obj.Response.task = obj.newResponseTask(targets, target_results);
            obj.Response.status = 'approved';

            obj.msglog('Validation completed successfully.');

            % Save to validations database
            obj.appendValidationToDb(targets, obj.Response);

            Result = obj.Response;
        end


        function response = newResponse(obj)
            % Creates a new response struct with a status field
            response = struct(...
                'validation_time', ultrasat.api.ModelBase.nowUtc(), ...
                'status', 'pending', ... % Default status
                'task', struct() ...     % Task data will be populated later
            );
        end


        function task = newResponseTask(obj, targets, target_results)
            % Creates the task struct for the response
            task = struct(...
                'task_id', sprintf('tsk_%s', datestr(api.ModelBase.nowUtc, 'yyyymmddHHMMSS')), ...
                'start_time', datestr(targets(1).start_time, 'yyyy-mm-ddTHH:MM:SS.FFFZ'), ...
                'estimated_end_time', datestr(targets(end).start_time + seconds(5), 'yyyy-mm-ddTHH:MM:SS.FFFZ'), ...
                'task_type', 'validation', ...
                'target_count', numel(targets), ...
                'first_modified_target', '', ... % Set if modified targets exist
                'targets', target_results ...
            );
        end


        function targets = generateTargetResults(obj, targets)
            % Generate validation results for each target.
            obj.msglog('Generating validation results for targets...');
            slew_time = 5;  % Fixed slew time for now
            for i = 1:numel(targets)
                targets(i).target_id = sprintf('trg_%s', datestr(targets(i).start_time, 'yyyymmddHHMMSS'));
                targets(i).estimated_end_time = targets(i).start_time + seconds(targets(i).image_count * (targets(i).exposure + slew_time));
                targets(i).status = 'approved';  %obj.getRandomStatus();
                targets(i).power_status = 'ok';
                targets(i).obrd_status = 'ok';
                targets(i).coord_roll = 0;
                targets(i).warning = struct();

                warn = false;
                if warn
                    targets(i).warning = struct(...
                        'time', datestr(api.ModelBase.nowUtc, 'yyyy-mm-ddTHH:MM:SS.FFFZ'), ...
                        'duration', '100', ...
                        'type', 'NegativePowerBalance', ...
                        'details', 'Potential power issue during imaging.' ...
                    );
                end
                obj.msglog('Target %d validated: status=%s', i, targets(i).status);
            end
        end


        function appendValidationToDb(obj, targets, response)
            % Appends validation request and response to the JSON 'database' file.
            % - Handles cases where the file does not exist or is empty.
            % - Ensures `logData.validations` exists before appending.

            logFilePath = obj.DbFilePath;
            logData = struct(); % Initialize as empty struct in case file doesn't exist
            
            % Try to load existing log if file exists
            if exist(logFilePath, 'file') == 2
                fid = fopen(logFilePath, 'r');
                if fid ~= -1
                    raw = fread(fid, inf, 'char');
                    fclose(fid);
                    if ~isempty(raw) % Ensure file is not empty
                        try
                            logData = jsondecode(char(raw'));
                        catch
                            obj.msglog('Warning: Failed to decode JSON. Creating new log.');
                            logData = struct(); % Reset log if decoding fails
                        end
                    end
                else
                    obj.msglog('Warning: Unable to open log file for reading.');
                end
            end
            
            % Ensure `validations` field exists in logData
            if ~isfield(logData, 'validations') || ~iscell(logData.validations)
                logData.validations = {}; % Initialize as empty cell array
            end
            
            % Assign serial number
            validationSerial = numel(logData.validations) + 1;
            validationEntry = struct(...
                'serial', validationSerial, ...
                'timestamp', ultrasat.api.ModelBase.nowUtcStr(), ...
                'input', targets, ...
                'output', response ...
            );
            
            % Append to log
            logData.validations{end+1} = validationEntry;
            
            % Save updated log
            fid = fopen(logFilePath, 'w');
            if fid ~= -1
                fwrite(fid, jsonencode(logData, 'PrettyPrint', true), 'char');
                fclose(fid);
                obj.msglog('Validation %d saved to log file.', validationSerial);
            else
                obj.msglog('Error: Failed to open log file for writing.');
            end
        end


        function status = getRandomStatus(obj)
            % Randomly select a status for the target.
            statuses = {'approved', 'not_approved', 'approved_warning'};
            status = statuses{randi(3)};
            obj.msglog('Random status generated: %s', status);
        end


        function msglog(obj, varargin)
            % Logs a formatted message to the log file
            ultrasat.api.ApiUtils.msglog(obj.LogFileName, 'ValidatorSim', varargin{:});
        end
    end


    methods(Static)
        function response = createSampleValidationResponse()
            % Create sample validation response, used for development of 'uplanner' class
            targets(1) = struct(...
                'target_id', 'trg_20250302120000', ...
                'target_type', 'imaging', ...
                'coord_ra', 111, ...
                'coord_dec', 222, ...
                'exposure', 300, ...
                'image_count', 1, ...
                'tiles', '1,2,3,4', ...
                'start_time', '2025-05-28T00:00:00.0Z', ...
                'estimated_end_time', '2025-05-28T00:05:00.0Z', ...
                'coord_roll', 0, ...
                'status', 'approved', ...
                'power_status', 'ok', ...
                'obrd_status', 'ok', ...
                'warning', struct(...
                    'time', '2025-05-27T23:55:00.0Z', ...
                    'duration', 100, ...
                    'type', 'NegativePowerBalance', ...
                    'details', 'Potential power issue during imaging.' ...
                ) ...
            );
        
            % Second sample target
            targets(2) = struct(...
                'target_id', 'trg_20250302121000', ...
                'target_type', 'imaging', ...
                'coord_ra', 115, ...
                'coord_dec', 225, ...
                'exposure', 350, ...
                'image_count', 2, ...
                'tiles', '5,6,7,8', ...
                'start_time', '2025-06-15T14:00:00.0Z', ...
                'estimated_end_time', '2025-06-15T14:10:00.0Z', ...
                'coord_roll', 5, ...
                'status', 'approved_warning', ...
                'power_status', 'warning', ...
                'obrd_status', 'ok', ...
                'warning', struct(...
                    'time', '2025-06-15T13:50:00.0Z', ...
                    'duration', 120, ...
                    'type', 'BalanceDuringImaging', ...
                    'details', 'Battery discharge detected.' ...
                ) ...
            );
        
            response = struct('start_time', '2025-05-28T00:00:00.0Z', ...
                'target_count', 2, ...
                'targets', targets);
        end

    end

end
