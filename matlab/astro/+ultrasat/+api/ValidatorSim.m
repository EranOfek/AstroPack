%==========================================================================
% ULTRASAT 
%
% File:   ValidatorSim.m
% Author: Chen Tishler
% Created: 17/02/2025
% Updated: 17/02/2025
%
%==========================================================================
% https://chatgpt.com/c/67b1bc9e-869c-8012-b527-debac46e0d95

classdef ValidatorSim < handle

    properties
        DbFilePath          % JSON file to store validation history
        LogFileName         % Log file for messages
        Response            % Struct to store the latest validation response
    end

    methods
        function obj = ValidatorSim(DbFilePath, LogFileName)
            % Constructor for ValidatorSim

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
            % Simulate external validation with random delay and generate validation results.
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
            % Appends validation request and response to the JSON log file.
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
                'timestamp', datestr(api.ModelBase.nowUtc, 'yyyy-mm-ddTHH:MM:SS.FFFZ'), ...
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
            % Logs a formatted message
            api.ApiUtils.msglog(obj.LogFileName, 'ValidatorSim', varargin{:});
        end
    end
end
