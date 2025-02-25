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
        LogFileName         %
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
        

        function response = validateTargets(obj, targets)
            % Simulate external validation with random delay and generate validation results.
            obj.msglog('Starting validation for %d targets...', numel(targets));
            pause(rand() * 0.5 + 0.5);  % Random delay between 0.5 to 3 seconds
            
            response = struct();
            response.task = struct(...
                'task_id', sprintf('tsk_%s', datestr(now, 'yyyymmddHHMMSS')), ...
                'start_time', datestr(targets(1).start_time, 'yyyy-mm-ddTHH:MM:SS.FFFZ'), ...
                'estimated_end_time', datestr(targets(end).start_time + seconds(5), 'yyyy-mm-ddTHH:MM:SS.FFFZ'), ...  % @FIX
                'task_type', 'validation', ...
                'target_count', numel(targets), ...
                'first_modified_target', '', ... %sprintf('trg_%s', datestr(now, 'yyyymmddHHMMSS')), ...
                'targets', obj.generateTargetResults(targets) ...
            );
            obj.msglog('Validation completed successfully.');

            % Save to validations database
            obj.appendValidationToDb(targets, response);            
        end


        function targets = generateTargetResults(obj, targets)
            % Generate validation results for each target.
            obj.msglog('Generating validation results for targets...');
            slew_time = 5;  % Fixed slew time for now
            for i = 1:numel(targets)
                targets(i).target_id = sprintf('trg_%s', datestr(targets(i).start_time, 'yyyymmddHHMMSS'));
                targets(i).estimated_end_time = datestr(datenum(targets(i).start_time) + (targets(i).image_count * (seconds(targets(i).exposure) + slew_time) / (24*3600)), 'yyyy-mm-ddTHH:MM:SS.FFFZ');
                targets(i).status = obj.getRandomStatus();
                targets(i).power_status = 'ok';
                targets(i).obrd_status = 'ok';
                targets(i).warning = struct(...
                    'time', datestr(now, 'yyyy-mm-ddTHH:MM:SS.FFFZ'), ...
                    'duration', '100', ...
                    'type', 'NegativePowerBalance', ...
                    'details', 'Potential power issue during imaging.' ...
                );
                obj.msglog('Target %d validated: status=%s', i, targets(i).status);
            end
        end


        function appendValidationToDb(obj, targets, response)
            % Appends validation request and response to the JSON log file.

            % Load existing log
            fid = fopen(obj.DbFilePath, 'r');
            raw = fread(fid, inf, 'char');
            fclose(fid);
            logData = jsondecode(char(raw'));

            % Assign serial number
            validationSerial = numel(logData.validations) + 1;
            validationEntry = struct(...
                'serial', validationSerial, ...
                'timestamp', datestr(now, 'yyyy-mm-ddTHH:MM:SS.FFFZ'), ...
                'input', targets, ...
                'output', response ...
            );

            % Append to log
            logData.validations{end+1} = validationEntry;

            % Save updated log
            fid = fopen(obj.DbFilePath, 'w');
            fwrite(fid, jsonencode(logData, 'PrettyPrint', true), 'char');
            fclose(fid);

            obj.msglog('Validation %d saved to log file.', validationSerial);
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
