%==========================================================================
% ULTRASAT 
%
% File:   ValidatorSim.m
% Author: Chen Tishler
% Created: 17/02/2025
% Updated: 17/02/2025
%
%==========================================================================

classdef ValidatorSim < handle
    methods
        function obj = ValidatorSim()
            % Constructor for ValidatorSim
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


        function status = getRandomStatus(obj)
            % Randomly select a status for the target.
            statuses = {'approved', 'not_approved', 'approved_warning'};
            status = statuses{randi(3)};
            obj.msglog('Random status generated: %s', status);
        end


        function msglog(obj, varargin)
            % Logs a formatted message to the console.
            fprintf('ValidatorSim: ');
            fprintf(varargin{:});
            fprintf('\n');
        end
    end
end
