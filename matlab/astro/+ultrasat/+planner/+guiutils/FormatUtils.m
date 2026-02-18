%==========================================================================
% Project     : ULTRASAT Planner
% File        : +planner/+guiutils/FormatUtils.m
% Author      : Chen Tishler
% Created     : 18/02/2026
% Updated     : 18/02/2026
% Description : Central class to hold common application data
%==========================================================================

classdef FormatUtils < handle

    methods(Static)

        function Result = DateTime2Str(dt)
            % Convert datetime object to string 'yyyy-MM-dd HH:mm:ss'
            if isempty(dt)
                Result = '';
            else
                Result = datestr(dt, 'yyyy-mm-dd HH:MM:SS');
            end
        end

        
        function str = Duration2Str(dur, showSeconds)
            % Convert a duration value to string HH:MM or HH:MM:SS
            % :param dur: duration value
            % :param showSeconds: true for HH:MM:SS, false for HH:MM
        
            if nargin < 3
                showSeconds = false;
            end
        
            % Handle empty or invalid input
            if isempty(dur) || ~isduration(dur)
                str = '';
                return;
            end
        
            % Convert to total seconds
            totalSeconds = seconds(dur);
            if totalSeconds < 0
                totalSeconds = 0;
            end
        
            % Compute hours, minutes, seconds
            hh = floor(totalSeconds / 3600);
            mm = floor(mod(totalSeconds, 3600) / 60);
            ss = floor(mod(totalSeconds, 60));
        
            if showSeconds
                str = sprintf('%02d:%02d:%02d', hh, mm, ss);
            else
                str = sprintf('%02d:%02d', hh, mm);
            end
        end



        function Result = num2Str(Value)
            % Convert number to string
            if ~isempty(Value)
                Result = num2str(Value);
            else
                Result = '';
            end
        end


        function Result = ra2Str(Value)
            % Convert RA to string
            % @Todo - need to support sexa, etc.
            if ~isempty(Value)
                Result = sprintf('%f', Value);
            else
                Result = '';
            end
        end


        function Result = dec2Str(Value)
            % Convert Dec to string
            % @Todo - need to support sexa, etc.
            if ~isempty(Value)
                Result = sprintf('%f', Value);
            else
                Result = '';
            end
        end


        function Result = length2Str(array)
            % Convert array length to string as 'len: n'
            if isempty(array)
                Result = 'len: 0';
                return;
            end
        
            % Unwrap single-cell container
            if iscell(array) && numel(array) == 1
                inner = array{1};
                Result = sprintf('len: %d', numel(inner));
            else
                Result = sprintf('len: %d', numel(array));
            end
        end


        function charArray = cell2Str(cellArray)
            % Convert a cell array to a comma-separated character array

            % Convert elements to strings
            strArray = cellfun(@num2str, cellArray, 'UniformOutput', false);

            % Join elements with commas and convert to char array
            charArray = char(strjoin(strArray, ','));
        end


        function htmlStr = jsonToHtml(jsonData)
            % Converts a JSON string or struct to HTML with syntax highlighting

            % Convert struct to JSON if needed
            if isstruct(jsonData) || iscell(jsonData)
                jsonData = jsonencode(jsonData, 'PrettyPrint', true);
            end

            % Escape HTML special characters
            jsonData = strrep(jsonData, '&', '&amp;');
            jsonData = strrep(jsonData, '<', '&lt;');
            jsonData = strrep(jsonData, '>', '&gt;');

            % Apply syntax highlighting
            jsonData = regexprep(jsonData, '"(.*?)"(\s*:\s*)', '<span style="color:blue;">"$1"</span>$2'); % Keys
            jsonData = regexprep(jsonData, '(:\s*)(\d+)', '$1<span style="color:green;">$2</span>'); % Numbers
            jsonData = regexprep(jsonData, '(:\s*)"(.*?)"', '$1<span style="color:maroon;">"$2"</span>'); % Strings
            jsonData = regexprep(jsonData, '(:\s*)(true|false|null)', '$1<span style="color:purple;">$2</span>'); % Boolean/Null

            % Wrap in preformatted HTML block
            htmlStr = sprintf('<pre style="background:#f5f5f5; padding:10px; border:1px solid #ddd;">%s</pre>', jsonData);
        end

    end

end
