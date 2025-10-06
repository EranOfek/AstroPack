%==========================================================================
% ULTRASAT Planner
%
% File:   +planner/+gui/GuiHelper.m
% Author:  Chen Tishler
% Created: 07/01/2025
% Updated: 05/10/2025
% Title:   
%==========================================================================

classdef GuiHelper < ultrasat.api.Loggable
    % This class serves like a DataModule in Delphi.
    
    properties  
    end
    

    methods
        function obj = GuiHelper()
            % Constructor
            obj.msglog('GuiHelper created successfully');
        end

        % =================================================================

        function color = getValidationStatusColor(obj, status)
            % Returns text color (RGB) based on the validation status
        
            % Convert status to lowercase to ensure case insensitivity
            status = lower(string(status));
        
            switch status
                case ""  % Empty status (Default black)
                    color = [0, 0, 0]; % Black
                case "approved"
                    color = [0, 0.5, 0]; % Green
                case "warning"
                    color = [0.5, 0, 0.5]; % Purple
                case "failed"
                    color = [0.8, 0, 0]; % Red
                otherwise
                    color = [0.8, 0, 0]; % Default to Red for unknown statuses
            end
        end


        function color = getValidationStatusBackgroundColor(obj, status)
            % Returns background color (RGB) for black text based on the validation status
        
            % Convert status to lowercase to ensure case insensitivity
            status = lower(string(status));
        
            switch status
                case ""  % Empty status (Light yellowish background)
                    color = [1.00, 0.99, 0.82]; % Light pastel yellow
                case "approved"
                    color = [0.85, 1, 0.85]; % Light green
                case "warning"
                    color = [0.95, 0.85, 1]; % Light purple
                case "failed"
                    color = [1, 0.85, 0.85]; % Light red
                otherwise
                    color = [1, 0.85, 0.85]; % Default to Light Red for unknown statuses
            end
        end


        function style = getValidationStatusStyle(obj, status)
            % Returns the appropriate uistyle based on the validation status
        
            % Get the corresponding text color
            color = obj.getValidationStatusColor(status);
        
            % Create and return the style
            style = uistyle("FontColor", color);
        end       

        % =================================================================
        %                           Get UI Field Values
        % =================================================================

        function Result = getFieldText(obj, Value)
            % Return trimmed text field value as char, empty if invalid
            try
                if isstring(Value), Value = char(Value); end
                if ischar(Value)
                    Result = strtrim(Value);
                else
                    obj.msglog(sprintf('getFieldText: unsupported type %s', class(Value)));
                    Result = '';
                end
            catch ME
                obj.msglog(sprintf('getFieldText: error %s', ME.message));
                Result = '';
            end
        end


        function Result = getFieldNum(obj, Value)
            % Return numeric field value as double, NaN if invalid
            try
                if isnumeric(Value)
                    Result = Value;
                elseif isstring(Value) || ischar(Value)
                    Result = str2double(strtrim(char(Value)));
                else
                    obj.msglog(sprintf('getFieldNum: unsupported type %s', class(Value)));
                    Result = NaN;
                end
            catch ME
                obj.msglog(sprintf('getFieldNum: error %s', ME.message));
                Result = NaN;
            end
        end

        
        function Result = getFieldTitle(obj, Value)
            % Return trimmed title field value as char, empty if invalid
            try
                if isstring(Value), Value = char(Value); end
                if ischar(Value)
                    Result = strtrim(Value);
                else
                    obj.msglog(sprintf('getFieldTitle: unsupported type %s', class(Value)));
                    Result = '';
                end
            catch ME
                obj.msglog(sprintf('getFieldTitle: error %s', ME.message));
                Result = '';
            end
        end

        
        function Result = getFieldUniqueTargetName(obj, Value)
            % Return trimmed unique target name field as char, empty if invalid
            try
                if isstring(Value), Value = char(Value); end
                if ischar(Value)
                    Result = strtrim(Value);
                else
                    obj.msglog(sprintf('getFieldUniqueTargetName: unsupported type %s', class(Value)));
                    Result = '';
                end
            catch ME
                obj.msglog(sprintf('getFieldUniqueTargetName: error %s', ME.message));
                Result = '';
            end
        end

        %------------------------------------------------------------------        
        
        function Result = getFieldRA(obj, Value)
           % Return RA text field as double, NaN if invalid
           % @Todo - support Sexa
           try                
               if isstring(Value) || ischar(Value)
                   Result = str2double(strtrim(char(Value)));
               elseif isnumeric(Value)
                   Result = Value;
               else
                   obj.msglog(sprintf('getFieldRA: unsupported type %s', class(Value)));
                   Result = NaN;
               end
           catch ME
               obj.msglog(sprintf('getFieldRA: error %s', ME.message));
               Result = NaN;
           end
        end

 
        function Result = getFieldDec(obj, Value)
            % Return Dec text field as double, NaN if invalid
            % @Todo - support Sexa
            try
                if isstring(Value) || ischar(Value)
                    Result = str2double(strtrim(char(Value)));
                elseif isnumeric(Value)
                    Result = Value;
                else
                    obj.msglog(sprintf('getFieldDec: unsupported type %s', class(Value)));
                    Result = NaN;
                end
            catch ME
                obj.msglog(sprintf('getFieldDec: error %s', ME.message));
                Result = NaN;
            end
        end
        
        %------------------------------------------------------------------

        function Result = getFieldDateTime(obj, Value)
            % Return the value of a date-time text field as a datetime object.
            %
            % Expected Input:
            %   Value - A character vector, string scalar, or datetime object.
            %           Example: '2025-10-05T12:34:56.123456Z' or '2025-10-05 12:34:56'
            %
            % Output:
            %   Result - datetime object with TimeZone='UTC'.
            %            Returns empty datetime([]) with UTC if input is invalid.
            %
            % Notes:
            %   - Tries ISO 8601 first (with microseconds), then 'yyyy-MM-dd HH:mm:ss'.
            %   - Handles char, string, datetime, and numeric gracefully.
            %   - Logs and returns empty datetime on any failure.
        
            try
                % Handle empty input
                if isempty(Value)
                    Result = datetime([], 'TimeZone', 'UTC');
                    return;
                end
        
                % If already datetime, just ensure it's in UTC
                if isdatetime(Value)
                    if isempty(Value)
                        Result = datetime([], 'TimeZone', 'UTC');
                    else
                        if isempty(Value.TimeZone)
                            Value.TimeZone = 'UTC';
                        else
                            Value = datetime(Value, 'TimeZone', 'UTC');
                        end
                        Result = Value;
                    end
                    return;
                end
        
                % Convert string to char for consistent parsing
                if isstring(Value)
                    Value = char(Value);
                elseif isnumeric(Value)
                    % Interpret numeric as MATLAB datenum
                    try
                        Result = datetime(Value, 'ConvertFrom', 'datenum', 'TimeZone', 'UTC');
                        return;
                    catch innerME
                        obj.msglog(sprintf('getFieldDateTime: numeric conversion failed: %s', innerME.message));
                        Result = datetime([], 'TimeZone', 'UTC');
                        return;
                    end
                elseif ~ischar(Value)
                    obj.msglog(sprintf('getFieldDateTime: unsupported input type %s', class(Value)));
                    Result = datetime([], 'TimeZone', 'UTC');
                    return;
                end
        
                % Trim
                strVal = strtrim(Value);
                if isempty(strVal)
                    Result = datetime([], 'TimeZone', 'UTC');
                    return;
                end
        
                % Try parsing ISO 8601 format
                try
                    Result = datetime(strVal, ...
                        'InputFormat', 'yyyy-MM-dd''T''HH:mm:ss.SSSSSS''Z', ...
                        'TimeZone', 'UTC');
                    return;
                catch
                    % Try parsing without 'T' and 'Z'
                    try
                        Result = datetime(strVal, ...
                            'InputFormat', 'yyyy-MM-dd HH:mm:ss', ...
                            'TimeZone', 'UTC');
                        return;
                    catch innerME
                        obj.msglog(sprintf('getFieldDateTime: failed to parse "%s": %s', strVal, innerME.message));
                        Result = datetime([], 'TimeZone', 'UTC');
                        return;
                    end
                end
        
            catch ME
                obj.msglog(sprintf('getFieldDateTime: error processing input (%s): %s', class(Value), ME.message));
                Result = datetime([], 'TimeZone', 'UTC');
            end
        end
        

        function Result = getFieldDuration(obj, Value)
            % getFieldDuration Convert various textual/numeric duration inputs to a duration object.
            %
            % Expected Input:
            %   Value - A character vector, string scalar, or numeric value.
            %           Examples: '300', "00:10:00", 120, '3 hr', '70 min', '3600 sec'
            %
            % Output:
            %   Result - A duration object. If Value is empty or invalid, returns [].
            %
            % Behavior:
            %   - Empty input → []
            %   - Numeric (scalar) → seconds(Value)
            %   - Pure integer string → seconds(str2double(Value))
            %   - 'hh:mm:ss' / 'dd:hh:mm:ss' → parsed by duration()
            %   - Natural language like '3 hr', '70 min', '3600 sec' → parsed by regexp
            %
            % Notes:
            %   - Handles char, string, numeric gracefully.
            %   - Logs errors but never throws exceptions.
        
            Result = [];
            try
                % Handle empty input
                if isempty(Value)
                    return;
                end
        
                % Normalize to char
                if isstring(Value)
                    Value = char(Value);
                elseif isnumeric(Value)
                    if isscalar(Value) && ~isnan(Value)
                        Result = seconds(Value);
                    else
                        obj.msglog('getFieldDuration: unsupported numeric input.');
                    end
                    return;
                elseif ~ischar(Value)
                    obj.msglog(sprintf('getFieldDuration: unsupported input type %s', class(Value)));
                    return;
                end
        
                % Trim whitespace
                strValue = strtrim(Value);
                if isempty(strValue)
                    return;
                end
        
                % 1. Try converting to numeric first (e.g., '300')
                numValue = str2double(strValue);
                if ~isnan(numValue) && isfinite(numValue)
                    Result = seconds(numValue);
                    return;
                end
        
                % 2. Try parsing as hh:mm:ss or dd:hh:mm:ss
                try
                    d = duration(strValue);
                    if isduration(d) && ~isempty(d)
                        Result = d;
                        return;
                    end
                catch
                    % we'll try natural units next
                end
        
                % 3. Try parsing natural language strings like "3 hr", "10 min", "3600 sec"
                %    Also supports plural like 'hours', 'minutes', etc., and decimals like '1.5 hr'
                tokens = regexp(lower(strValue), '^\s*([0-9]+(?:\.[0-9]+)?)\s*(hr|hrs|hour|hours|min|mins|minute|minutes|sec|secs|second|seconds)\s*$', 'tokens');
                if ~isempty(tokens)
                    valNum = str2double(tokens{1}{1});
                    unitStr = tokens{1}{2};
        
                    switch unitStr
                        case {'hr','hrs','hour','hours'}
                            Result = hours(valNum);
                        case {'min','mins','minute','minutes'}
                            Result = minutes(valNum);
                        case {'sec','secs','second','seconds'}
                            Result = seconds(valNum);
                        otherwise
                            obj.msglog(sprintf('getFieldDuration: unrecognized unit "%s" in "%s"', unitStr, strValue));
                            Result = [];
                    end
                    return;
                end
        
                % 4. If nothing matched
                obj.msglog(sprintf('getFieldDuration: unrecognized duration format "%s"', strValue));
        
            catch ME
                obj.msglog(sprintf('getFieldDuration: error processing input (%s): %s', class(Value), ME.message));
                Result = [];
            end
        end


        % =================================================================
        %                            Utilities
        % =================================================================

        function updateCheckboxesFromTiles(obj, app, Tiles)
            % Update the checkboxes based on the Tiles string.
            %
            % :param app: The App Designer application instance
            % :param Tiles: A string like '123' representing selected tiles
            % app.MainModule.updateCheckboxesFromTiles(ParamsApp, Plan.Tiles(Index))

            % Reset all checkboxes to false
            app.Tile1CheckBox.Value = false;
            app.Tile2CheckBox.Value = false;
            app.Tile3CheckBox.Value = false;
            app.Tile4CheckBox.Value = false;
        
            % Update based on Tiles string
            Tiles = char(Tiles);
            for tile = Tiles
                switch tile
                    case '1', app.Tile1CheckBox.Value = true;
                    case '2', app.Tile2CheckBox.Value = true;
                    case '3', app.Tile3CheckBox.Value = true;
                    case '4', app.Tile4CheckBox.Value = true;
                end
            end
        end


        function Tiles = getTilesFromCheckboxes(obj, app)
            % Construct the Tiles string from the selected checkboxes.
            %
            % :param app: The App Designer application instance
            % :return: String of selected tiles, e.g., '13'
            % Plan.Tiles(Index) = app.MainModule.getTilesFromCheckboxes(ParamsApp)

            Tiles = "";
        
            if app.Tile1CheckBox.Value, Tiles = Tiles + "1"; end
            if app.Tile2CheckBox.Value, Tiles = Tiles + "2"; end
            if app.Tile3CheckBox.Value, Tiles = Tiles + "3"; end
            if app.Tile4CheckBox.Value, Tiles = Tiles + "4"; end
        end      

        % =================================================================
        %
        % =================================================================
       
        function name = extractNameFromDisplayString(obj, displayStr)
            % Extracts the "name" part from a display string in the format "name:title".
            %
            % Expected Input:
            %   displayStr - A character vector or string in the format "name:title".
            %                Example: 'Omer:Project Lead'
            %
            % Output:
            %   name - The trimmed string before the first colon.
            %          If no colon is found or an error occurs, the entire input string is returned.
            %
            % Notes:
            %   - Internally converts string inputs to char once, then works in char mode.
            %   - Logs and returns the input if anything unexpected happens.
        
            name = '';
            try
                if isempty(displayStr)
                    return;
                end
        
                % Convert string scalar to char for consistent downstream behavior
                if isstring(displayStr)
                    displayStr = char(displayStr);
                end
        
                if ~ischar(displayStr)
                    obj.msglog(sprintf('extractNameFromDisplayString: unsupported type %s', class(displayStr)));
                    return;
                end
        
                parts = strsplit(displayStr, ':');
        
                if numel(parts) >= 1
                    name = strtrim(parts{1});
                else
                    name = strtrim(displayStr);
                end
        
            catch ME
                obj.msglog(sprintf('extractNameFromDisplayString: error processing "%s": %s', displayStr, ME.message));
                name = strtrim(displayStr);
            end
        end
        
        
        function titleStr = extractTitleFromDisplayString(obj, displayStr)
            % Extracts the "title" part from a display string in the format "name:title".
            %
            % Expected Input:
            %   displayStr - A character vector or string in the format "name:title".
            %                Example: 'Omer:Project Lead'
            %
            % Output:
            %   titleStr - The trimmed string after the first colon.
            %              If no colon is found or an error occurs, the entire input string is returned.
            %
            % Notes:
            %   - Internally converts string inputs to char once, then works in char mode.
            %   - Logs and returns the input if anything unexpected happens.
        
            titleStr = '';
            try
                if isempty(displayStr)
                    return;
                end
        
                % Convert string scalar to char for consistent downstream behavior
                if isstring(displayStr)
                    displayStr = char(displayStr);
                end
        
                if ~ischar(displayStr)
                    obj.msglog(sprintf('extractTitleFromDisplayString: unsupported type %s', class(displayStr)));
                    return;
                end
        
                parts = strsplit(displayStr, ':');
        
                if numel(parts) >= 2
                    titleStr = strtrim(parts{2});
                else
                    titleStr = strtrim(displayStr);
                end
        
            catch ME
                obj.msglog(sprintf('extractTitleFromDisplayString: error processing "%s": %s', displayStr, ME.message));
                titleStr = strtrim(displayStr);
            end
        end
                
    end

end
