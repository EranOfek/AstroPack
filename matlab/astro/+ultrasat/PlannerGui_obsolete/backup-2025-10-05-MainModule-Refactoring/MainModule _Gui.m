%==========================================================================
% ULTRASAT Planner
%
% File:   +planner/+gui/MainModule.m
% Author:  Chen Tishler
% Created: 07/01/2025
% Updated: 28/01/2025
% Title:   
%==========================================================================
% Debug:
%   DM = ultrasat.planner.gui.MainModule()
%

classdef MainModule < ultrasat.api.Loggable
    % This class serves like a DataModule in Delphi.
    

    methods

        % =================================================================

        function color = getValidationStatusColor(obj, status)
            % Returns text color (RGB) based on the validation status
        
            % Convert status to lowercase to ensure case insensitivity
            status = lower(string(status));
        
            switch status
                case ""  % Empty status (Default black)
                    color = [0 0 0]; % Black
                case "approved"
                    color = [0 0.5 0]; % Green
                case "warning"
                    color = [0.5 0 0.5]; % Purple
                case "failed"
                    color = [0.8 0 0]; % Red
                otherwise
                    color = [0.8 0 0]; % Default to Red for unknown statuses
            end
        end


        function color = getValidationStatusBackgroundColor(obj, status)
            % Returns background color (RGB) for black text based on the validation status
        
            % Convert status to lowercase to ensure case insensitivity
            status = lower(string(status));
        
            switch status
                case ""  % Empty status (Light yellowish background)
                    color = [1.00 0.99 0.82]; % Light pastel yellow
                case "approved"
                    color = [0.85 1 0.85]; % Light green
                case "warning"
                    color = [0.95 0.85 1]; % Light purple
                case "failed"
                    color = [1 0.85 0.85]; % Light red
                otherwise
                    color = [1 0.85 0.85]; % Default to Light Red for unknown statuses
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
            % Return the value of text field
            Result = strtrim(Value);
        end

        function Result = getFieldNum(obj, Value)
            % Return the value of numerical field
            if ischar(Value) || isstring(Value)
                % If input is a string, trim and convert to a number
                s = strtrim(Value);       % Remove leading and trailing spaces
                Result = str2double(s);   % Convert to double (handles invalid strings as NaN)
            elseif isnumeric(Value)
                % If input is already numeric, return it directly
                Result = Value;
            else
                % Handle unsupported input types
                error('Input must be a string, character array, or numeric.');
            end
        end

        function Result = getFieldTitle(obj, Value)
            % Return the value of text field            
            Result = strtrim(Value);
        end

        function Result = getFieldUniqueTargetName(obj, Value)
            % Return the value of text field
            Result = strtrim(Value);
        end

        function Result = getFieldRA(obj, Value)
            % Return the value of RA text field as double
            % @Todo - Need to support Sexa - Eran - convert.
            Result = str2double(strtrim(Value));
        end        

        function Result = getFieldDec(obj, Value)
            % Return the value of Dec text field as double
            % @Todo - Need to support Sexa - Eran - convert.
            Result = str2double(strtrim(Value));
        end        


        function Result = getFieldDateTime(obj, Value)
            % Return the value of date-time text field as a datetime object
            if ~isempty(Value)
                try
                    % First, try parsing as ISO 8601 format
                    Result = datetime(Value, 'InputFormat', 'yyyy-MM-dd''T''HH:mm:ss.SSSSSS''Z', 'TimeZone', 'UTC');
                catch
                    try
                        % If it fails, try parsing as 'yyyy-MM-dd HH:mm:ss'
                        Result = datetime(Value, 'InputFormat', 'yyyy-MM-dd HH:mm:ss', 'TimeZone', 'UTC');
                    catch
                        % If parsing fails, return an empty datetime with UTC
                        warning('Invalid date format: %s', Value);
                        Result = datetime([], 'TimeZone', 'UTC');
                    end
                end
            else
                Result = [];
            end
        end
        

        function Result = getFieldDuration(obj, Value)
            % Return the value of a duration text field as a duration object
            % If input is a valid integer, interpret it as seconds
            
            if isempty(Value)
                Result = [];
                return;
            end
        
            % Convert to string and trim whitespace
            strValue = strtrim(string(Value));
        
            % Try converting to a number
            numValue = str2double(strValue);
        
            if ~isnan(numValue) && mod(numValue, 1) == 0
                % Valid integer: interpret as seconds
                Result = seconds(numValue);
            else
                % Otherwise, try parsing as duration string (e.g., "00:10:00")
                Result = duration(strValue);
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

        % -------------------------------------------------------------------

        function name = extractNameFromDisplayString(obj, displayStr)
            % Extracts the name part from "id - name" format
            parts = strsplit(displayStr, ':');
            if numel(parts) >= 2
                name = strtrim(parts{1});  % Take just the name part
            else
                name = strtrim(displayStr);  % Fallback: return whole string
            end
        end


        function name = extractTitleFromDisplayString(obj, displayStr)
            % Extracts the name part from "id - name" format
            parts = strsplit(displayStr, ':');
            if numel(parts) >= 2
                name = strtrim(parts{2});  % Take just the name part
            else
                name = strtrim(displayStr);  % Fallback: return whole string
            end
        end        

        
    end

end
