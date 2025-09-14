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

classdef MainModule < handle
    % This class serves like a DataModule in Delphi.
    
    properties
        ApiClient               % MissionApiClient/MissionApiSim instance
        Preferences             % ultrasat.planner.gui.Preferences()
        PreferencesFileName     %
        NamespaceId             % 'OPER' for operationl, lowercase id for simulators ('sim01' etc.)
        NamespaceDisplay        % String as 'Id - Name'
        NamespaceDisplayList    % List of available namespaces other than OPER as 'Id - Name'
        UserName                % Current user
        MainApp                 % AppDesigner main window - ultrasat.planner.gui.PlannerMain
        LoggerApp               % ultrasat.planner.gui.Logger
        
        PlanType                % Current plan type: HCS, LCS, AllSS, DDT, TOO (= ultrasat.planner.uplanner.Type)
        Planner                 % instance of ultrasat.planner.uplanner
        PlanData                % instance of ultrasat.api.PlanData, same as ApiClient.PlanData
        AllowEdit               % False for read-only mode

        % Status
        StatusText              % Status text for display        
        CurrentStatus           % 'OK', 'Error', 'Warning'

        %
        Modified = false;       % True after data is being modified        
        AfterBuild = false;     %
        PlannerPath             %
        DebugPath               % Folder of debug files, such as saved .mat files
        BaseDataDir             % uplanner constructor param
        LogFileName             %
        AppUtils                %
    end
    

    methods
        function obj = MainModule()
            % Constructor
            disp('app.MainModule');
                       
            % Get namespace from O/S env
            % setenv('SOC_NAMESPACE_ID', 'OPER')
            % setenv('SOC_NAMESPACE_ID', 'SIM')
            obj.NamespaceId = getenv('SOC_NAMESPACE_ID');
            if isempty(obj.NamespaceId)
                obj.NamespaceId = 'OPER';
            end

            % @Future - Need to fix it on linux? or keep it like this?
            obj.BaseDataDir = '~/matlab/data/ULTRASAT/';
            obj.PlannerPath = '~/matlab/data/ULTRASAT/Planner/';
            if ispc
                obj.BaseDataDir = fullfile(getenv('ASTROPACK_DATA_PATH'), 'ULTRASAT');
                obj.PlannerPath = fullfile(obj.BaseDataDir, 'PlannerGUI');
            end

            if ~exist(obj.PlannerPath, 'dir')
                mkdir(obj.PlannerPath);
            end
            obj.DebugPath = obj.PlannerPath;
            obj.LogFileName = fullfile(obj.PlannerPath, 'planner.log');
            obj.msglog('MainModule started');

            %
            obj.PreferencesFileName = fullfile(obj.PlannerPath, 'preferences.json');
            obj.Preferences = ultrasat.planner.gui.Preferences(obj.PreferencesFileName);
            obj.Preferences.load();

            % Setup ApiClient %%%%%
            UseSim = true;
            if UseSim
                obj.msglog('Creating ApiClient as api.MissionClientSim');
                obj.ApiClient = ultrasat.api.MissionApiSim('LogFileName', obj.LogFileName);
            else
                obj.msglog('Creating ApiClient as api.MissionClient');
                obj.ApiClient = ultrasat.api.MissionApiClient('LogFileName', obj.LogFileName);
                obj.ApiClient.ApiUrl = 'http://localhost:8215';                          
            end

            % Operational - When starting Planner from OPER, this is the
            % only option for the user, otherwise get the namespace list
            % from the server
            if strcmp(obj.NamespaceId, 'OPER')
                obj.NamespaceDisplay = 'OPERATIONAL';
            else
                response = obj.ApiClient.getNamespaceList();
                if response.ok
                    obj.NamespaceDisplayList = response.display_list;
                end
            end
          
            % Create instance of AppUtils
            obj.msglog('MainModule created successfully');
        end


        function Result = login(obj, UserName, Password, Namespace)
            % Connect & login to server
            obj.UserName = [];
            obj.NamespaceId = [];
            Result = false;
            ANamespaceId = obj.extractNameFromDisplayString(Namespace);
            response = obj.ApiClient.login(UserName, Password, ANamespaceId);
            if response.ok
                obj.UserName = UserName;
                obj.NamespaceId = ANamespaceId;
                obj.NamespaceDisplay = Namespace;
                obj.ApiClient.NamespaceId = obj.NamespaceId;
                Result = true;
            end
        end


        function Result = logout(obj)
            % Logout from server
            if isempty(obj.UserName)
                Result = true;
                return;
            end
            Result = false;
            response = obj.ApiClient.logout(obj.UserName);

            % Currently we do not check response.ok, so even if logout
            % failed (why?) we clear UserName, leave NamespaceId without change
            obj.UserName = [];
            %obj.NamespaceId = [];
            Result = true;
        end        


        function setPlanner(obj, Planner)
            % Set the current Planner object & type
            obj.msglog(sprintf('setPlanner: %s', Planner.Type));
            obj.Planner = Planner;
            obj.PlanType = Planner.Type;
            Planner.Mclient = obj.ApiClient;
        end

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
        

        function Result = getFieldDateTime0(obj, Value)
            % Return the value of date-time text field as datetime object
            if ~isempty(Value)
                Result = datetime(Value, 'TimeZone', 'UTC');
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

        function Result = DateTime2Str(obj, dt)
            % Convert datetime object to string 'yyyy-MM-dd HH:mm:ss'
            if isempty(dt)
                Result = '';
            else
                Result = datestr(dt, 'yyyy-mm-dd HH:MM:SS');
            end
        end


        function Result = num2Str(obj, Value)
            % Convert number to string
            if ~isempty(Value)
                Result = num2str(Value);
            else
                Result = '';
            end
        end

        function Result = ra2Str(obj, Value)
            % Convert RA to string
            % @Todo - need to support sexa, etc.
            if ~isempty(Value)            
                Result = sprintf('%f', Value);           
            else
                Result = '';
            end                
        end


        function Result = dec2Str(obj, Value)
            % Convert Dec to string
            % @Todo - need to support sexa, etc.            
            if ~isempty(Value)            
                Result = sprintf('%f', Value);
            else
                Result = '';
            end                
        end

        function Result = length2Str(obj, array)
            % Convert array length to string as 'len: n'
            if ~isempty(array)
                Result = sprintf('len: %d', length(array));
            else
                Result = 'len: 0';
            end
        end

        function charArray = cell2Str(obj, cellArray)
            % Convert a cell array to a comma-separated character array
            
            % Convert elements to strings
            strArray = cellfun(@num2str, cellArray, 'UniformOutput', false);
            
            % Join elements with commas and convert to char array
            charArray = char(strjoin(strArray, ','));
        end
        

        function Value = safeStr(obj, s)
            if isempty(s)
                Value = '';
            else
                Value = string(s);
            end
        end
 
        % =================================================================

        function setModified(obj)
            % Set the Modified flag
            obj.Modified = true;
        end

        function clearModified(obj)
            % Clear the Modified flag
            obj.Modified = false;
        end

        function clearStatus(obj)
            % Clear current status fields
            obj.CurrentStatus = [];
            obj.StatusText = [];
            if ~isempty(obj.PlanData)
                obj.PlanData.clearStatus();
            end
        end


        function setStatus(obj, NewStatus, NewText)
            % Set/append current status

            % Define the priority levels of each status
            StatusLevels = struct('OK', 1, 'Warning', 2, 'Error', 3);
            
            % Ensure the new status is one of the allowed values, else treat it as 'Error'
            if ~isfield(StatusLevels, NewStatus)
                NewStatus = 'Error'; % Treat any other status as 'Error'
            end
            
            % If CurrentStatus is empty, default it to 'OK'
            if isempty(obj.CurrentStatus)
                obj.CurrentStatus = 'OK';
            end
            
            % Compare the levels and update only if NewStatus is more severe
            if StatusLevels.(NewStatus) > StatusLevels.(obj.CurrentStatus)
                obj.CurrentStatus = NewStatus;
            end
            
            NewText = sprintf('%s %s', api.ModelBase.nowUtcStr(), NewText);
            
            % Append new text to StatusText
            if isempty(obj.StatusText)
                obj.StatusText = NewText;
            else
                obj.StatusText = sprintf('%s;  %s', obj.StatusText, NewText);
            end
        end

        % =================================================================
        %
        % =================================================================        

        function msglog(obj, varargin)
            %
            api.ApiUtils.msglog(obj.LogFileName, 'Planner', varargin{:});
        end

        function msgex(obj, msg, ME, varargin)
            % Log exception with message
            api.ApiUtils.logException(obj.LogFileName, sprintf('Planner: %s', msg), ME, false, varargin{:});
        end      

        % =================================================================
        %                            Utilities
        % =================================================================
        function Result = loadTableFromCsvText(obj, CsvText)
            % Read the table from the temporary file
            tempFile = [tempname, '.csv'];
            fid = fopen(tempFile, 'w');
            fwrite(fid, CsvText);
            fclose(fid);
            Result = readtable(tempFile);
            delete(tempFile);
        end


        function Result = getSortedRowNumbers(obj, Data)
            % Extract row indices for non-empty 'Order' values, sorted by 'Order'
            
            % Convert to cell array if necessary (handles both strings and chars)
            if iscell(Data.Order) || isstring(Data.Order)
                % Trim whitespace and convert empty strings to NaN for filtering
                trimmedOrder = strtrim(Data.Order);
                isValid = ~strcmp(trimmedOrder, "") & ~strcmp(trimmedOrder, " ");  % Check for truly empty strings
                Data.Order(~isValid) = NaN;  % Replace empty strings with NaN
            end
        
            % Find non-empty (non-NaN) rows
            nonEmptyRows = find(~isnan(Data.Order));
        
            % Sort by 'Order' column
            [~, sortedIdx] = sort(Data.Order(nonEmptyRows));
        
            % Return sorted row indices
            Result = nonEmptyRows(sortedIdx);
        end
        

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

        function createPlanData(obj)
            % Create new instance
            obj.PlanData = ultrasat.api.PlanData();
            obj.ApiClient.PlanData = obj.PlanData;
        end


        function setPlanData(obj, Data)
            % Set PlanData
            obj.PlanData = Data;

            % Link current instance to ApiClient
            obj.ApiClient.PlanData = obj.PlanData;
            if ~isempty(obj.PlanData.planner)
                obj.setPlanner(obj.PlanData.planner);
            end
        end        


        function clearData(obj)
            % Note: ApiClient, Preferences, MainApp, and LoggerApp are **not** cleared

            % Clear planner-related data
            obj.Planner = [];
            obj.PlanData = [];
            obj.ApiClient.PlanData = []; % Keep ApiClient but clear its PlanData
                       
            % Clear plan type and permissions
            obj.PlanType = [];
            obj.AllowEdit = [];
        
            % Reset status properties
            obj.StatusText = [];
            obj.CurrentStatus = [];
        
            % Reset modification tracking and debug paths
            obj.Modified = false;
            obj.AfterBuild = false;
        end

        % =================================================================
        %
        % =================================================================
        
        function tbl = convertTableDatetimeToString(obj, tbl)
            % Converts all datetime columns in a table to string format (yyyy-MM-dd HH:mm:ss)
            % Handles empty datetime (NaT) values correctly.
        
            % Get all variable (column) names
            varNames = tbl.Properties.VariableNames;
        
            % Iterate through each column
            for i = 1:numel(varNames)
                colName = varNames{i};
        
                % Check if the column contains datetime values
                if isa(tbl.(colName), 'datetime')
                    % Initialize new column as cell array of strings
                    newCol = cell(height(tbl), 1);
        
                    % Loop through all rows in the column
                    for j = 1:height(tbl)
                        if isnat(tbl.(colName)(j))  % Check if it's NaT
                            newCol{j} = "";  % Empty string for NaT values
                        else
                            newCol{j} = datestr(tbl.(colName)(j), 'yyyy-MM-dd HH:mm:ss'); % Convert datetime to string
                        end
                    end
        
                    % Convert cell array to string array and assign back to table
                    tbl.(colName) = string(newCol);
                end
            end
        end


        function htmlStr = jsonToHtml(obj, jsonData)
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


        % -------------------------------------------------------------------

        function name = extractNameFromDisplayString(obj, displayStr)
            % Extracts the name part from "id - name" format
            parts = strsplit(displayStr, ' - ');
            if numel(parts) >= 2
                name = strtrim(parts{2});  % Take just the name part
            else
                name = strtrim(displayStr);  % Fallback: return whole string
            end
        end

    end

end
