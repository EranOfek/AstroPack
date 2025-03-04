%==========================================================================
% ULTRASAT Planner
%
% File:   +planner/+gui/MainModule.m
% Author:  Chen Tishler
% Created: 07/01/2025
% Updated: 28/01/2025
% Title:   
%==========================================================================

classdef MainModule < handle
    % This class serves like a DataModule in Delphi.
    
    properties
        ApiClient               % MissionClient/MissionClientSim instance
        Preferences             % ultrasat.planner.gui.Preferences()
        PreferencesFileName     %
        UserName                % Current user
        MainApp                 % AppDesigner main window - ultrasat.planner.gui.PlannerMain
        LoggerApp               % ultrasat.planner.gui.Logger
        
        PlanType                % Current plan type: HCS, LCS, AllSS, DDT, TOO (= ultrasat.planner.uplanner.Type)
        Planner                 % instance of ultrasat.planner.uplanner
        PlanData                % instance of ultrasat.api.PlanData, same as ApiClient.PlanData
        ReadOnly                % = ~AllowEdit
        AllowEdit               % = ~ReadOnly

        % Status
        StatusText              % Status text for display        
        CurrentStatus           % 'OK', 'Error', 'Warning'

        BuildStatus             % @Todo - Consider if we need it here of take it from Planner? Check & Think
        SelfConsistencyStatus   %
        ValidateStatus          %
        ValidateStatusText      %
        SubmitStatus            %

        BuildStatusData         %
        ValidationStatusData    %
        SubmitStatusData        %

        %
        Modified = false;       % True after data is being modified        
        AfterBuild = false;     %
        PlannerPath             %
        DebugPath               % Folder of debug files, such as saved .mat files
        BaseDataDir             % uplanner constructor param
        LogFileName             %
    end
    

    methods
        function obj = MainModule()
            % Constructor
            disp('app.MainModule');
                       
            % Temporary solution, @Todo @Yossi
            obj.BaseDataDir = '~/matlab/data/ULTRASAT/';
            obj.PlannerPath = '~/matlab/data/ULTRASAT/Planner/';
            if ispc
                obj.BaseDataDir =  'C:/AstroPack/Data/ULTRASAT/';
                obj.PlannerPath = 'C:/Temp/_planner';                
            end

            if ~exist(obj.PlannerPath, 'dir')
                mkdir(obj.PlannerPath);
            end
            obj.DebugPath = obj.PlannerPath;  %fullfile(obj.PlannerPath, 'debug');
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
                obj.ApiClient = ultrasat.api.MissionClientSim('LogFileName', obj.LogFileName);
            else
                obj.msglog('Creating ApiClient as api.MissionClient');
                obj.ApiClient = ultrasat.api.MissionClient('LogFileName', obj.LogFileName);
                obj.ApiClient.ApiUrl = 'http://localhost:8215';                          
            end

            obj.msglog('MainModule created successfully');
        end


        function Result = login(obj, UserName, Password)
            % Connect & login to server
            obj.UserName = [];
            Result = false;
            response = obj.ApiClient.login(UserName, Password);
            if response.ok
                obj.UserName = UserName;
                Result = true;
            end
        end


        function Result = logout(obj)
            % Logout from server
            obj.UserName = [];
            Result = obj.ApiClient.logout(obj.UserName);
            if Result
                obj.UserName = [];
            end
        end        


        function setPlanner(obj, Planner)
            % Set the current Planner object & type
            obj.msglog(sprintf('setPlanner: %s', Planner.Type));
            obj.Planner = Planner;
            obj.PlanType = Planner.Type;
            Planner.Mclient = obj.ApiClient;
        end

        % =================================================================

        function Result = startValidation(obj, Planner)
            Result = true;
        end


        function Result = stopValidation(obj)
            Result = true;
        end        


        function Result = getValidationStatus(obj)
            Result = true;  % struct() - @Todo
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
            % Return the value of duration text field as duration object
            if ~isempty(Value)
                Result = duration(Value);
            else
                Result = []; 
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
            % @Todo - need to support sexa, etc.
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
        
        % =================================================================

        function setModified(obj)
            % Set the Modified flag
            obj.Modified = true;
        end

        function clearModified(obj)
            % Clear the Modified flag
            obj.Modified = true;
        end

        function clearStatus(obj)
            % Clear current status fields
            obj.CurrentStatus = [];
            obj.StatusText = [];
            obj.BuildStatus = [];
            obj.SelfConsistencyStatus = [];

            %
            obj.BuildStatusData = obj.newStatusData('');
            obj.ValidationStatusData = obj.newStatusData('');
            obj.SubmitStatusData = obj.newStatusData('');
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
            
            NewText = sprintf('%s %s', obj.nowUtcStr(), NewText);
            
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


        function data = newStatusData(obj, Status)
            % 
            
            % Define the data for the model with all fields set to []
            data = struct(...
                'Status', Status, ...
                'StartTime', datestr(now, 'yyyy-MM-dd HH:mm:ss'), ...
                'UpdateTime', datestr(now, 'yyyy-MM-dd HH:mm:ss'), ...
                'Text', [], ...
                'Html', [] ...
            );
        end        
        

        function Result = nowUtc(obj)
            Result = datetime('now', 'TimeZone', 'UTC');
        end

        function Result = nowUtcStr(obj)
            Result = datestr(datetime('now', 'TimeZone', 'UTC'), 'yyyy-MM-dd HH:mm:ss');            
        end

        function datetimeStr(obj, dt)
            Result = datestr(dt, 'yyyy-MM-dd HH:mm:ss');            
        end
    end

end
