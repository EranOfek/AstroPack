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
    % This class serves like a DataModule in Delphi

    properties
        ApiClient               % MissionApiClient/MissionApiSim instance
        UserClient              % UserManagerClient/UserManagerSim instance
        Preferences             % ultrasat.planner.gui.Preferences()
        PreferencesFileName     %
        NamespaceId             % 'OPER' for operationl, lowercase id for simulators ('sim01' etc.)
        NamespaceDisplay        % String as 'Id - Name'
        NamespaceDisplayList    % List of available namespaces other than OPER as 'Id - Name'
        UserName                % Current user
        MainApp                 % AppDesigner main window - ultrasat.planner.gui.PlannerMain
        LoggerApp               % ultrasat.planner.gui.Logger
        ErrorLoggerApp          % ultrasat.planner.gui.ErrorLogger
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

        % Helpers
        AppUtils                % Utility functions
        TableHelper             % Utility functions for tables
        GuiHelper               % Utility functions for GUI
    end


    methods
        function obj = MainModule(NamespaceId)
            % Constructor
            disp('app.MainModule');

            % Get namespace from O/S env
            % setenv('SOC_NAMESPACE_ID', 'OPER')
            % setenv('SOC_NAMESPACE_ID', 'SIM')
            obj.NamespaceId = NamespaceId;  %getenv('SOC_NAMESPACE_ID');
            obj.NamespaceDisplay = '';
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

            % Load Preferences from file
            obj.PreferencesFileName = fullfile(obj.PlannerPath, 'preferences.json');
            obj.Preferences = ultrasat.planner.guiutils.Preferences(obj.PreferencesFileName);
            obj.Preferences.load();

            % Setup ApiClient - CURRENTLY we use only Sim - with Local access
            % to JSON files or or remote access using simple_file_server.py
            UseSim = true;
            if UseSim
                obj.msglog('Creating ApiClient as ultrasat.api.MissionClientSim');
                obj.ApiClient = ultrasat.api.MissionApiSim();   %'LogFileName', obj.LogFileName);
                obj.UserClient = ultrasat.api.UserManagerSim();  %'LogFileName', obj.LogFileName);
            else
                obj.msglog('Creating ApiClient as ultrasat.api.MissionClient');
                obj.ApiClient = ultrasat.api.MissionApiClient();  % 'LogFileName', obj.LogFileName);
                obj.ApiClient.ApiUrl = 'http://localhost:8215';
                obj.UserClient = ultrasat.api.UserManagerClient();  %'LogFileName', obj.LogFileName);
            end

            % Operational - When starting Planner from OPER, this is the
            % only option for the user, otherwise get the namespace list
            % from the server
            if strcmp(obj.NamespaceId, 'OPER')
                obj.NamespaceDisplay = 'OPERATIONAL';
            else
                response = obj.UserClient.getNamespaceList();
                if response.ok
                    obj.NamespaceDisplayList = response.display_list;
                end
            end

            % Create helper classes
            obj.TableHelper = ultrasat.planner.guiutils.TableHelper();
            obj.GuiHelper = ultrasat.planner.guiutils.GuiHelper();

            % Create instance of AppUtils
            obj.msglog('MainModule created successfully');
        end


        function Result = login(obj, UserName, Password, Namespace)
            % Connect & login to server
            obj.UserName = [];
            obj.NamespaceId = [];
            Result = false;
            ANamespaceId = obj.GuiHelper.extractNameFromDisplayString(Namespace);
            response = obj.UserClient.login(UserName, Password, ANamespaceId);
            if response.ok
                obj.UserName = UserName;
                obj.NamespaceId = ANamespaceId;
                obj.NamespaceDisplay = obj.GuiHelper.extractTitleFromDisplayString(Namespace);
                %obj.ApiClient.NamespaceId = obj.NamespaceId;

                % Set the namespace id for the PathUtils class, so any class derived from Loggable will use this namespace id
                ultrasat.api.PathUtils.NamespaceId(obj.NamespaceId);
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
            response = obj.UserClient.logout(obj.UserName);

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

            NewText = sprintf('%s %s', ultrasat.api.ModelBase.nowUtcStr(), NewText);

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

    end

end
