%==========================================================================
% Project     : ULTRASAT Planner
% File        : +planner/+guiutils/MainModule.m
% Author      : Chen Tishler
% Created     : 07/01/2025
% Updated     : 18/02/2026
% Description : Central class to hold common application data
%==========================================================================

classdef MainModule < ultrasat.api.core.Loggable
    % This class serves like a DataModule in Delphi

    properties
        Preferences             % ultrasat.planner.gui.Preferences()
        PreferencesFileName     % Preferences file name
        NamespaceId             % 'OPER' for operationl, lowercase id for simulators ('sim01' etc.)
        NamespaceDisplay        % String as 'Id - Name'
        NamespaceDisplayList    % List of available namespaces other than OPER as 'Id - Name'
        UserName                % Current user
        MainApp                 % AppDesigner main window - ultrasat.planner.gui.PlannerMain
        LoggerApp               % ultrasat.planner.gui.Logger
        ErrorLoggerApp          % ultrasat.planner.gui.ErrorLogger
        Planner                 % instance of ultrasat.planner.uplanner
        PlanData                % instance of ultrasat.api.PlanData

        % Clients
        NamespaceClient         % NamespaceManagerClient instance
        UserClient              % UserManagerClient/UserManagerSim instance
        ScheduleClient          % ScheduleManagerClient instance
        PlansClient             % PlansManagerClient instance
        ValidatorClient         % ValidatorManagerClient instance

        % Status
        StatusText              % Status text for display
        CurrentStatus           % 'OK', 'Error', 'Warning'

        %
        Modified = false;       % True after data is being modified
        AfterBuild = false;     % True after build is completed
        PlannerPath             % Path to Planner folder
        DebugPath               % Folder of debug files, such as saved .mat files
        BaseDataDir             % uplanner constructor param - base data directory
        LogFileName             % Log file name

        % Helpers
        AppUtils                % Utility functions, created in PlannerMain.mlapp
        TableHelper             % Utility functions for tables
        GuiHelper               % Utility functions for GUI
    end


    methods
        function obj = MainModule()
            % Constructor

            obj.LogPrefix = 'MainModule';

            % @Future - Need to fix it on linux? or keep it like this?
            obj.BaseDataDir = '~/matlab/data/ULTRASAT/';
            obj.PlannerPath = '~/matlab/data/ULTRASAT/Planner/';
            if ispc
                obj.BaseDataDir = fullfile(getenv('ASTROPACK_DATA_PATH'), 'ULTRASAT');
                obj.PlannerPath = fullfile(obj.BaseDataDir, 'PlannerGUI');
            end

            % Create Planner folder if it does not exist
            if ~exist(obj.PlannerPath, 'dir')
                mkdir(obj.PlannerPath);
            end
            obj.DebugPath = obj.PlannerPath;
            obj.LogFileName = fullfile(obj.PlannerPath, 'planner.log');
            obj.msglog('MainModule started');

            % Load Preferences from local file 'preferences.json'
            obj.PreferencesFileName = fullfile(obj.PlannerPath, 'preferences.json');
            obj.Preferences = ultrasat.planner.guiutils.Preferences(obj.PreferencesFileName);
            obj.Preferences.load();

            % Setup clients
            factory = ultrasat.api.clients.ClientFactory();
            url = factory.getServiceBaseUrl('namespace_manager');
            obj.NamespaceClient = ultrasat.api.clients.NamespaceManagerClient(url);

            url = factory.getServiceBaseUrl('user_manager');
            obj.UserClient = ultrasat.api.clients.UserManagerClient(url);

            url = factory.getServiceBaseUrl('schedule_manager');
            obj.ScheduleClient = ultrasat.api.clients.ScheduleManagerClient(url);

            url = factory.getServiceBaseUrl('plans_manager');
            obj.PlansClient = ultrasat.api.clients.PlansManagerClient(url);

            url = factory.getServiceBaseUrl('validator_manager');
            obj.ValidatorClient = ultrasat.api.clients.ValidatorManagerClient(url);

            % Get the list of namespaces
            response = obj.NamespaceClient.getNamespaceList();
            if isfield(response, 'namespaces') && ~isempty(response.namespaces)
                obj.NamespaceDisplayList = response.display_list;

                % If there is only one namespace, set obj.NamespaceId to it
                if numel(obj.NamespaceDisplayList) == 1
                    obj.NamespaceId = obj.GuiHelper.extractNameFromDisplayString(obj.NamespaceDisplayList{1});
                    obj.setNamespace(obj.NamespaceId);
                end
            else
                obj.NamespaceId = 'none';
                obj.NamespaceDisplay = 'not connected';
            end

            % Create helper classes
            obj.TableHelper = ultrasat.planner.guiutils.TableHelper();
            obj.GuiHelper = ultrasat.planner.guiutils.GuiHelper();

            % Create instance of AppUtils
            obj.msglog('MainModule created successfully');
        end


        function Result = login(obj, UserName, Password, Namespace)
            % Connect & login to server

            Result = false;

            % Clear UserName and NamespaceId
            obj.UserName = [];
            obj.NamespaceId = [];
            ANamespaceId = obj.GuiHelper.extractNameFromDisplayString(Namespace);

            % Try to login
            try
              response = obj.UserClient.login(UserName, Password, ANamespaceId);
            catch ME
                obj.setStatus('Error', sprintf('Login failed: %s', ME.message));
                return;
            end

            % Check if login was successful
            if ~isstruct(response) || ~isfield(response, 'ok')
                obj.setStatus('Error', 'Invalid login response');
                return;
            end

            % If login was successful, set UserName, NamespaceId and NamespaceDisplay
            if response.ok
                obj.UserName = UserName;
                obj.NamespaceId = ANamespaceId;
                obj.NamespaceDisplay = obj.GuiHelper.extractTitleFromDisplayString(Namespace);

                % Set the namespace id for the PathUtils class, so any class derived from Loggable will use this namespace id
                ultrasat.api.utils.PathUtils.NamespaceId(obj.NamespaceId);
                obj.setNamespace(obj.NamespaceId);

                Result = true;
            end
        end


        function Result = logout(obj)
            % Logout from server

            % Do nothing if not logged in
            if isempty(obj.UserName)
                Result = true;
                return;
            end

            Result = false;
            try
                response = obj.UserClient.logout(obj.UserName);
            catch ME
                obj.setStatus('Error', sprintf('Logout failed: %s', ME.message));
                return;
            end

            % Currently we do not check response.ok, so even if logout
            % failed (why?) we clear UserName, leave NamespaceId without change
            obj.UserName = [];
            Result = true;
        end


        function setNamespace(obj, NamespaceId)
            % Update the NamespaceId of clients that require it (PlansClient, ScheduleClient, etc.).
            if ~isempty(obj.ScheduleClient)
                obj.msglog(sprintf('setNamespace: setting ScheduleClient namespace to %s', NamespaceId));
                obj.ScheduleClient.Namespace = NamespaceId;
            end
            if ~isempty(obj.PlansClient)
                obj.msglog(sprintf('setNamespace: setting PlansClient namespace to %s', NamespaceId));
                obj.PlansClient.Namespace = NamespaceId;
            end            
        end


        function setPlanner(obj, Planner)
            % Set the current Planner object & type

            obj.msglog(sprintf('setPlanner: %s', Planner.Type));
            obj.Planner = Planner;

            % Create UplannerClient instance (adapter class for uplanner)
            uplannerClient = ultrasat.api.UplannerClient( obj.PlansClient, obj.ScheduleClient, obj.ValidatorClient );
            Planner.Mclient = uplannerClient;

            % Override BaseDataDir to allow Linux/Windows compatibility
            if ~strcmp(Planner.BaseDataDir, obj.BaseDataDir) 
                obj.msglog(sprintf('setPlanner: updating BaseDataDir to match current O/S: %s', obj.BaseDataDir));
                Planner.BaseDataDir = obj.BaseDataDir;
            end
        end

        % =================================================================

        function setModified(obj)
            % Set the Modified flag to true
            obj.Modified = true;
        end


        function clearModified(obj)
            % Clear the Modified flag to false
            obj.Modified = false;
        end


        function clearStatus(obj)
            % Clear current status fields: CurrentStatus and StatusText, and clear status of PlanData if it exists
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

            NewText = sprintf('%s %s', ultrasat.api.utils.DateTimeUtils.nowUtcStr(), NewText);

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
            % Create new instance of PlanData
            obj.PlanData = ultrasat.api.models.PlanData();
        end


        function setPlanData(obj, Data)
            % Set PlanData to the given instance
            obj.PlanData = Data;
            if ~isempty(obj.PlanData.planner)
                obj.setPlanner(obj.PlanData.planner);
            end
        end


        function clearData(obj)
            %

            % Clear planner-related data
            obj.Planner = [];
            obj.PlanData = [];

            % Reset status properties
            obj.StatusText = [];
            obj.CurrentStatus = [];

            % Reset modification tracking and debug paths
            obj.Modified = false;
            obj.AfterBuild = false;
        end

        % -------------------------------------------------------------------

    end

end
