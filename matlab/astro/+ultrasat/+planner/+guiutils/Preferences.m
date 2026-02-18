%==========================================================================
% Project     : ULTRASAT Planner
% File        : +planner/+guiutils/Preferences.m
% Author      : Chen Tishler
% Created     : 20/01/2025
% Updated     : 06/10/2025
% Description : Preferences for Main Planner
%==========================================================================

classdef Preferences < ultrasat.api.core.Loggable
    % This class manages the loading and saving of user preferences to/from a JSON file.
    % The class will be enhanced later with additional preference options.

    properties
        FileName                    % Full path to the preferences JSON file
        UserName                    % Current user name for personalization

        UniqueTargetsFileName       % Name of the file storing unique targets
        UniqueTargetsFolder         % Directory path where unique targets are stored

        LocalPlanFileName           % Name of the file storing the local observation plan
        LocalPlanFolder             % Directory path where local plans are stored

        UseSim = false              % If true use MissionApiSim (JSON files), else MissionApiClient (FastAPI plans_manager)
        PlansManagerApiUrl = ''     % Base URL for plans_manager API (e.g. http://localhost:8321)
        ApiKey = ''                 % API key for plans_manager authentication
    end


    methods
        function obj = Preferences(FileName)
            % Constructor

            obj.LogPrefix = 'Preferences';

            % Initializes the Preferences object with a file name for JSON storage.
            obj.FileName = FileName;
        end


        function save(obj)
            % Saves the user preferences to the JSON file.
            obj.saveToJson(obj.FileName);
        end


        function load(obj)
            % Loads user preferences from the JSON file.
            obj.loadFromJson(obj.FileName);
        end

        % =================================================================
        %                                Save
        % =================================================================

        function saveToJson(obj, filePath)
            % Saves the Preferences object to the specified JSON file.
            % Converts the current object properties into a JSON string
            % and writes them to the specified file.
            %
            % If any step fails (struct conversion, JSON encoding, file writing),
            % the error is logged and the function returns without throwing.

            try
                % Convert object properties to a struct
                dataStruct = struct(...
                    'UserName', obj.UserName, ...
                    'UniqueTargetsFileName', obj.UniqueTargetsFileName, ...
                    'UniqueTargetsFolder', obj.UniqueTargetsFolder, ...
                    'LocalPlanFileName', obj.LocalPlanFileName, ...
                    'LocalPlanFolder', obj.LocalPlanFolder, ...
                    'UseSim', obj.UseSim, ...
                    'PlansManagerApiUrl', obj.PlansManagerApiUrl, ...
                    'ApiKey', obj.ApiKey ...
                );
            catch ME
                obj.msglog(sprintf('saveToJson: failed to create dataStruct: %s', ME.message));
                return;
            end

            % Convert struct to JSON
            try
                jsonStr = jsonencode(dataStruct, 'PrettyPrint', true);
            catch ME
                obj.msglog(sprintf('saveToJson: jsonencode failed: %s', ME.message));
                return;
            end

            % Write JSON to file
            fid = fopen(filePath, 'w');
            if fid == -1
                obj.msglog(sprintf('saveToJson: could not open file for writing: %s', filePath));
                return;
            end

            try
                fwrite(fid, jsonStr, 'char');
            catch ME
                obj.msglog(sprintf('saveToJson: fwrite failed for file %s: %s', filePath, ME.message));
            end

            try
                fclose(fid);
            catch ME
                obj.msglog(sprintf('saveToJson: fclose failed for file %s: %s', filePath, ME.message));
            end
        end

        % =================================================================
        %                                Load
        % =================================================================

        function loadFromJson(obj, filePath)
            % Loads the Preferences object from the specified JSON file.
            % Reads the JSON file, decodes it, and updates object properties.
            %
            % If the file is missing, unreadable, or contains invalid JSON,
            % the error is logged and no exception is thrown.

            % Check if the file exists
            if ~isfile(filePath)
                obj.msglog(sprintf('loadFromJson: file not found: %s', filePath));
                return;
            end

            % Read JSON file
            fid = fopen(filePath, 'r');
            if fid == -1
                obj.msglog(sprintf('loadFromJson: could not open file for reading: %s', filePath));
                return;
            end

            raw = [];
            try
                raw = fread(fid, inf, 'char');
            catch ME
                obj.msglog(sprintf('loadFromJson: fread failed for file %s: %s', filePath, ME.message));
            end

            try
                fclose(fid);
            catch ME
                obj.msglog(sprintf('loadFromJson: fclose failed for file %s: %s', filePath, ME.message));
            end

            if isempty(raw)
                obj.msglog(sprintf('loadFromJson: file is empty or unreadable: %s', filePath));
                return;
            end

            % Convert JSON to struct
            try
                jsonStr = char(raw');
                dataStruct = jsondecode(jsonStr);
            catch ME
                obj.msglog(sprintf('loadFromJson: jsondecode failed for file %s: %s', filePath, ME.message));
                return;
            end

            % Update object properties safely
            try
                if isfield(dataStruct, 'UserName'), obj.UserName = dataStruct.UserName; end
                if isfield(dataStruct, 'UniqueTargetsFileName'), obj.UniqueTargetsFileName = dataStruct.UniqueTargetsFileName; end
                if isfield(dataStruct, 'UniqueTargetsFolder'), obj.UniqueTargetsFolder = dataStruct.UniqueTargetsFolder; end
                if isfield(dataStruct, 'LocalPlanFileName'), obj.LocalPlanFileName = dataStruct.LocalPlanFileName; end
                if isfield(dataStruct, 'LocalPlanFolder'), obj.LocalPlanFolder = dataStruct.LocalPlanFolder; end
                if isfield(dataStruct, 'UseSim'), obj.UseSim = dataStruct.UseSim; end
                if isfield(dataStruct, 'PlansManagerApiUrl'), obj.PlansManagerApiUrl = dataStruct.PlansManagerApiUrl; end
                if isfield(dataStruct, 'ApiKey'), obj.ApiKey = dataStruct.ApiKey; end
            catch ME
                obj.msglog(sprintf('loadFromJson: failed to update properties from file %s: %s', filePath, ME.message));
            end
        end

        % =================================================================
        %                                Get
        % =================================================================

        function v = get(obj, key, default)
            % Returns the value of a preference key, or default if missing/empty.
            %
            % :param key: Property name (e.g. 'UseSim', 'PlansManagerApiUrl').
            % :param default: Value to return if key is missing or empty.
            % :return: Value of obj.(key) or default.
            if isprop(obj, key)
                v = obj.(key);
                if isempty(v) && nargin >= 3
                    v = default;
                end
            elseif nargin >= 3
                v = default;
            else
                v = [];
            end
        end
    end
end

