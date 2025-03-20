%==========================================================================
% ULTRASAT Planner
%
% File:   +planner/+gui/Preferences.m
% Author:  Chen Tishler
% Created: 20/01/2025
% Updated: 20/03/2025
% Title:   
%==========================================================================

classdef Preferences < handle
    % This class manages the loading and saving of user preferences to/from a JSON file.
    % The class will be enhanced later with additional preference options.
    
    properties
        FileName                    % Full path to the preferences JSON file
        UserName                    % Current user name for personalization

        UniqueTargetsFileName       % Name of the file storing unique targets
        UniqueTargetsFolder         % Directory path where unique targets are stored

        LocalPlanFileName           % Name of the file storing the local observation plan
        LocalPlanFolder             % Directory path where local plans are stored
    end
    

    methods
        function obj = Preferences(FileName)
            % Constructor for Preferences class.
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
            
            % Convert object properties to a struct
            dataStruct = struct(...
                'UserName', obj.UserName, ...
                'UniqueTargetsFileName', obj.UniqueTargetsFileName, ...
                'UniqueTargetsFolder', obj.UniqueTargetsFolder, ...
                'LocalPlanFileName', obj.LocalPlanFileName, ...
                'LocalPlanFolder', obj.LocalPlanFolder ...
            );

            % Convert struct to JSON
            jsonStr = jsonencode(dataStruct, 'PrettyPrint', true);

            % Write JSON to file
            fid = fopen(filePath, 'w');
            if fid == -1
                error('Could not open file: %s', filePath);
            end
            fwrite(fid, jsonStr, 'char');
            fclose(fid);
        end


        % =================================================================
        %                                Load
        % =================================================================        

        function loadFromJson(obj, filePath)
            % Loads the Preferences object from the specified JSON file.
            % Reads the JSON file, decodes it, and updates object properties.

            % Check if the file exists
            if ~isfile(filePath)
                %error('File not found: %s', filePath);
                return;
            end

            % Read JSON file
            fid = fopen(filePath, 'r');
            raw = fread(fid, inf, 'char');
            fclose(fid);
            jsonStr = char(raw');

            % Convert JSON to struct
            dataStruct = jsondecode(jsonStr);

            % Update object properties
            obj.UserName = dataStruct.UserName;

            obj.UniqueTargetsFileName = dataStruct.UniqueTargetsFileName;
            obj.UniqueTargetsFolder = dataStruct.UniqueTargetsFolder;

            obj.LocalPlanFileName = dataStruct.LocalPlanFileName;            
            obj.LocalPlanFolder = dataStruct.LocalPlanFolder;
        end       

    end
end

