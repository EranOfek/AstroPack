%==========================================================================
% ULTRASAT Planner
%
% File:   +planner/+gui/Preferences.m
% Author:  Chen Tishler
% Created: 20/01/2025
% Updated: 20/01/2025
% Title:   
%==========================================================================

classdef Preferences < handle
    % This class serves as DataModule in Delphi.
    
    properties
        FileName                    %
        UserName                    % Current user

        UniqueTargetsFileName       %        
        UniqueTargetsFolder         %

        LocalPlanFileName           %
        LocalPlanFolder             %
    end
    

    methods
        function obj = Preferences(FileName)
            % Constructor
            obj.FileName = FileName;
        end


        function save(obj)
            obj.saveToJson(obj.FileName);
        end


        function load(obj)
            obj.loadFromJson(obj.FileName);
        end        

        % =================================================================
        %                                Save
        % =================================================================        
        function saveToJson(obj, filePath)
            % Saves the Preferences object to a JSON file.
            %
            % :param filePath: Full path of the JSON file.
            
            % Convert object properties to a struct
            dataStruct = struct(...
                'UserName', obj.UserName, ...
                'UniqueTargetsFileName', obj.UniqueTargetsFileName, ...
                'UniqueTargetsFolder', obj.UniqueTargetsFolder, ...
                'LocalPlanFileName', obj.LocalPlanFileName ...
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
            % Loads the Preferences object from a JSON file.
            %
            % :param filePath: Full path of the JSON file.

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

