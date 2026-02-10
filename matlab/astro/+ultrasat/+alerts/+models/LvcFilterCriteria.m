%==========================================================================
% Project     : ULTRASAT SOC Alert Parser
% File        : +alerts/+models/    LvcFilterCriteria.m
% Author      : Chen Tishler
% Created     : 09/02/2026
% Updated     : 09/02/2026  
% Description : Class to hold LVC filter criteria
%==========================================================================

classdef LvcFilterCriteria
    properties
        % ---------------------------- Acceptance
        bns_min double = 0.0
        nsbh_min double = 0.0
        bbh_min double = 999.0
        massgap_min double = 0.0
        hasns_min double = 0.0
        hasremnant_min double = 0.0

        % ---------------------------- Rejections
        terrestrial_max double = 999.0
        far_max double = 999.0    % [1/year]

        % ---------------------------- Currently not used
        area_max double = 999999.0
        area_credzone double = 0.9
        handle_bursts logical = false
        burst_freq_min double = 0.0
        burst_duration_min double = 0.0
        tile_credzone double = 0.9
        tile_area_max double = 30.0
    end

    % =================================================================
    %                           Constructor
    % =================================================================

    methods  % Constructor
        function obj = LvcFilterCriteria(varargin)
            % Constructor with name=value pairs
            %
            % Example:
            %   crit = LvcFilterCriteria("bns_min", 0.5, "far_max", 10);
            %
            if mod(nargin, 2) ~= 0
                error("LvcFilterCriteria:Constructor", "Arguments must be name/value pairs");
            end

            % Set properties from name/value pairs
            for i = 1:2:nargin
                name = varargin{i};
                value = varargin{i+1};

                if isprop(obj, name)
                    obj.(name) = value;
                else
                    error("LvcFilterCriteria:InvalidProperty", "Unknown property: %s", name);
                end
            end
        end


        function result = toJsonString(obj)
            % Converts the object to a JSON string
            %
            % Returns:
            %   result - JSON string
            s = struct(obj);
            result = jsonencode(s, 'PrettyPrint', true);
        end


        function saveToJsonFile(obj, file_path)
            % Saves the object to a JSON file
            %
            % Parameters:
            %   file_path - Path to the JSON file
            result = obj.toJsonString();
            fid = fopen(file_path, 'w');
            fwrite(fid, result, 'char');
            fclose(fid);
        end
    end


    % =================================================================
    %                           Serialization
    % =================================================================

    methods(Static)
        function obj = fromJsonString(jsonString)
            % Converts a JSON string to an object
            %
            % Parameters:
            %   jsonString - JSON string
            %
            % Returns:
            %   obj - Object
            data = jsondecode(jsonString);
            obj = ultrasat.alerts.models.LvcFilterCriteria.fromStruct(data);
        end


        function obj = loadFromJsonFile(file_path)
            % Loads the object from a JSON file
            %
            % Parameters:
            %   file_path - Path to the JSON file
            %
            % Returns:
            %   obj - Object
            result = fileread(file_path);
            obj = ultrasat.alerts.models.LvcFilterCriteria.fromJsonString(result);
        end


        function obj = fromStruct(s)
            % Converts a struct to an object
            %
            % Parameters:
            %   s - Struct
            %
            % Returns:
            %   obj - Object
            obj = ultrasat.alerts.models.LvcFilterCriteria();

            obj.bns_min = getfield(s, "bns_min", 0.0);
            obj.nsbh_min = getfield(s, "nsbh_min", 0.0);
            obj.bbh_min = getfield(s, "bbh_min", 999.0);
            obj.massgap_min = getfield(s, "massgap_min", 0.0);
            obj.hasns_min = getfield(s, "hasns_min", 0.0);
            obj.hasremnant_min = getfield(s, "hasremnant_min", 0.0);
            obj.terrestrial_max = getfield(s, "terrestrial_max", 999.0);
            obj.far_max = getfield(s, "far_max", 999.0);
            obj.area_max = getfield(s, "area_max", 999999.0);
            obj.area_credzone = getfield(s, "area_credzone", 0.9);
            obj.handle_bursts = getfield(s, "handle_bursts", false);
            obj.burst_freq_min = getfield(s, "burst_freq_min", 0.0);
            obj.burst_duration_min = getfield(s, "burst_duration_min", 0.0);
            obj.tile_credzone = getfield(s, "tile_credzone", 0.9);
            obj.tile_area_max = getfield(s, "tile_area_max", 30.0);
        end
    end
end

% =================================================================
%                                Helper methods
% =================================================================

function v = getfield(s, name, default)
    % Gets the value of a field from a struct
    %
    % Parameters:
    %   s - Struct
    %   name - Name of the field
    %   default - Default value if the field is not found
    %
    % Returns:
    %   v - Value of the field
    if isfield(s, name)
        v = s.(name);
    else
        v = default;
    end
end

