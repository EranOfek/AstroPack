%==========================================================================
% Project     : ULTRASAT SOC Alert Parser
% File        : +alerts/+models/LvcParsedAlert.m
% Author      : Chen Tishler
% Created     : 09/02/2026
% Updated     : 09/02/2026
% Description : Class to hold parsed LVC alert data
%==========================================================================

classdef LvcParsedAlert
    % Parsed representation of an LVC alert, normalized by the parser
    % and shared between Python SOC and MATLAB filters.

    properties
        % ------------------------- Identity
        alert_id string = ""
        superevent_id string = ""
        alert_type string = ""

        % ------------------------- Timing (UTC)
        time_created datetime = NaT
        event_time datetime = NaT

        % ------------------------- Classification probabilities (0..1)
        prob_bns double = NaN
        prob_nsbh double = NaN
        prob_bbh double = NaN
        prob_terrestrial double = NaN

        % ------------------------- Event properties (0..1)
        has_ns double = NaN
        has_remnant double = NaN
        has_mass_gap double = NaN

        % ------------------------- Rates and metadata
        far_hz double = NaN
        far_per_year double = NaN

        % ------------------------- Localization
        skymap_path string = ""
        localization_area_deg2 double = NaN

        % ------------------------- Raw / extended fields
        instruments string = strings(0,1)
        pipeline string = ""
        search string = ""
        raw_fields struct = struct()

        % ------------------------- Parser metadata
        parsed_time datetime = NaT
    end


    methods  % Constructor
        function obj = LvcParsedAlert(varargin)
            % Constructor with name=value pairs
            %
            % Example:
            %   alert = ultrasat.alerts.models.LvcParsedAlert( ...
            %       "alert_id", "G12345", ...
            %       "prob_bns", 0.7, ...
            %       "far_per_year", 2.5 ...
            %   );

            if mod(nargin, 2) ~= 0
                error("LvcParsedAlert:Constructor", "Arguments must be name/value pairs");
            end

            % Set properties from name/value pairs
            for i = 1:2:nargin
                name = varargin{i};
                value = varargin{i+1};

                if isprop(obj, name)
                    obj.(name) = value;
                else
                    error("LvcParsedAlert:InvalidProperty", "Unknown property: %s", name);
                end
            end
        end

        % =================================================================
        %                                Serialize
        % =================================================================

        function result = toJsonString(obj)
            % Converts the object to a JSON string
            %
            % Returns:
            %   asJsonString - JSON string
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
            obj = ultrasat.alerts.models.LvcParsedAlert.fromStruct(data);
        end


        function obj = loadFromJsonFile(file_path)
            % Loads the object from a JSON file
            %
            % Parameters:
            %   file_path - Path to the JSON file
            %
            % Returns:
            %   obj - Object
            txt = fileread(file_path);
            obj = ultrasat.alerts.models.LvcParsedAlert.fromJsonString(txt);
        end


        function obj = fromStruct(s)
            % Converts a struct to an object
            %
            % Parameters:
            %   s - Struct
            %
            % Returns:
            %   obj - Object
            obj = ultrasat.alerts.models.LvcParsedAlert();

            obj.alert_id = getfield(s, "alert_id", "");
            obj.superevent_id = getfield(s, "superevent_id", "");
            obj.alert_type = getfield(s, "alert_type", "");

            obj.time_created = parse_datetime(getfield(s, "time_created", []));

            obj.event_time = parse_datetime(getfield(s, "event_time", []));

            obj.prob_bns = getfield(s, "prob_bns", NaN);
            obj.prob_nsbh = getfield(s, "prob_nsbh", NaN);
            obj.prob_bbh = getfield(s, "prob_bbh", NaN);
            obj.prob_terrestrial = getfield(s, "prob_terrestrial", NaN);

            obj.has_ns = getfield(s, "has_ns", NaN);
            obj.has_remnant = getfield(s, "has_remnant", NaN);
            obj.has_mass_gap = getfield(s, "has_mass_gap", NaN);

            obj.far_hz = getfield(s, "far_hz", NaN);
            obj.far_per_year = getfield(s, "far_per_year", NaN);

            obj.skymap_path = getfield(s, "skymap_path", "");
            obj.localization_area_deg2 = getfield(s, "localization_area_deg2", NaN);

            obj.instruments = string(getfield(s, "instruments", strings(0,1)));
            obj.pipeline = getfield(s, "pipeline", "");
            obj.search = getfield(s, "search", "");
            obj.raw_fields = getfield(s, "raw_fields", struct());

            obj.parsed_time = parse_datetime(getfield(s, "parsed_time", []));
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


function dt = parse_datetime(x)
    % Parses a datetime string to a datetime object
    %
    % Parameters:
    %   x - Datetime string
    %
    % Returns:
    %   dt - Datetime object
    if isempty(x)
        dt = NaT;
    elseif isdatetime(x)
        dt = x;
    else
        dt = datetime(x, 'InputFormat', "yyyy-MM-dd'T'HH:mm:ss", 'TimeZone', 'UTC');
    end
end

