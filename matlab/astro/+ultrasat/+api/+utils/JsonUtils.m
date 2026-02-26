% ***************************************************************************
% Project     : ULTRASAT Observation Planner
% Filename    : JsonUtils.m
% Author      : Chen Tishler
% Created     : 16/09/2025
% Modified    : 21/09/2025
% Description : Utility functions for JSON and struct/class conversion
% ***************************************************************************

classdef JsonUtils
    methods (Static)

        function s = json2struct(js)
            s = jsondecode(js);
            s = ultrasat.api.utils.DateTimeUtils.convertStringToDatetime(s);
        end


        function js = struct2json(s)
            s = ultrasat.api.utils.DateTimeUtils.convertDatetimeToString(s);
            js = jsonencode(s);
        end


        function s = class2struct(obj)
            % Converts a MATLAB class instance to a struct with all properties
            props = properties(obj); % Get all properties of the class
            s = struct();

            for i = 1:numel(props)
                s.(props{i}) = obj.(props{i}); % Copy each property to struct
            end
        end


        function js = class2json(obj)
            % Converts a MATLAB class instance to a struct with all properties
            data = ultrasat.api.utils.JsonUtils.class2struct(obj);
            js = ultrasat.api.utils.JsonUtils.struct2json(data);
        end


        function obj = struct2class(s, className)
            % Converts a struct back to an instance of the specified class
            obj = feval(className); % Create an empty instance of the class
            props = properties(obj); % Get all properties of the class

            for i = 1:numel(props)
                if isfield(s, props{i})
                    obj.(props{i}) = s.(props{i}); % Assign struct fields to class properties
                end
            end
        end


        function obj = json2class(js, className)
            % Converts a struct back to an instance of the specified class
            data = ultrasat.api.utils.JsonUtils.json2struct(js);
            obj = ultrasat.api.utils.JsonUtils.struct2class(data, className);
        end


        function cleanedData = removeEmptyFields(data)
            % Recursively removes fields with empty values from a struct
            %
            % This function ensures that only non-empty fields are
            % included in the final struct. It also handles nested structs.
            %
            % :param data: A MATLAB struct with possible empty fields.
            % :return: A new struct with empty fields removed.
            fields = fieldnames(data);
            cleanedData = struct();
            for i = 1:numel(fields)
                value = data.(fields{i});
                if ~isempty(value)
                    % Retain non-empty fields
                    if isstruct(value)
                        % Recursively clean nested structs
                        cleanedData.(fields{i}) = ultrasat.api.utils.JsonUtils.removeEmptyFields(value);
                    else
                        cleanedData.(fields{i}) = value;
                    end
                end
            end
        end


        function s = replaceEmptyWithNull(s)
            % Replace empty fields with <missing> (converted to json none)
            fields = fieldnames(s);
            for i = 1:numel(fields)
                v = s.(fields{i});
                if isnumeric(v) && isempty(v)
                    s.(fields{i}) = missing;
                elseif isstruct(v) && isscalar(v)
                    s.(fields{i}) = ultrasat.api.utils.JsonUtils.replaceEmptyWithNull(v);
                end
            end
        end


        function isEqual = cmpstruct(A, B)
            % Compare two structs by converting them to JSON string, to
            % avoid MATLAB's non-equality when using isequal()
            isEqual = strcmp(jsonencode(orderfields(A)), jsonencode(orderfields(B)));
        end

    end
end
