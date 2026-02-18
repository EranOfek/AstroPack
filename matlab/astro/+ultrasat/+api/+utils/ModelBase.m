%==========================================================================
% Project     : ULTRASAT Observation Planner
% Filename    : ultrasat.api.utils.ModelBase.m
% Author      : Chen Tishler
% Created     : 01/12/2024
% Updated     : 21/09/2025
% Description : Base class for individual models.
%==========================================================================

classdef ModelBase
    % ModelBase - Base class for individual models.
    % This class serves as a foundation for data models used in the
    % ULTRASAT observation planner. It mimics the behavior of Python's
    % Pydantic BaseModel by structuring data as MATLAB structs.
    %
    % Key Features:
    % - Stores data as a structured MATLAB struct (`Data` property).
    % - Provides methods for JSON serialization and display.
    % - Includes utility functions for cleaning struct fields.
    %
    % This class is typically used with `ModelFactoryBase`, which
    % generates struct instances for FastAPI requests.

    properties (Access = public)
        Data % Struct containing the public fields of the model
    end


    methods
        function obj = ModelBase(data)
            % Constructor for ModelBase
            %
            % Initializes the model with the given data structure.
            %
            % :param data: A MATLAB struct containing model fields.
            % :return: An instance of ModelBase with stored data.
            obj.Data = data;
        end


        function jsonStr = toJson(obj)
            % Converts the Data property to a JSON string, converting datetime fields
            jsonStr = ultrasat.api.utils.ModelBase.struct2json(obj.Data);
        end


        function show(obj)
            % Displays the current model data in the console
            %
            % Prints the structured data stored in the model instance.
            disp('Model Data:');
            disp(obj.Data);
        end
    end

    % ---------------------------------------------------------------------

    methods (Static)

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
            data = ultrasat.api.utils.ModelBase.class2struct(obj);
            js = ultrasat.api.utils.ModelBase.struct2json(data);
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
            data = ultrasat.api.utils.ModelBase.json2struct(js);
            obj = ultrasat.api.utils.ModelBase.struct2class(data, className);
        end


        function jsonStr = struct2json(Data)
            % Converts the Data property to a JSON string, converting datetime fields
            jsonReadyData = ultrasat.api.utils.DateTimeUtils.convertDatetimeToString(Data);
            jsonStr = jsonencode(jsonReadyData);
        end


        function jsonStruct = json2struct(jsonStr)
            % Converts JSON string to struct, decode timestamp strings to MATLAB's datetime type.
            % If `jsonStr` is a filename, it reads the file first.
            %
            % :param jsonStr: JSON string or a filename containing JSON.
            % :return: struct
            decodedStruct = jsondecode(jsonStr);
            jsonStruct = ultrasat.api.utils.DateTimeUtils.convertStringToDatetime(decodedStruct);
        end



        function Model = fromJson(jsonStr)
            % Converts the JSON string to new instance of ModelBase
            % (settings its Data property)
            %
            % :return: New instance of BaseModel class
            Model = ultrasat.api.utils.ModelBase([]);
            Model.Data = ultrasat.api.utils.ModelBase.json2struct(jsonStr);
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
                        cleanedData.(fields{i}) = ultrasat.api.utils.ModelBase.removeEmptyFields(value);
                    else
                        cleanedData.(fields{i}) = value;
                    end
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
