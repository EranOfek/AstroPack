%==========================================================================
% Project     : ULTRASAT Observation Planner
% Filename    : ultrasat.api.ModelBase.m
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
            jsonStr = ultrasat.api.ModelBase.struct2json(obj.Data);
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

        function Result = nowUtc()
            % Returns the current UTC datetime as a datetime object
            Result = datetime('now', 'TimeZone', 'UTC');
        end


        function Result = nowUtcStr()
            % Returns the current UTC datetime as a formatted string (YYYY-MM-DD HH:MM:SS)
            Result = datestr(datetime('now', 'TimeZone', 'UTC'), 'yyyy-mm-dd HH:MM:SS');
        end


        function Result = datetimeStr(dt)
            % Converts a given datetime object to a formatted string (YYYY-MM-DD HH:MM:SS)
            if isempty(dt)
                Result = 'None';
            else
                Result = datestr(dt, 'yyyy-mm-dd HH:MM:SS');
            end
        end


        function dt = str2datetime(datetimeStr)
            % Converts a formatted datetime string (YYYY-MM-DD HH:MM:SS) to a datetime object
            dt = datetime(datetimeStr, 'InputFormat', 'yyyy-MM-dd HH:mm:ss', 'TimeZone', 'UTC');
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
            data = ultrasat.api.ModelBase.class2struct(obj);
            js = ultrasat.api.ModelBase.struct2json(data);
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
            data = ultrasat.api.ModelBase.json2struct(js);
            obj = ultrasat.api.ModelBase.struct2class(data, className);
        end


        function jsonStr = struct2json(Data)
            % Converts the Data property to a JSON string, converting datetime fields
            jsonReadyData = ultrasat.api.ModelBase.convertDatetimeToString(Data);
            jsonStr = jsonencode(jsonReadyData);
        end


        function jsonStruct = json2struct(jsonStr)
            % Converts JSON string to struct, decode timestamp strings to MATLAB's datetime type.
            % If `jsonStr` is a filename, it reads the file first.
            %
            % :param jsonStr: JSON string or a filename containing JSON.
            % :return: struct
            decodedStruct = jsondecode(jsonStr);
            jsonStruct = ultrasat.api.ModelBase.convertStringToDatetime(decodedStruct);
        end



        function Model = fromJson(jsonStr)
            % Converts the JSON string to new instance of ModelBase
            % (settings its Data property)
            %
            % :return: New instance of BaseModel class
            Model = ultrasat.api.ModelBase([]);
            Model.Data = ultrasat.api.ModelBase.json2struct(jsonStr);
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
                        cleanedData.(fields{i}) = ultrasat.api.ModelBase.removeEmptyFields(value);
                    else
                        cleanedData.(fields{i}) = value;
                    end
                end
            end
        end


        function result = isoFormat(dt)
            % Converts a MATLAB datetime object to ISO 8601 format
            %
            % :param dt: A MATLAB datetime object.
            % :return: A string in the format 'YYYY-MM-DDTHH:MM:SS.FFFZ'.

            % Ensure the datetime object is in UTC
            if isempty(dt.TimeZone)
                dt.TimeZone = 'UTC';
            elseif dt.TimeZone ~= "UTC"
                dt = datetime(dt, 'ConvertFrom', dt.TimeZone, 'TimeZone', 'UTC');
            end

            % Format the result as an ISO 8601 string
            result = datestr(dt, 'yyyy-mm-ddTHH:MM:SS.FFFZ');
        end


        function data = convertDatetimeToString(data)
            % Recursively converts all datetime fields in a struct (or cell array of structs)
            % to ISO strings.
            %
            % :param data: Struct or cell containing datetime fields.
            % :return: Struct or cell with datetime fields converted to ISO format.

            % Handle cell arrays (e.g. when targets is {struct, struct, ...})
            if iscell(data)
                for i = 1:numel(data)
                    if isstruct(data{i}) || iscell(data{i})
                        data{i} = ultrasat.api.ModelBase.convertDatetimeToString(data{i});
                    elseif isdatetime(data{i}) && ~isempty(data{i})
                        data{i} = ultrasat.api.ModelBase.isoFormat(data{i});
                    end
                end
                return
            end

            % Handle structs (scalar or array)
            if isstruct(data)
                % Handle empty struct arrays
                if isempty(data)
                    return
                end                

                % If struct array, loop over elements
                if numel(data) > 1
                    for k = 1:numel(data)
                        data(k) = ultrasat.api.ModelBase.convertDatetimeToString(data(k));
                    end
                    return
                end

                % Scalar struct — convert its fields
                fields = fieldnames(data);
                for i = 1:numel(fields)
                    fieldName = fields{i};
                    value = data.(fieldName);

                    if isdatetime(value) && ~isempty(value)
                        data.(fieldName) = ultrasat.api.ModelBase.isoFormat(value);

                    elseif isstruct(value) || iscell(value)
                        data.(fieldName) = ultrasat.api.ModelBase.convertDatetimeToString(value);
                    end
                end
            end
        end


        function data = convertStringToDatetime(data)
            % Recursively converts all ISO datetime strings in a struct to datetime objects
            %
            % :param data: Struct containing ISO datetime strings.
            % :return: Struct with datetime strings converted back to datetime.

            if isstruct(data)
                fields = fieldnames(data);
                for i = 1:numel(fields)
                    fieldName = fields{i};
                    value = data.(fieldName);

                    % Convert ISO date string to datetime
                    if (ischar(value) || isstring(value)) && contains(value, 'T') % Heuristic for ISO timestamps
                        try
                            data.(fieldName) = datetime(value, ...
                                'InputFormat', 'yyyy-MM-dd''T''HH:mm:ss.SSS''Z''', ...
                                'TimeZone', 'UTC');
                        catch
                            % If conversion fails, keep original string
                        end

                    % Recursively process struct fields
                    elseif isstruct(value)
                        if isempty(value)
                            % Keep empty structs unchanged
                            data.(fieldName) = struct();
                        elseif numel(value) > 1
                            % Handle struct arrays
                            for j = 1:numel(value)
                                value(j) = ultrasat.api.ModelBase.convertStringToDatetime(value(j));
                            end
                            data.(fieldName) = value;
                        else
                            % Handle single struct
                            data.(fieldName) = ultrasat.api.ModelBase.convertStringToDatetime(value);
                        end
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
