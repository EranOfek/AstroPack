% ***************************************************************************
% Project     : ULTRASAT Observation Planner
% Filename    : DateTimeUtils.m
% Author      : Chen Tishler
% Created     : 16/09/2025
% Modified    : 18/02/2026
% Description : Utility functions for datetime operations
% ***************************************************************************

classdef DateTimeUtils
    methods (Static)

        function dt = toUtc(dt)
            % Converts input to datetime in UTC if needed.
            if isdatetime(dt)
                if isempty(dt.TimeZone) || ~strcmp(dt.TimeZone, 'UTC')
                    dt.TimeZone = 'UTC';
                end
            elseif ischar(dt) || isstring(dt)
                dt = datetime(dt, ...
                    'InputFormat', 'yyyy-MM-dd''T''HH:mm:ss.SSS''Z''', ...
                    'TimeZone', 'UTC');
            else
                error('Input must be a datetime object or a date-time string.');
            end
        end


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
                        data{i} = ultrasat.api.utils.DateTimeUtils.convertDatetimeToString(data{i});
                    elseif isdatetime(data{i}) && ~isempty(data{i})
                        data{i} = ultrasat.api.utils.DateTimeUtils.isoFormat(data{i});
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
                        data(k) = ultrasat.api.utils.DateTimeUtils.convertDatetimeToString(data(k));
                    end
                    return
                end

                % Scalar struct — convert its fields
                fields = fieldnames(data);
                for i = 1:numel(fields)
                    fieldName = fields{i};
                    value = data.(fieldName);

                    if isdatetime(value) && ~isempty(value)
                        data.(fieldName) = ultrasat.api.utils.DateTimeUtils.isoFormat(value);

                    elseif isstruct(value) || iscell(value)
                        data.(fieldName) = ultrasat.api.utils.DateTimeUtils.convertDatetimeToString(value);
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
                                value(j) = ultrasat.api.utils.DateTimeUtils.convertStringToDatetime(value(j));
                            end
                            data.(fieldName) = value;
                        else
                            % Handle single struct
                            data.(fieldName) = ultrasat.api.utils.DateTimeUtils.convertStringToDatetime(value);
                        end
                    end
                end
            end
        end


        function dt = parseIsoDateTime(str)
            % Parse ISO 8601 datetime strings with 'Z' or timezone offsets.
            %
            %   dt = parseIsoDateTime(str)
            %
            %   Supports:
            %       2025-01-01T00:00:00Z
            %       2025-01-01T00:00:00.000Z
            %       2025-01-01T00:00:00.000000Z
            %       2025-01-01T00:00:00+00:00
            %       2025-01-01T00:00:00.000+00:00
            %
            %   Returns datetime with TimeZone = 'UTC'.
            %   Returns NaT if parsing fails.

            dt = NaT;

            try
                if isempty(str)
                    return;
                end

                % Convert string type if needed
                if isstring(str)
                    str = char(str);
                end

                str = strtrim(str);

                % List of acceptable input formats (from most to least precise)
                fmts = { ...
                    'yyyy-MM-dd''T''HH:mm:ss.SSSSSS''Z''', ...
                    'yyyy-MM-dd''T''HH:mm:ss.SSS''Z''', ...
                    'yyyy-MM-dd''T''HH:mm:ss''Z''', ...
                    'yyyy-MM-dd''T''HH:mm:ss.SSSSSSXXX', ...
                    'yyyy-MM-dd''T''HH:mm:ss.SSSXXX', ...
                    'yyyy-MM-dd''T''HH:mm:ssXXX' ...
                };

                % Try each format until one works
                for i = 1:numel(fmts)
                    try
                        dt = datetime(str, 'InputFormat', fmts{i}, 'TimeZone', 'UTC');
                        if ~isnat(dt)
                            return;
                        end
                    catch
                        % continue trying
                    end
                end

                % If still NaT, issue a warning
                if isnat(dt)
                    warning('parseIsoDatetime:UnknownFormat', ...
                        'String does not match expected ISO 8601 formats: "%s"', str);
                end

            catch ME
                warning('parseIsoDatetime:Failed', ...
                    'Failed to parse datetime string "%s": %s', str, ME.message);
            end
        end

    end
end
