classdef ModelBase  % Do we need handle??? < handle
    % ModelBase - Base class for individual models.
    
    properties (Access = public)
        Data % Struct containing the public fields of the model
    end
    
    methods
        function obj = ModelBase(data)
            % Constructor for ModelBase
            obj.Data = data;
        end
        
        function jsonStr = toJson(obj)
            % Converts the Data property to a JSON string
            jsonStr = jsonencode(obj.Data);
        end
        
        function show(obj)
            % Display the current Data
            disp('Model Data:');
            disp(obj.Data);
        end
    end

    methods (Static)
        function cleanedData = removeEmptyFields(data)
            % Recursively remove fields with empty values from a struct
            fields = fieldnames(data);
            cleanedData = struct();
            for i = 1:numel(fields)
                value = data.(fields{i});
                if ~isempty(value)
                    % Retain non-empty fields
                    if isstruct(value)
                        % Recursively clean nested structs
                        cleanedData.(fields{i}) = removeEmptyFields(value);
                    else
                        cleanedData.(fields{i}) = value;
                    end
                end
            end
        end


        function result = isoFormat(dt)
            result = datestr(dt, 'yyyy-mm-ddTHH:MM:SS.FFFZ');
        end
    end
end
