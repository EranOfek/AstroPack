classdef ModelFactoryBase < handle
    % ModelsFactoryBase - Base class for factories that create model instances.
    
    methods (Static)
        function jsonStr = toJson(structData)
            % toJson - Converts a struct to a JSON string.
            jsonStr = jsonencode(structData);
        end
    end    
end

