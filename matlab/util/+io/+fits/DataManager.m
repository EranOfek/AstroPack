classdef DataManager
    properties
        DataKeepers % A cell array of DataKeeper instances
    end
    
    methods
        function obj = addDataKeeper(obj, dataKeeper)
            if isempty(obj.DataKeepers)
                obj.DataKeepers = {dataKeeper};
            else
                obj.DataKeepers{end+1} = dataKeeper;
            end
        end
        
        function scanAndRelease(obj)
            % Scans all DataKeepers and releases expired ones
            for i = length(obj.DataKeepers):-1:1
                if obj.DataKeepers{i}.checkExpired()
                    disp(['Releasing data that started at ', ...
                          char(obj.DataKeepers{i}.StartTime)]);
                    obj.DataKeepers(i) = []; % Remove from the list
                end
            end
        end
    end
end
