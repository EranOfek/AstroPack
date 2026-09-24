
classdef DataKeeper < handle
    properties
        Num         % Serial number, mostly for debugging
        Data        % Data as cell array or struct
        FlagMat     % Flag matrix, allocated by caller as zeros(1, 2, 'uint32')
        FlagVal     %
        StartTime   % 
        Duration    % Duration in seconds after which the data can be released
    end
    
    methods
        function Obj = DataKeeper(data, flagMat, flagVal, duration)
            % Constructor
            persistent Counter;
            if isempty(Counter)
                Counter = 0;
            end

            Counter = Counter + 1;
            Obj.Num = Counter;
            Obj.Data = data;
            Obj.FlagMat = flagMat;
            Obj.FlagVal = flagVal;
            Obj.StartTime = datetime('now');
            Obj.Duration = duration;
        end
        

        function isExpired = checkExpired(Obj, Now)
            % Check if the data is past its expiration time

            % Check flag set by the thread upon completion
            if Obj.FlagMat(1) == Obj.FlagVal
                %fprintf('Done: %d\n', obj.Num);
                isExpired = true;
            else
                elapsedTime = Now - Obj.StartTime;
                isExpired = seconds(elapsedTime) > Obj.Duration;
            end
        end
    end
end
