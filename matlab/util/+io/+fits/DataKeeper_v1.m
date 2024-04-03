
classdef DataKeeper
    properties
        Num         %
        Data        %
        Flag        %
        StartTime   %
        Duration    % Duration in seconds after which the data can be released
    end
    
    methods
        function Obj = DataKeeper(imageData, headerData, flag, duration)
            % Constructor
            persistent Counter;
            if isempty(Counter)
                Counter = 0;
            end

            Counter = Counter + 1;
            Obj.Num = Counter;
            Obj.Data = {imageData, headerData};
            Obj.Flag = flag;
            Obj.StartTime = datetime('now');
            Obj.Duration = duration;
        end
        

        function isExpired = checkExpired(Obj, Now)
            % Check if the data is past its expiration time

            % Check flag set by the thread upon completion
            if Obj.Flag(1) == 0x12345678
                %fprintf('Done: %d\n', obj.Num);
                isExpired = true;
            else
                elapsedTime = Now - Obj.StartTime;
                isExpired = seconds(elapsedTime) > Obj.Duration;
            end
        end
    end
end
