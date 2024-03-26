
classdef DataKeeper
    properties
        Num
        ImageData
        HeaderData
        FlagData
        StartTime
        Duration % Duration in seconds after which the data can be released
    end
    
    methods
        function Obj = DataKeeper(imageData, headerData, flagData, duration)
            % Constructor
            persistent Counter;
            if isempty(Counter)
                Counter = 0;
            end

            Counter = Counter+1;
            Obj.Num = Counter;
            Obj.ImageData = imageData;
            Obj.HeaderData = headerData;
            Obj.FlagData = flagData;
            Obj.StartTime = datetime('now');
            Obj.Duration = duration;
        end
        

        function isExpired = checkExpired(Obj, now)
            % Check if the data is past its expiration time

            % Check flag
            if Obj.FlagData(1) == 0x12345678
                %fprintf('Done: %d\n', obj.Num);
                isExpired = true;
            else
                %now = datetime('now');
                elapsedTime = now - Obj.StartTime;
                isExpired = seconds(elapsedTime) > Obj.Duration;
            end
        end
    end
end
