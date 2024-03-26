
classdef DataKeeper
    properties
        ImageData
        HeaderData
        StartTime
        Duration % Duration in seconds after which the data can be released
    end
    
    methods
        function obj = DataKeeper(imageData, headerData, duration)
            % Constructor

            obj.ImageData = imageData;
            obj.HeaderData = headerData;
            obj.StartTime = datetime('now');
            obj.Duration = duration;
        end
        

        function isExpired = checkExpired(obj)
            % Check if the data is past its expiration time
            elapsedTime = datetime('now') - obj.StartTime;
            isExpired = seconds(elapsedTime) > obj.Duration;
        end
    end
end
