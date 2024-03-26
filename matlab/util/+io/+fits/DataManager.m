
classdef DataManager < handle
    properties
        DataKeepers % A cell array of DataKeeper instances
    end
    

    methods
        function Obj = DataManager()
            % Constructor
        end


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
                    disp(['Releasing data that started at ', char(obj.DataKeepers{i}.StartTime)]);
                    obj.DataKeepers(i) = []; % Remove from the list
                end
            end
        end
    end


    methods(Static)
        function Obj = getSingleton()
            persistent dataManager;
            if isempty(dataManager)
                dataManager = io.fits.DataManager();
            end
            Obj = dataManager;
        end


        function init()
            % Initialize the DataManager instance
            dataManager = DataManager.getSingleton();

            % Create a MATLAB timer that calls dataManager.scanAndRelease() every 5 seconds
            t = timer;
            t.Period = 5; % Set timer period to 5 seconds
            t.ExecutionMode = 'fixedRate'; % Execute the timer callback at a fixed rate
            t.TimerFcn = @(~, ~) dataManager.scanAndRelease(); % Timer callback function
            
            % Start the timer
            start(t);
            
            % To keep the MATLAB command window responsive and prevent the script from terminating immediately,
            % you might want to include a pause, uiwait, or some mechanism to keep the script running.
            uiwait(msgbox('Click OK to stop the timer and exit.', 'Timer Running'));
            
            % When you want to stop and delete the timer, e.g., after the user clicks "OK" on the message box:
            stop(t); % Stop the timer
            delete(t); % Delete the timer object
        end

    end

end
