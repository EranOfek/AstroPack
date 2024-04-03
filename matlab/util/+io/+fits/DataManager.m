
classdef DataManager < handle
    properties
        DataKeepers % A cell array of DataKeeper instances
        LastScanTime
        LastFinishTime
    end
    

    methods
        function Obj = DataManager()
            % Constructor
            Obj.LastScanTime = tic;
            Obj.LastFinishTime = 0;
        end


        function Obj = addDataKeeper(Obj, dataKeeper)
            if isempty(Obj.DataKeepers)
                Obj.DataKeepers = {dataKeeper};
            else
                Obj.DataKeepers{end+1} = dataKeeper;
            end

            %
            Obj.LastFinishTime = 0;

            % Scan and release every 1 second
            t = toc(Obj.LastScanTime);
            if t >= 1
                Obj.scanAndRelease();
                Obj.LastScanTime = tic;
            end            
        end
        

        function scanAndRelease(Obj)
            % Scans all DataKeepers and releases expired ones
            len = length(Obj.DataKeepers);
            if len > 0
                fprintf('scanAndRelease: %d\n', length(Obj.DataKeepers));
            end

            Now = datetime('now');
            for i = length(Obj.DataKeepers):-1:1
                if Obj.DataKeepers{i}.checkExpired(Now)
                    %disp(['Releasing data that started at ', char(Obj.DataKeepers{i}.StartTime)]);
                    Obj.DataKeepers(i) = []; % Remove from the list
                end
            end

            %if len ~= length(Obj.DataKeepers)
            %    fprintf('scanAndRelease: %d\n', length(Obj.DataKeepers));
            %end

            if (len > 0) && isempty(Obj.DataKeepers)
                Obj.LastFinishTime = datetime('now');
                fprintf('Finished: %s \n', Obj.LastFinishTime);
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
            % CURRENTLY UNUSED 
            
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
