
classdef DataKeeper < handle
    properties
        Num         % Serial number, mostly for debugging
        Data        % Data as cell array or struct
        FlagMat     % Flag matrix, allocated by caller as zeros(1, 2, 'uint32')
        FlagVal     % FlagMat(1) value set by the thread when done; FlagMat(2) is FlagVal on success, else a failure code
        StartTime   % 
        Duration    % Seconds after which a still-pending thread is reported (the data is never released before the thread is done)
        FileName    % File written by the thread, used in messages
        Warned = false; % True after the pending message was printed
    end
    
    methods
        function Obj = DataKeeper(data, flagMat, flagVal, duration, fileName)
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
            if nargin > 4
                Obj.FileName = fileName;
            end
        end
        

        function isExpired = checkExpired(Obj, Now)
            % Check if the data can be released, i.e. the thread is done with it.
            % The data is never released while the thread may still use it
            % (#1391); a thread pending longer than Duration is reported once.

            % Check flag set by the thread upon completion
            if Obj.FlagMat(1) == Obj.FlagVal
                %fprintf('Done: %d\n', obj.Num);
                isExpired = true;
                if Obj.FlagMat(2) ~= Obj.FlagVal
                    % stderr, not warning(): AstroPack startup turns warnings off
                    fprintf(2, 'DataKeeper: background write of %s failed (code %d)\n', ...
                            Obj.FileName, Obj.FlagMat(2));
                end
            else
                isExpired = false;
                elapsedTime = seconds(Now - Obj.StartTime);
                if ~Obj.Warned && elapsedTime > Obj.Duration
                    fprintf(2, 'DataKeeper: background write of %s still pending after %.0f s\n', ...
                            Obj.FileName, elapsedTime);
                    Obj.Warned = true;
                end
            end
        end
    end
end
