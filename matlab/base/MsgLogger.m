
classdef MsgLogger < handle
    % Message logger with levels
    
    % Properties
    properties (SetAccess = public)
        CurLevel LogLevel   % Current level
        UserData            % Optional
    end
    
    %-------------------------------------------------------- 
    methods
        % Constructor    
        function Obj = MsgLogger()
            Obj.CurLevel = LogLevel.All;
        end
    end
	
	
    methods(Static) % Static functions
                
        function Result = getSingle()
            % Return singleton object
            persistent PersObj
            if isempty(PersObj)
                PersObj = MsgLogger;
            end
            Result = PersObj;
        end
        
        
        function setLogLevel(Level)
            % Set global LogLevel
            m = MsgLogger.getSingle();
            m.CurLevel = Level;
        end
        
        
        function Value = getLogLevel()
            % Get global LogLevel
            Value = MsgLogger.getSingle().CurLevel;
        end
                
        
		function msgLog(Level, varargin)
            % Global msgLog

            % Do nothing if log is disabled
            if Level == LogLevel.None
                return
            end
            
            % Ignore levels above CurLevel
            if uint32(Level) > uint32(MsgLogger.getLogLevel())
                return
            end
            
            % Convert enum to string
			s = '';
            switch Level        
				case LogLevel.None
					s = 'NON';
				case LogLevel.Error
					s = 'ERR';
				case LogLevel.Warning
					s = 'WRN';
				case LogLevel.Info
					s = 'INF';
				case LogLevel.Debug
					s = 'DBG';
				case LogLevel.Test
					s = 'TST';
				otherwise
					error('Unknown LogLevel');
            end
            
            fprintf('[%s] ', s);
            fprintf(varargin{:});
			fprintf('\n');
            
            % Log to file            
            f = LogFile.getSingle();
            f.write2(sprintf('[%s]', s), varargin{:});
            
		end
    end
    
    
    methods(Static) % Unit test
        function Result = unitTest()            
            fprintf("MsgLog test started\n");
            
            MsgLogger.msgLog(LogLevel.Test, 'Test: %d', uint32(LogLevel.Test));
            MsgLogger.msgLog(LogLevel.Debug, 'Test: %d', uint32(LogLevel.Debug));
            MsgLogger.msgLog(LogLevel.Info, 'Test: %d', uint32(LogLevel.Info));
            MsgLogger.msgLog(LogLevel.Warning, 'Test: %d', uint32(LogLevel.Warning));
            MsgLogger.msgLog(LogLevel.Error, 'Test: %d', uint32(LogLevel.Error));
            MsgLogger.msgLog(LogLevel.None, 'Test: %d', uint32(LogLevel.None));
            
            fprintf("MsgLog test passed\n");
        end
    end
    
end


