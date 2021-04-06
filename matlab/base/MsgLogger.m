
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
            Obj.CurLevel = LogLevel.Debug;
        end
    end
	
	
    methods(Static) % Static functions
                
        function Result = getGlobal()
            % Return singleton object
            persistent PersObj
            if isempty(PersObj)
                PersObj = MsgLogger;
            end
            Result = PersObj;
        end
        
        
        function setLogLevel(Level)
            % Set global LogLevel
            m = MsgLogger;
            m.getGlobal().CurLevel = Level;
        end
        
        
        function Value = getLogLevel()
            % Get global LogLevel
            m = MsgLogger;
            Value = m.getGlobal().CurLevel;
        end
                
        
		function msgLog(Level, varargin)
            % Global msgLog
            
            % Ignore levels above CurLevel
            if uint32(Level) > uint32(MsgLogger.getLogLevel())
                return
            end
            
            % Convert enum to string
			s = '';
            switch Level        
				case LogLevel.None
					s = 'None';
				case LogLevel.Error
					s = 'Error';
				case LogLevel.Warning
					s = 'Warning';
				case LogLevel.Info
					s = 'Info';
				case LogLevel.Debug
					s = 'Debug';
				case LogLevel.Test
					s = 'UnitTest';
				otherwise
					error('Unknown LogLevel');
            end
		   
            % Log to console
			fprintf('[%s]: ', s);
            for i=1:nargin-1
                fprintf('%s', string(varargin{i}));
            end
			fprintf('\n');
            
            % Log to file @TODO
		end
	end
end


