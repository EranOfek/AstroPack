% 
% Note that this class is derived from handle (and not from Component)

classdef MsgLogger < handle
    % Message logger to console and file, with log levels
    
    % Properties
    properties (SetAccess = public)
        Enabled logical         % True to enable logging
        CurFileLevel LogLevel   % Current level for log file
        CurDispLevel LogLevel   % Current level for display
        LogF LogFile            % Log file
        UserData                % Optional
    end
    
    %-------------------------------------------------------- 
    methods % Constructor        
        function Obj = MsgLogger()
            Obj.Enabled = true;
            Obj.CurFileLevel = LogLevel.All;
            Obj.CurDispLevel = LogLevel.All;
            Obj.LogF = LogFile.getSingleton();
        end
    end
	
    
	methods
        
		function msgLog(Obj, Level, varargin)
            % Log message to console/file according to current
            % LogLevel settings
            
            % Do nothing if log is disabled
            if ~Obj.Enabled || Level == LogLevel.None
                return
            end
                       
            % Always use msgStyle to print errors in red color
            if Level == LogLevel.Error || Level == LogLevel.Fatal
                Obj.msgStyle(Level, '@error', varargin{:});
                return
            end
            
            % Always use msgStyle to print warnings in red color
            if Level == LogLevel.Warning
                Obj.msgStyle(Level, '@warn', varargin{:});
                return
            end            
            
            % Prepare prompt with level
            LevStr = getLevelStr(Obj, Level);
            
            % Log to display
            if uint32(Level) <= uint32(Obj.CurDispLevel)
                fprintf('%s [%s] ', datestr(now, 'HH:MM:SS.FFF'), LevStr);
                fprintf(varargin{:});                   
    			fprintf('\n');
            end
            
            % Log to file            
            if uint32(Level) <= uint32(Obj.CurFileLevel)
                if ~isempty(Obj.LogF)
                    Obj.LogF.write2(sprintf('[%s]', LevStr), varargin{:});
                end
            end            
        end        
             
        
		function msgStyle(Obj, Level, Style, varargin)
            % Log message to console/file according to current
            % LogLevel settings
            
            % Do nothing if log is disabled
            if ~Obj.Enabled || Level == LogLevel.None
                return
            end
            
            % Prepare prompt with log level
            LevStr = getLevelStr(Obj, Level);
            
            % Log to display
            if uint32(Level) <= uint32(Obj.CurDispLevel)
                cprintf(Style, '[%s] ', LevStr);
                cprintf(Style, varargin{:});
                fprintf('\n');
            end
            
            % Log to file            
            if uint32(Level) <= uint32(Obj.CurFileLevel)
                if ~isempty(Obj.LogF)
                    Obj.LogF.write2(sprintf('[%s]', LevStr), varargin{:});
                end
            end            
        end        

        
		function Result = shouldLog(Obj, Level, CurLevel)
            % Return true if specified Level should be logged according
            % to the specified CurLevel settings
            
            % Do nothing if log is disabled
            if ~Obj.Enabled || Level == LogLevel.None
                Result = false;
                       
            % Error - always use log
            elseif Level == LogLevel.Error || Level == LogLevel.Fatal
                Result = true;
            
            % Warning - always log
            elseif Level == LogLevel.Warning
                Result = true;
                
            % Other level below or equal to current
            elseif uint32(Level) <= uint32(CurLevel)
                Result = true;
                
            else
                Result = false;
            end            
        end        
        
        
        function Result = getLevelStr(Obj, Level)
            % Convert Level enumeation to string
            
            % Convert enum to string
			s = '';
            switch Level        
				case LogLevel.None
					s = 'NON';
				case LogLevel.Fatal
					s = 'FTL';
				case LogLevel.Error
					s = 'ERR';
				case LogLevel.Warning
					s = 'WRN';
				case LogLevel.Info
					s = 'INF';
				case LogLevel.Verbose
					s = 'VRB';                    
				case LogLevel.Debug
					s = 'DBG';
				case LogLevel.Test
					s = 'TST';
				case LogLevel.DebugEx
					s = 'DBX';                    
				otherwise
					error('Unknown LogLevel');
            end
            Result = s;
        end
        
    
		function msgStack(Obj, Level, varargin)
            % Log stack trace
            
            [StackTrace, WorkspaceIndex] = dbstack;

            Obj.msgLog(Level, varargin{:});
            Obj.msgLog(Level, 'StackTrace:');
            
            if numel(StackTrace) < 2
                CallerName = 'session';
                Line      = NaN;
            else
                va = sprintf(varargin{:});
                for i = 2:numel(StackTrace)                    
                    Msg = sprintf('File: %s, Line: #%d, Caller: %s, varargin: %s', StackTrace(i).file, StackTrace(i).line, StackTrace(i).name, va);
                    Obj.msgLog(Level, Msg);            
                end
            end

            %Msg = sprintf('File: %s, Line: #%d, Caller: %s - %s', File, Line, CallerName, sprintf(varargin{:}));
            %Obj.msgLog(Level, Msg);
            Obj.msgLog(Level, '');
        end
    end
    
    
    methods(Static) % Static functions
                
        function Result = getSingleton()
            % Return singleton object, the deafult MsgLogger
            persistent PersObj
            if isempty(PersObj)
                PersObj = MsgLogger;
            end
            Result = PersObj;
        end
        
        
        function setLogLevel(Level, Args)
            % Set current log level, Args.type is 'all', 'file', 'disp'
            arguments
                Level LogLevel
                Args.type = 'all'
            end
            
            % Set global LogLevel
            m = MsgLogger.getSingleton();
            
            % Set file level
            if strcmp(Args.type, 'all') || strcmp(Args.type, 'file')
                m.CurFileLevel = Level;
            end
            
            % Set disp level
            if strcmp(Args.type, 'all') || strcmp(Args.type, 'disp')
                m.CurDispLevel = Level;
            end                                  
        end        
    end
    
    
    methods(Static) % Unit test
        
        
        Result = unitTest()
    end
    
end
