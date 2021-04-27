% [StackTrace, WorkspaceIndex] = dbstack;
% 
% if numel(StackTrace) < 2
%     CallerName = 'session';
%     Line      = NaN;
% else
%     CallerName = StackTrace(2).name;
%     Line      = StackTrace(2).line;
% end

classdef MsgLogger < handle
    % Message logger with levels
    
    % Properties
    properties (SetAccess = public)
        CurLevel LogLevel   % Current level
        LogF LogFile        % Log file
        UserData            % Optional
    end
    
    %-------------------------------------------------------- 
    methods
        % Constructor    
        function Obj = MsgLogger()
            Obj.CurLevel = LogLevel.All;
            Obj.LogF = LogFile.getSingleton();
        end
    end
	
    
	methods
        
		function msgLog(Obj, Level, varargin)

            % Do nothing if log is disabled
            if Level == LogLevel.None
                return
            end
            
            % Ignore levels above CurLevel
            if uint32(Level) > uint32(Obj.CurLevel)
                return
            end
            
            if Level == LogLevel.Error
                Obj.msgStyle(Level, '@error', varargin{:});
                return
            end
            
            if Level == LogLevel.Warning
                Obj.msgStyle(Level, '@warn', varargin{:});
                return
            end            
            
            LevStr = getLevelStr(Obj, Level);
            fprintf('[%s] ', LevStr);
            fprintf(varargin{:});
			fprintf('\n');
            
            % Log to file            
            if ~isempty(Obj.LogF)
                Obj.LogF.write2(sprintf('[%s]', LevStr), varargin{:});
            end
            
        end        
             
        
		function msgStyle(Obj, Level, Style, varargin)

            % Do nothing if log is disabled
            if Level == LogLevel.None
                return
            end
            
            % Ignore levels above CurLevel
            if uint32(Level) > uint32(Obj.CurLevel)
                return
            end
            
            LevStr = getLevelStr(Obj, Level);
            cprintf(Style, '[%s] ', LevStr);
            cprintf(Style, varargin{:});
			fprintf('\n');
            
            % Log to file            
            if ~isempty(Obj.LogF)
                Obj.LogF.write2(sprintf('[%s]', LevStr), varargin{:});
            end
            
        end        

        
        function Result = getLevelStr(Obj, Level)
        
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
            Result = s;
        end
        
    
		function msgStack(Obj, Level, varargin)
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
            % Return singleton object
            persistent PersObj
            if isempty(PersObj)
                PersObj = MsgLogger;
            end
            Result = PersObj;
        end
        
        
        function setLogLevel(Level)
            % Set global LogLevel
            m = MsgLogger.getSingleton();
            m.CurLevel = Level;
        end
        
        
        function Value = getLogLevel()
            % Get global LogLevel
            Value = MsgLogger.getSingleton().CurLevel;
        end             

    end
    
    
    methods(Static) % Unit test
        
        function unitTestStackTrace(Count)            
            if Count > 0
                M = MsgLogger.getSingleton();            
                M.msgStack(LogLevel.Test, 'Recursion(%d)', Count);            
                MsgLogger.unitTestStackTrace(Count-1);
            end
        end

        function Result = unitTest()            
            fprintf('MsgLogger test started\n');
            
            % Test cprintf (in external/)
            fprintf('cprintf test started\n');
            cprintf('text',    'regular black text\n');
            cprintf('hyper',   'followed %s', 'by');
            cprintf('-comment','& underlined\n');
            cprintf('err',     'elements: ');
            cprintf('cyan',    'cyan ');
            cprintf('_green',  'underlined green\n');
            fprintf('cprintf test done\n');
            
            % Test MsgLogger
            M = MsgLogger.getSingleton();            
            M.msgLog(LogLevel.Test, 'Test: %d', uint32(LogLevel.Test));
            M.msgLog(LogLevel.Debug, 'Test: %d', uint32(LogLevel.Debug));
            M.msgLog(LogLevel.Info, 'Test: %d', uint32(LogLevel.Info));
            M.msgLog(LogLevel.Warning, 'Test: %d', uint32(LogLevel.Warning));
            M.msgLog(LogLevel.Error, 'Test: %d', uint32(LogLevel.Error));
            M.msgLog(LogLevel.None, 'Test: %d', uint32(LogLevel.None));
            
            M.msgStack(LogLevel.Test, 'MyStackTrace: %d', 123);            
            MsgLogger.unitTestStackTrace(5);
            
            % Test msgStyle
            M.msgStyle(LogLevel.Test, 'blue', 'Message in blue');
            M.msgStyle(LogLevel.Test, 'red', 'Message in red');
            
            fprintf('MsgLogger test passed\n');
            Result = true;
        end
    end
    
end


