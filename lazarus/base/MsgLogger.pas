

// Message logger with levels
MsgLogger = class(TComponent)
public

  Enabled: Boolean;
  CurFileLevel: LogLevel;
  CurDispLevel: LogLevel;
  LogF: LogFile;
  UserData: ?;

  constructor Create;


constructor MsgLogger.Create;
begin
  Obj.Enabled = true;
  Obj.CurFileLevel = LogLevel.All;
  Obj.CurDispLevel = LogLevel.All;
  Obj.LogF = LogFile.getSingleton();
end;
	
    
	methods
        
		function msgLog(Obj, Level, varargin)

            % Do nothing if log is disabled
            if ~Obj.Enabled || Level == LogLevel.None
                return
            end
                       
            % Error - always usg msgStyle to print in color
            if Level == LogLevel.Error
                Obj.msgStyle(Level, '@error', varargin{:});
                return
            end
            
            % Warning - always usg msgStyle to print in color
            if Level == LogLevel.Warning
                Obj.msgStyle(Level, '@warn', varargin{:});
                return
            end            
            
            % Prepare prompt
            LevStr = getLevelStr(Obj, Level);
            
            % Log to display
            if uint32(Level) <= uint32(Obj.CurDispLevel)
                fprintf('[%s] ', LevStr);
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

            % Do nothing if log is disabled
            if ~Obj.Enabled || Level == LogLevel.None
                return
            end
            
            % Prepare prompt                     
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
				case LogLevel.Verbose
					s = 'VRB';                    
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
        
        
        function setLogLevel(Level, Args)
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
            
            % Set specific log level
            MsgLogger.setLogLevel(LogLevel.Info, 'type', 'file');            
            MsgLogger.setLogLevel(LogLevel.Warning, 'type', 'disp');            
            io.msgLog(LogLevel.Info, 'This should go to file only');            
            io.msgLog(LogLevel.Warning, 'This should go to file and display');
            MsgLogger.setLogLevel(LogLevel.Test, 'type', 'all');
            io.msgLog(LogLevel.Test, 'Back to all');
            
            % Test MsgLogger            
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


