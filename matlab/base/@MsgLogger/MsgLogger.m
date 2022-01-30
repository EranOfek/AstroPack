% Message Logger to log-file and console
% Note that this class is derived from handle (and not from Component)
%
% Author: Chen Tishler (Apr 2021)
%

% #functions (autogen)
% MsgLogger -
% getLevelStr - Convert Level enumeation to string
% getSingleton - Return singleton object, the deafult MsgLogger
% msgLog - Log message to console/file according to current LogLevel settings
% msgStack - Log stack trace
% msgStyle - Log message to console/file according to current LogLevel settings
% setLogLevel - Set current log level, Args.type is 'all', 'file', 'disp'
% shouldLog - Return true if specified Level should be logged according to the specified CurLevel settings
% #/functions (autogen)
%

classdef MsgLogger < handle
    % Message logger to console and file, with log levels

    % Properties
    properties (SetAccess = public)
        CurFileLevel LogLevel   % Current level for log file
        CurDispLevel LogLevel   % Current level for display
        Console                 % True to print messages to console
        Enabled logical         % True to enable logging        
        LogF LogFile            % Log file, used internally
        UserData                % Optional
    end

    %--------------------------------------------------------
    methods % Constructor
        function Obj = MsgLogger(Args)
            % Construct Logger (file and console)
            % Input:   'FileName'       - File name, if empty is the singleton LogFile instance
            %          'UseTimestamp'   - true - 
            %          'MaxFileSize'    - > 0, switch file to '.old' when reaches file size
            %          'Console'        - true to log also to console using fprintf()
            %          'LoadConfig'     - 
            %
            % Example: 
            %   M = MsgLogger.getSingleton()
            %   M.msgLog(LogLevel.Debug, 'My message');
            %
            % From any object:            
            %   Obj.msgLog(LogLevel.Debug, 'My message');
            %
            % 
            %   MyLog = MsgLogger('FileName', '/tmp/my_log_file');
            %   MyLog.msgLog(LogLevel.Debug,  'My log message');
            %

            %
            % There are two options to use MsgLogger, one with the singleton
            % instance, and the other by creating an object instance.
            % This allows having multiple log files in the system.
            %
            % 1. Usage with a singleton logger:
            %
            %   You can get the singleton instance by calling MsgLogger.getSingleton()
            %   or by using io.msgLog():
            %
            %       M = MsgLogger.getSingleton()
            %       M.msgLog(LogLevel.Debug, 'My message');
            %
            %   is equvalent to
            %
            %       io.msgLog(LogLevel.Debug, 'My message');
            %
            %   Note that each class derived from Component has log functions
            %   that use its 'Log' property (Log MsgLogger) which is set
            %   by Component's constructor to the singleton,
            %   i.e. Obj.Log = MsgLogger.getSingleton()
            %
            %   Call first to create the singleton with the file name.
            %   Must be called before creating Configuration object which
            %   uses io.msgLog().
            %
            %   Settings UseTimestamp=true add the current system time to
            %   the file name, for example:
            %   '/tmp/2021-10-27__14-44-10-OtherLogFile'
            %
            %   UseTimestamp=true will add the current date/time to the 
            %   file name, so each time the process is started, a new log 
            %   file will be created, allowing you to differentiate the executions.
            %
            %   LogFileName = '/tmp/SystemLogFileName.log';
            %   MsgLogger.getSingleton('FileName', LogFileName, 'UseTimestamp', true);
            %
            %   See base/LogLevel.m for list of log levels.
            %   Set log level for log file, only messages with log level 
            %   with this and higher priority  will be logged to the file.
            %   MsgLogger.setLogLevel(LogLevel.Info, 'type', 'file');            
            %
            %   Set log level for log file, only messages with log level 
            %   with this and higher priority  will be logged to console.
            %   MsgLogger.setLogLevel(LogLevel.Warning, 'type', 'disp');            
            %
            %   io.msgLog(LogLevel.Info, 'This should go to file only');              
            %   io.msgLog(LogLevel.Warning, 'This should go to file and display');
            %
            % 2. Usage with user instnace, it allows having many loggers in the system:
            %
            %   MyLog = MsgLogger('FileName', fullfile(Path, 'OtherLogFile'), 'UseTimestamp', true);
            %   MyLog.setLogLevel(LogLevel.Info, 'type', 'file');            
            %   MyLog.setLogLevel(LogLevel.Warning, 'type', 'disp');                
            %   MyLog.msgLog(LogLevel.Test,    'Test: %d', uint32(LogLevel.Test));
            %   MyLog.msgLog(LogLevel.Debug,   'Test: %d', uint32(LogLevel.Debug));
            %   MyLog.msgLog(LogLevel.Info,    'Test: %d', uint32(LogLevel.Info));
            %
            %
            arguments
                Args.FileName = 'AstroPackLog'  % Log file name, if empty it uses the singleton object
                Args.UseTimestamp = false       % True to add timestamp to file name
                Args.MaxFileSize = 10000000     % Maximum file size, used when 'FileName' is not empty
                Args.Console = true             % True to print messages to console
                Args.LoadConfig = true;         % True to load configuration settings from config/MsgLogger.yml
            end
            Obj.Enabled = true;
            Obj.CurFileLevel = LogLevel.All;
            Obj.CurDispLevel = LogLevel.All;
            Obj.Console = Args.Console;
            
            if isempty(Args.FileName)
                Obj.LogF = LogFile.getSingleton();
            else
                Obj.LogF = LogFile(Args.FileName, 'UseTimestamp', Args.UseTimestamp, ...
                    'MaxFileSize', Args.MaxFileSize);
            end
            
            %
            persistent ConfigLoaded;
            persistent AddPath;                            
            if Args.LoadConfig && isempty(ConfigLoaded)
                ConfigLoaded = true;
                if isempty(AddPath)
                    AddPath = true;
                    MyFileName = mfilename('fullpath');
                    [MyPath, ~, ~] = fileparts(MyFileName);
                    ExternalPath = fullfile(MyPath, '..', '..', 'external');
                    addpath(ExternalPath);                
                end

                % Load config file from config/local/ or from config/
                Path = Configuration.getSysConfigPath();
                FileName = fullfile(Path, 'local', 'MsgLogger.yml');
                if ~isfile(FileName)
                    FileName = fullfile(Path, 'MsgLogger.yml');
                end
                
                % Load settings
                Conf = yaml.ReadYaml(string(FileName).char);                
                if ~isempty(Conf)
                    Obj.CurFileLevel = eval(Conf.MsgLogger.FileLevel);
                    Obj.CurDispLevel = eval(Conf.MsgLogger.DispLevel);
                    Obj.Console = Conf.MsgLogger.Console;
                    
                    % 
                    Obj.LogF.MaxFileSize = Conf.MsgLogger.MaxFileSize;
                end            
            end
        end
    end


	methods

		function msgLog(Obj, Level, varargin)
            % Log message to console/file according to current LogLevel settings
            % Input:   Level    - LogLevel enumeration, see LogLevel.m
            %          varargin - Any fprintf arguments
            % Output:  -
            % Example: Obj.msgLog(LogLevel.Debug, 'Elapsed time: %f', toc)
            
            % Do nothing if log is disabled
            if ~Obj.Enabled || Level == LogLevel.None
                return
            end
			
            % Always use msgStyle to print errors in red color
            if Level == LogLevel.Error || Level == LogLevel.Fatal || Level == LogLevel.Assert
                Obj.msgStyle(Level, '@error', varargin{:});
                return
            end

            % Always use msgStyle to print warnings in red color
            if Level == LogLevel.Warning
                Obj.msgStyle(Level, '@warn', varargin{:});
                return
            end
			
			LogToDisplay = Obj.Console && uint32(Level) <= uint32(Obj.CurDispLevel);
			LogToFile = uint32(Level) <= uint32(Obj.CurFileLevel);			
            if Obj.mustLog(Level)
                LogToDisplay = true;
                LogToFile = true;
            end
            
            % Prepare prompt with level
            LevStr = getLevelStr(Obj, Level);

            % Log to display			
            if LogToDisplay
                fprintf('%s [%s] ', datestr(now, 'HH:MM:SS.FFF'), LevStr);
                fprintf(varargin{:});
    			fprintf('\n');
            end

            % Log to file
            if LogToFile
                if ~isempty(Obj.LogF)
                    Obj.LogF.write2(sprintf('[%s]', LevStr), varargin{:});
                end
            end
        end


		function msgStyle(Obj, Level, Style, varargin)
            % Log message to console/file according to current LogLevel settings
            % Input:   Level    - LogLevel enumeration, see LogLevel.m
            %          Style    - 'red', 'blue', etc., see cprintf.m
            %          varargin - Any fprintf arguments
            % Output:  -
            % Example: Obj.msgStype(LogLevel.Debug, 'red', 'Elapsed time: %f', toc)
            
            % Do nothing if log is disabled
            if ~Obj.Enabled || Level == LogLevel.None
                return
            end

			LogToDisplay = Obj.Console && uint32(Level) <= uint32(Obj.CurDispLevel);
			LogToFile = uint32(Level) <= uint32(Obj.CurFileLevel);			
            if Obj.mustLog(Level)
                LogToDisplay = true;
                LogToFile = true;
            end            
            
            % Prepare prompt with log level
            LevStr = getLevelStr(Obj, Level);

            % Log to display
            if LogToDisplay
                cprintf(Style, '[%s] ', LevStr);
                cprintf(Style, varargin{:});
                fprintf('\n');
            end

            % Log to file
            if LogToFile
                if ~isempty(Obj.LogF)
                    Obj.LogF.write2(sprintf('[%s]', LevStr), varargin{:});
                end
            end
        end


		function msgLogEx(Obj, Level, Ex, varargin)
            % Log MException message to console/file according to current LogLevel settings
            % Input:   Level    - LogLevel enumeration, see LogLevel.m
            %          Ex       - MException object
            %          varargin - Any fprintf arguments
            % Output:  -
            % Example: Obj.msgLogEx(LogLevel.Debug, Ex, 'Function failed, elapsed time: %f', toc)
            MsgReport = getReport(Ex, 'extended', 'hyperlinks', 'off');
            if ~isempty(varargin)
                Msg = sprintf('Exception: %s - %s - %s - %s', Ex.identifier, Ex.message, MsgReport, sprintf(varargin{:}));
            else
                Msg = sprintf('Exception: %s - %s - %s', Ex.identifier, Ex.message, MsgReport);
            end
            Obj.msgStyle(Level, 'red', Msg);
        end
        
        
		function Result = shouldLog(Obj, Level, CurLevel)
            % Return true if specified Level should be logged according
            % to the specified CurLevel settings
            % Input:   Level    - 
            %          CurLevel - 
            % Output:  true/false according to specified Level and CurLevel
            % Example: Obj.shouldLog(LogLevel.Debug, Obj.CurLevel)
            
            % Do nothing if log is disabled
            if ~Obj.Enabled || Level == LogLevel.None
                Result = false;

            % Error - always use log
            elseif Level == LogLevel.Error || Level == LogLevel.Fatal || Level == LogLevel.Assert
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


        function Result = mustLog(Obj, Level)
            % Return true if specified Level should be logged according
            % Input:   Level - 
            % Output:  true/false
            % Example: Obj.mustLog(LogLevel.Debug)
            Result = false;
			if Level == LogLevel.Error || Level == LogLevel.Fatal || Level == LogLevel.Assert || ...
               Level == LogLevel.Warning || Level == LogLevel.Test
                Result = true;
            end
        end
                
        
        function Result = getLevelStr(Obj, Level)
            % Convert Level enumeation to string
            % Input:   Level - LogLevel enumeration (in file LogLeve.m)
            % Output:  char array
            % Example: getLevelStr(LogLevel.Error)
            switch Level
				case LogLevel.None
					s = 'NON';
				case LogLevel.Fatal
					s = 'FTL';
				case LogLevel.Error
					s = 'ERR';
				case LogLevel.Assert
					s = 'ASR';
				case LogLevel.Warning
					s = 'WRN';
				case LogLevel.Info
					s = 'INF';
				case LogLevel.Verbose
					s = 'VRB';
				case LogLevel.Debug
					s = 'DBG';
				case LogLevel.DebugEx
					s = 'DBX';
                case LogLevel.Perf
                    s = 'PRF';
				case LogLevel.Test
					s = 'TST';                    
				case LogLevel.All
					s = 'ALL';
				otherwise
					error('Unknown LogLevel');
            end
            Result = s;
        end


		function msgStack(Obj, Level, varargin)
            % Log stack trace, @Todo - NOT fully tested yet!
            % Input:   Level
            %          varargin - fprintf arguments
            % Output:  Log to file and console with msgLog()
            % Example: Obj.msgStack(LogLevel.Error);
            
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

        function Result = getSingleton(Args)
            % Return singleton object, the default MsgLogger
            % Input:   'FileName'       - 
            %          'UseTimestamp'   -
            %          'Console'        - 
            % Output:  MsgLogger object
            % Example: Logger = MsgLogger.getSingleton()
            
            arguments
                Args.FileName       = 'AstroPackLog'    % File name, if empty, default name is used
                Args.UseTimestamp   = false             % True to add timestamp to file name
                Args.Console        = true              % True to enable console output
            end
            
            persistent PersObj
            if isempty(PersObj)
                PersObj = MsgLogger('FileName', Args.FileName, 'UseTimestamp', ...
                    Args.UseTimestamp, 'Console', Args.Console);
            end
            Result = PersObj;
        end


        function setLogLevel(Level, Args)
            % Set current log level, Args.type is 'all', 'file', 'disp'
            % Input:   Level  - 
            %          'type' - 'all', 'file', 'disp'
            % Output:  -
            % Example: MsgLogger.getSingleton().setLogLevel(LogLevel.Debug)
            
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
            % Unit test
    end

end
