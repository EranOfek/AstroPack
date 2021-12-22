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
        Enabled logical         % True to enable logging
        CurFileLevel LogLevel   % Current level for log file
        CurDispLevel LogLevel   % Current level for display
        Console                 % True to print messages to console
        LogF LogFile            % Log file
        UserData                % Optional
    end

    %--------------------------------------------------------
    methods % Constructor
        function Obj = MsgLogger(Args)
            % Usage with a singleton logger:
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
            arguments
                Args.FileName = 'AstroPackLog'  % Log file name, if empty it uses the singleton object
                Args.UseTimestamp = false       % True to add timestamp to file name
                Args.Console = true             % True to print messages to console
                Args.LoadConfig = true;         % True to load configuration settings
            end
            Obj.Enabled = true;
            Obj.CurFileLevel = LogLevel.All;
            Obj.CurDispLevel = LogLevel.All;
            Obj.Console = Args.Console;
            
            if isempty(Args.FileName)
                Obj.LogF = LogFile.getSingleton();
            else
                Obj.LogF = LogFile(Args.FileName, 'UseTimestamp', Args.UseTimestamp);
            end
            
            %
            persistent ConfigLoaded;
            if Args.LoadConfig && isempty(ConfigLoaded)
                ConfigLoaded = true;
                persistent AddPath;
                if isempty(AddPath)
                    AddPath = true;
                    MyFileName = mfilename('fullpath');
                    [MyPath, ~, ~] = fileparts(MyFileName);
                    ExternalPath = fullfile(MyPath, '..', '..', 'external');
                    addpath(ExternalPath);                
                end
            
                Path = Configuration.getSysConfigPath();
                FileName = fullfile(Path, 'MsgLogger.yml');
                Conf = yaml.ReadYaml(string(FileName).char);
                
                if ~isempty(Conf)
                    Obj.CurFileLevel = eval(Conf.MsgLogger.FileLevel);
                    Obj.CurDispLevel = eval(Conf.MsgLogger.DispLevel);
                    Obj.Console = Conf.MsgLogger.Console;
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
            % Input:  
            % Output: 
            % Example: 
            % Convert enum to string
			s = '';
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
            %          'type' - 
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
