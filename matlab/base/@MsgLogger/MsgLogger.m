% Message Logger to log-file and console.
% See detailed help in constructor.
% Note that this class is derived from handle (and not from Base or Component)
%
% Author: Chen Tishler (Apr 2021)
%

% #functions (autogen)
% MsgLogger -
% getLevelStr - Convert Level enumeation to string
% getSingleton - Return singleton object, the deafult MsgLogger
% msgLog - Log message to console/file according to current LogLevel settings
% msgLogEx - Log exception message
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
        CurSyslogLevel LogLevel % Current level for display
        Console                 % True to print messages also to console
        Enabled logical         % True to enable logging, when false, calling msg..() function will do nothing
        UserData                % Optional user data
        LogF LogFile            % Log file, used internally
        Syslog io.Syslog        % Syslog logger
    end

    %--------------------------------------------------------
    methods % Constructor
        function Obj = MsgLogger(Args)
            % Construct Logger (file and optional console)
            % This class uses internally the LogFile class for actual file access.
            %
            % Input:   'FileName'       - File name, if empty the singleton LogFile is used
            %          'UseTimestamp'   - true - Add current timestamp as prefix to the
            %                             file name, i.e. MsgLogger('FileName', 'MyLog') will actually create
            %                             a file named '2022-01-31__12-05-36-MyLog.log'
            %          'MaxFileSize'    - When > 0, switch file to '.old' when reaches
            %                             file size, when zero, file size is not limited
            %          'Console'        - true to log also to console using fprintf()
            %          'LoadConfig'     - When true (default), settings from
            %                             config/local/MsgLogger.yml or config/MsgLogger.yml are used for
            %                             initial log levels, Console, and MaxFileSize
            %
            % Any component derived from 'Component' class has a Logger property 
            % which by default is set to the singleton logger.
            % This allows by default single log file for all our classes.
            %
            % From any Component derived object:
            %   Obj.msgLog(LogLevel.Debug, 'My message');
            %            
            % To replace the default singleton logger of your class:
            %   Obj.Logger = MsgLogger('FileName', '/tmp/my_log_file');
            %
            % If you need additional loggers for your object, define more
            % properties and create them in your class constructor (or later):
            %
            %   Obj.MySpecialLogger = MsgLogger('FileName', '/tmp/special_log');
            %
            % Then log to this specific logger:
            %   Obj.MySpecialLogger.msgLog(LogLevel.Info, 'Special message text');
            %
            %
            % There is also a global function in 'io' package that uses
            % the singleton logger, to allow non-class and static function to
            % easily use a logger:
            %
            %       io.msgLog(LogLevel.Debug, 'My message');
            %
            %
            % Note: You can always create instances of MsgLogger even when
            %       not using a class. For example, a package function may have a 
            %       logger using 'persistent':
            %
            %       function Result = myFunctionInsidePackage()
            %           persistent Logger;
            %           if isempty(Logger)
            %               Logger = MsgLogger('FileName', 'myFunction_logfile', 'MaxFileSize', 10000);
            %               Logger.msgLog(LogLevel.Info, 'Logger initialized');
            %           end
            %           Logger.msgLog(LogLevel.Info, 'Function called on %s', datestr(now, 'yyyy-mm-dd HH:MM:SS.FFF'));
            %
            %
            % Using log-levels:
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
            %
            arguments
                Args.FileName = 'AstroPackLog'  % Log file name, if empty it uses the singleton object
                Args.UseTimestamp = false       % True to add timestamp to file name
                Args.MaxFileSize = 0            % Maximum file size, used when 'FileName' is not empty, 0 will use the default value from config/MsgLogger.yml
                Args.Console = true             % True to print messages to console
                Args.LoadConfig = true;         % True to load configuration from config/local/MsgLogger.yml or config/MsgLogger.yml
            end
            Obj.Enabled = true;
            Obj.CurFileLevel = LogLevel.All;
            Obj.CurDispLevel = LogLevel.All;
            Obj.CurSyslogLevel = LogLevel.All;
            Obj.Console = Args.Console;
                       
            % Load configuration file only once
            persistent Conf;
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

                % Load config file from config/local/MsgLogger.yml or from config/MsgLogger.yml
                Path = Configuration.getSysConfigPath();
                ConfigFileName = fullfile(Path, 'local', 'MsgLogger.yml');
                if ~isfile(ConfigFileName)
                    ConfigFileName = fullfile(Path, 'MsgLogger.yml');
                end
            
                % Load settings without using Configuration object
                Conf = yaml.ReadYaml(string(ConfigFileName).char);
            end
            
            if ~isempty(Conf)
                Obj.CurFileLevel = eval(Conf.MsgLogger.FileLevel);
                Obj.CurDispLevel = eval(Conf.MsgLogger.DispLevel);
                Obj.CurSyslogLevel = eval(Conf.MsgLogger.SyslogLevel);
                Obj.Console = Conf.MsgLogger.Console;

                % Use MaxFileSize from configuration
                if Args.MaxFileSize == 0
                    Args.MaxFileSize = Conf.MsgLogger.MaxFileSize;
                end                
            end            

            % Always have a default value, even if not set in configuration
            if Args.MaxFileSize == 0
                Args.MaxFileSize = 10000000;
            end
            
            % If file name is not specified, use the singleton file
            % otherwise, create a LogFile instance
            if isempty(Args.FileName)
                Obj.LogF = LogFile.getSingleton();
            else
                Obj.LogF = LogFile(Args.FileName, 'UseTimestamp', Args.UseTimestamp, ...
                    'MaxFileSize', Args.MaxFileSize);
            end
            
            v = ver;
            if isunix && any(strcmp(cellstr(char(v.Name)), 'Instrument Control Toolbox'))
                Obj.Syslog = io.Syslog();
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
			
            % Check if first element is cell array
            is_cell = ~isempty(varargin) && iscell(varargin{1});
            
            if isempty(varargin)
                return;
            end
                
            % Always use msgStyle to print errors in red color
            if (Level == LogLevel.Error || Level == LogLevel.Fatal || Level == LogLevel.Assert) && ~is_cell
                Obj.msgStyle(Level, '@error', varargin{:});
                return
            end

            % Always use msgStyle to print warnings in red color
            if (Level == LogLevel.Warning) && ~is_cell
                Obj.msgStyle(Level, '@warn', varargin{:});
                return
            end
			
			LogToDisplay = Obj.Console && uint32(Level) <= uint32(Obj.CurDispLevel);
			LogToFile = uint32(Level) <= uint32(Obj.CurFileLevel);
			LogToSyslog = uint32(Level) <= uint32(Obj.CurSyslogLevel);				
            if Obj.mustLog(Level)
                LogToDisplay = true;
                LogToFile = true;
                LogToSyslog = true;
            end
            
            % Prepare prompt with level
            LevStr = MsgLogger.getLevelStr(Level);

            % Log to display			
            if LogToDisplay
                if is_cell
                    cellArray = varargin{1};
                    for i = 1:numel(cellArray)
                        msg = sprintf(cellArray{i});
                        if msg == ""
                            continue; % empty mesage
                        end
                        
                        fprintf('%s [%s] ', datestr(now, 'HH:MM:SS.FFF'), LevStr);
                        fprintf(msg);
                        fprintf('\n');
                    end
                else
                    if ~isempty(varargin)
                        msg = sprintf(varargin{:});
                        if msg ~= ""
                            fprintf('%s [%s] ', datestr(now, 'HH:MM:SS.FFF'), LevStr);
                            fprintf(msg);
                            fprintf('\n');
                        end
                    end
                end
            end

            % Log to file
            if LogToFile
                if ~isempty(Obj.LogF)
                    if is_cell
                        cellArray = varargin{1};
                        for i = 1:numel(cellArray)                        
                            Obj.LogF.write2(sprintf('[%s]', LevStr), cellArray{i});
                        end
                    else                    
                        Obj.LogF.write2(sprintf('[%s]', LevStr), varargin{:});
                    end
                end
            end
            
            % Log to Syslog
            if LogToSyslog
                if ~isempty(Obj.Syslog) && ~isempty(Obj.Syslog.UdpSocket)
                    if is_cell
                        cellArray = varargin{1};
                        for i = 1:numel(cellArray)
                            if ~isempty(cellArray{i})
                                Obj.Syslog.sendMessage(Level, cellArray{i});
                            end
                        end
                    else                    
                        Obj.Syslog.sendMessage(Level, varargin{:});
                    end
                end
            end
        end

		function msgStyle(Obj, Level, Style, varargin)
            % Change style of log message.
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
			LogToSyslog = uint32(Level) <= uint32(Obj.CurSyslogLevel);			
            if Obj.mustLog(Level)
                LogToDisplay = true;
                LogToFile = true;
                LogToSyslog = true;
            end            
            
            % Prepare prompt with log level
            LevStr = "[" + MsgLogger.getLevelStr(Level) + "] ";
            
            DateStr = datestr(now, 'hh:MM:SS.FFF') + " ";

            % Log to display
            if LogToDisplay
                cprintf(Style, DateStr);
                cprintf(Style, LevStr);
                cprintf(Style, varargin{:});
                fprintf('\n');
            end

            % Log to file
            if LogToFile
                if ~isempty(Obj.LogF)
                    Obj.LogF.write2(sprintf('[%s]', LevStr), varargin{:});
                end
            end
            
            % Log to Syslog
            if LogToSyslog
                if ~isempty(Obj.Syslog)
                    Obj.Syslog.sendMessage(Level, varargin{:});
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
            DateStr = datestr(now, 'hh:MM:SS.FFF') + " ";
            PrefixStr = newline + DateStr + "[Err] ";
            if isprop(Obj, 'Tag')
                PrefixStr = PrefixStr + Obj.Tag;
            end
            MsgReport = strrep(MsgReport, newline, [PrefixStr]);
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
            %          'type' - 'all', 'file', 'disp', 'syslog'
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

            % Set syslog level
            if strcmp(Args.type, 'all') || strcmp(Args.type, 'syslog')
                m.CurSyslogLevel = Level;
            end
        end
        
        function Result = getLevelStr(Level)
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
        
        %
        % We use a LIFO (stack) to store the program name
        % This allows it to be temporarily set (as in a unit test)
        %  and then restored to the previous value.
        %
        % The ProgramName can be used by, for example, the logging subsystem
        %
        function setProgramName(Name)            
            % Pushes the given Name on top of the ProgramName stack, making
            %  it the current one.
            global ProgramNameLIFO;
            
            if isempty(ProgramNameLIFO)
                ProgramNameLIFO{1} = Name;
            else
                ProgramNameLIFO(2:end+1) = ProgramNameLIFO(1:end);
                ProgramNameLIFO{1} = Name;
            end
        end
        
        function Name = getProgramName()
            % Gets the Name on top of the ProgramName stack
            global ProgramNameLIFO;
            
            if isempty(ProgramNameLIFO)
                Name = [];
            else
                Name = ProgramNameLIFO{1};
            end
        end
        
        function LastName = unsetProgramName()  
            % Pops the ProgramName stack, thus revealing the previous one
            global ProgramNameLIFO;
            
            if ~isempty(ProgramNameLIFO)
                LastName = ProgramNameLIFO{1};
                ProgramNameLIFO(1:end-1) = ProgramNameLIFO(2:end);
                ProgramNameLIFO(end) = [];
            else
                LastName = [];
            end
        end
        
    end


    methods(Static) % Unit test

        Result = unitTest()
            % Unit test
    end

end
