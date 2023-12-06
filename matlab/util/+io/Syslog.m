%
% A MsgLogger helper class for forwarding messages to the system's syslog server.
%
% NOTES:
%   - Syslog is the logging facility defined by RFC5424.
%   - We configure the Linux rsyslog service (via /etc/rsyslog.conf) to listen to UDP:514
%
% Author: Arie Blumenzweig (Apr 2023)
%
classdef Syslog < handle
    % Forwarder of MsgLogger messages to syslog
    
    properties (Access = public)
        ServerIp string     % Syslog's server address
        ServerPort          % Syslog's server UDP port
        HostName string     % The current machine's hostname
        ProgName string     % The current process' name
        Pid uint32          % The current process' pid
    end
    
    properties (Access = private)
        UdpSocket           % Socket to the Syslog server
    end
    
    properties(Constant, Hidden)
        % from <syslog.h>
        LOG_KERN     =  0 % kernel messages
        LOG_USER     =  1 % random user-level messages
        LOG_MAIL     =  2 % mail system
        LOG_DAEMON   =  3 % system daemons
        LOG_AUTH     =  4 % security/authorization messages
        LOG_SYSLOG   =  5 % messages generated internally by syslogd
        LOG_LPR      =  6 % line printer subsystem
        LOG_NEWS     =  7 % network news subsystem
        LOG_UUCP     =  8 % UUCP subsystem
        LOG_CRON     =  9 % clock daemon
        LOG_AUTHPRIV = 10 % security/authorization messages (private)
        LOG_FTP      = 11 % ftp daemon

        LOG_LOCAL0	 = 16 % reserved for local use
        LOG_LOCAL1	 = 17 % reserved for local use
        LOG_LOCAL2	 = 18 % reserved for local use
        LOG_LOCAL3	 = 19 % reserved for local use
        LOG_LOCAL4	 = 20 % reserved for local use
        LOG_LOCAL5	 = 21 % reserved for local use
        LOG_LOCAL6	 = 22 % reserved for local use
        LOG_LOCAL7	 = 23 % reserved for local use
            
        LOG_LAST     = 16 % all LAST messages will have the LOG_LOCAL0 facility
    end
    
    properties(Constant, Hidden)
        % from <syslog.h>
        LOG_EMERG	 =  0 % system is unusable
        LOG_ALERT	 =  1 % action must be taken immediately
        LOG_CRIT     =  2 % critical conditions
        LOG_ERR      =  3 % error conditions
        LOG_WARNING	 =  4 % warning conditions
        LOG_NOTICE	 =  5 % normal but significant condition
        LOG_INFO     =  6 % informational
        LOG_DEBUG	 =  7 % debug-level messages
    end
    
    properties(Access = private)
        PriorityMap containers.Map
    end
    
    methods % Constructor
        
        function Obj = Syslog(Args)
            % Construct a Syslog
            % Uses an UDP socket to forward messages to the syslog daemon on the localhost.
            arguments
            	Args.ServerIp string = '127.0.0.1'  % optionally log to another machine
            	Args.ServerPort = 514               % optionally use another UDP port
            end
            
            % Map our LogLevels to the standard (RFC5424) priorities
            Obj.PriorityMap = containers.Map('KeyType', 'double', 'ValueType', 'double');
            
            Obj.PriorityMap(double(LogLevel.Fatal))     = Obj.LOG_CRIT;
            Obj.PriorityMap(double(LogLevel.Error))     = Obj.LOG_ERR;
            Obj.PriorityMap(double(LogLevel.Debug))     = Obj.LOG_DEBUG;
            Obj.PriorityMap(double(LogLevel.Warning))   = Obj.LOG_WARNING;
            Obj.PriorityMap(double(LogLevel.Info))      = Obj.LOG_INFO;
            Obj.PriorityMap(double(LogLevel.Verbose))   = Obj.LOG_INFO;
            Obj.PriorityMap(double(LogLevel.DebugEx))   = Obj.LOG_DEBUG;
            Obj.PriorityMap(double(LogLevel.Test))      = Obj.LOG_DEBUG;
            Obj.PriorityMap(double(LogLevel.unitTest))  = Obj.LOG_DEBUG;
            
            [~, HostName] = system('hostname -s');
            Obj.HostName = HostName(1:end-1);
            Obj.Pid = feature('getpid');
            
            Obj.ServerIp = Args.ServerIp;
            Obj.ServerPort = Args.ServerPort;            
            Obj.UdpSocket = udpport('byte', 'EnablePortSharing', true);
            
            Obj.ProgName = MsgLogger.getProgramName;
            if isempty(Obj.ProgName)
                Obj.ProgName = 'no-program-name';
            end
        end

        function sendMessage(Obj, LogLevel, varargin)
            % Sends a message in RFC5424 format to the syslog server
            
            try
                MappedPriority = Obj.PriorityMap(double(LogLevel));
            catch
                MappedPriority = Obj.LOG_DEBUG;
            end
            
            Message = sprintf('<%d>%s %s %s[%d]: [%s] %s', ...
                MappedPriority + (Obj.LOG_LAST * 8), ...
                datetime('now', 'Format', 'MMM dd HH:mm:ss'), ...
                Obj.HostName, ...
                Obj.ProgName, ...
                Obj.Pid, ...
                MsgLogger.getLevelStr(LogLevel), ...
                sprintf(varargin{:}));

            Obj.UdpSocket.write(Message, Obj.ServerIp, Obj.ServerPort);
        end
        
        function delete(Obj)
            % Destructor
            delete(Obj.UdpSocket);
        end
        
    end
    
    
    methods(Static)
       
        function Result = unitTest()
            
            logger = MsgLogger;
            MsgLogger.setProgramName('syslog-unit-test');
            
            levels = { ...
                LogLevel.Assert,    LogLevel.Debug, LogLevel.DebugEx,   LogLevel.Error,     LogLevel.Fatal, ...
                LogLevel.Info,      LogLevel.Perf,  LogLevel.Test,      LogLevel.Verbose,   LogLevel.Warning ...
            };
            
            try
                for i = 1:numel(levels)
                    l = levels{i};
                    logger.msgLog(l, sprintf("A message of type '%s'", string(l)));
                end
                
                ex = MException('MATLAB:Syslog:unitTest', 'Exception');
                logger.msgLogEx(LogLevel.Error, ex);
                Result = true;
            catch
                Result = false;
                return;
            end            
            
            MsgLogger.unsetProgramName;
            
            delete(logger);
        end
        
    end
end