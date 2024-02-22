% db.SysDb
%
%
classdef SysDb < Component

    properties (SetAccess = public)
        DbName              = ''        %
        Query               = []        % DbQuery object (filled when a DB object is created)
        TableName    = ''               %
        LastAliveUpdateTime = 0         %
        AliveUpdateInterval = 30        %
        ProcessName         = ''        %
        Pid                 = 0;        %
        CmdLine             = '';       %
        AliveId             = 0;        % Optional
        SartTime            = [];       %
    end

    
    methods % constructor
        
        function Obj = SysDb(Args)
            % Create new DbQuery object
            % To use SSH Tunnel, run SSH on local machine, and specify its port:
            %       ssh -L 63331:localhost:5432 ocs@10.23.1.25
            %
            % Input : - 
            %           * Pairs of ...,key,val,...
            %             The following keys are available:
            %             'Host'      - Host name
            %             'Database'  - Database name
            %             'UserName'  - User name
            %             'Password'  - Password
            %             'Port'      - Port number
            %
            % Output   : - New instance of LastDb object
            % Author   : Chen Tishler (07/2023)
            % Examples :
            %   % Create query object width default connection parameters
            %   Q = DbQuery()
            %
            arguments
                % These arguments are used when both DbQuery and DbCon are NOT set:
                Args.Host          = 'socsrv' %localhost'     % 'socsrv'        % Host name or IP address
                Args.Port          = 5432 % 63331           % 5432            % Port number
                Args.DatabaseName  = 'lastdb'        % 'last_operational' at last0 node
                Args.UserName      = ''      % User name
                Args.Password      = ''      % Password
                Args.ReadConfig    = false;  % read config from a local config file or create an object with the given parameters                
            end
            
            Obj.ProcessName = tools.os.getProcessName();
            Obj.Pid = tools.os.getPid();
            Obj.CmdLine = tools.os.getCmdLine();
            Obj.SartTime = datetime('now', 'TimeZone', 'UTC');
            
            PM = PasswordsManager;
            Args.UserName = PM.search(Args.DatabaseName).User;
            Args.Password = PM.search(Args.DatabaseName).Pass;

            %
            Obj.setName(Args.DatabaseName);
            
            % Create DbQuery object
            Obj.msgLog(LogLevel.Info, 'Connecting to server %s:%d, database: %s, user: %s/%s', Args.Host, Args.Port, Args.DatabaseName, Args.UserName, Args.Password);
            
            if Args.ReadConfig
                Obj.Query = db.DbQuery(Args.DatabaseName);
            else
                Obj.Query = db.DbQuery('Host', Args.Host, 'Port', Args.Port, 'UserName', 'postgres', 'Password', Args.Password, 'DatabaseName', Args.DatabaseName);
                Obj.Query.Conn.ServerSharePath = '/var/samba/pgshare'; 
                Obj.Query.Conn.MountSharePath  = '/media/socsrv_pgshare'; 
            end
            
            % Query database version, to verify that we have a connection
            pgver = Obj.Query.getDbVersion();
            Obj.msgLog(LogLevel.Info, 'Connected, Postgres version: %s', pgver);
            assert(contains(pgver, 'PostgreSQL'));              
        end
        
    end
    
      
    methods % low level addImage and addCatalog functions             
        
        function updateProcessAlive(Obj, Args)
            % Insert source catalog records to src_catalog table
            % Input :  - LastDb object
            %          - Optionally additional columns in struct
            %          * Pairs of ...,key,val,...
            %            The following keys are available:
            %            - 'Force' - 
            %
            % Output  : -
            % Author  : Chen Tishler (02/2023)
            % Example : 
            arguments
                Obj
                Args.Force = false;         %
            end

            % Check elapsed time since last update
            if ~Args.Force
                Elapsed = toc(Obj.LastAliveUpdateTime);
                if Elapsed < Obj.LastAliveUpdateTime
                    return
                end
            end
            
            Obj.LastAliveUpdateTime = tic;
            try
                DT = datetime('now', 'TimeZone', 'UTC');
                Data = struct;
                Data.proc_name = Obj.ProcessTableName;
                Data.proc_type = 'matlab';
                Data.proc_pid = Obj.Pid;
                Data.proc_alive_id = Obj.AliveId;
                Data.proc_alive_time = DT;
                Data.proc_status = 'running';
                
                if Obj.FirstUpdate
                    Obj.FirstUpdate = false;
                    Data.proc_fullpath = mfilename('fullpath');
                    Data.proc_cmdline = Obj.CmdLine;
                    Data.proc_start_time = Obj.StartTime;
                end
                
                Obj.Query.insert(Data, 'TableName', Obj.TableName, 'ColumnsOnly', true, 'Returning', '');
            catch Ex
                Obj.msgLogEx(LogLevel.Error, Ex, 'updateProcessAlive failed');
            end
        end
                
    end

    methods (Static) % unitTest and examples
        
        function getSysTools()
            % Return (and create) singleton object
            persistent Tools
            if isempty(Tools)
                Tools = db.SysDb.();
            end
            
            return Tools;
        end
        
        
        function updateAlive(Args)
            arguments
                Args.Force = false;      %
            end
            Tools = getSysTools();
            Tools.updateProcessAlive('Force', Args.Force);
        end
        
        % Unit-test
        Result = unitTest()
    end

end
