% db.AlertsDb
%
% Properties :
%         Query       - DbQuery object (filled when a DB object is created)
%
% Functionality :
%
% AlertsDb -
%
% TODO:
% 
%
% Examples:
%
%
%

classdef AlertsDb < Component

    properties (SetAccess = public)
        Query       = []      % DbQuery object (filled when a DB object is created)
       
    end

    methods % construction of an AstroDb object 
        
        function Obj = AlertsDb(Args)
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
            % Output   : - New instance of AlertsDb object
            % Author   : Chen Tishler (07/2023)
            % Examples :
            %   % Create query object width default connection parameters
            %   Q = DbQuery()
            %
            arguments
                % These arguments are used when both DbQuery and DbCon are NOT set:
                Args.Host          = 'socsrv'           % Host name or IP address
                Args.Port          = 5432               % Port number
                Args.DatabaseName  = 'socdb'            % Database name
                Args.UserName      = ''                 % User name
                Args.Password      = ''                 % Password
                Args.TableName     = 'alerts.events';   % Alerts table name
            end
            
            % Retreive user/password 
            PM = PasswordsManager;
            Args.UserName = PM.search(Args.DatabaseName).User;
            Args.Password = PM.search(Args.DatabaseName).Pass;

            %
            Obj.setName('AlertsDb');
            
            % Create DbQuery object
            Obj.msgLog(LogLevel.Info, 'Connecting to server %s:%d, database: %s, user: %s/%s', Args.Host, Args.Port, Args.DatabaseName, Args.UserName, '***');  %Args.Password);
            Obj.Query = db.DbQuery('Host', Args.Host, 'Port', Args.Port, 'UserName', 'postgres', 'Password', Args.Password, 'DatabaseName', Args.DatabaseName);
            
            % Query database version, to verify that we have a connection
            pgver = Obj.Query.getDbVersion();
            Obj.msgLog(LogLevel.Info, 'Connected, Postgres version: %s', pgver);
            assert(contains(pgver, 'PostgreSQL'));   
            
        end
        
    end
    
    
    methods % user interface functions
        
        function Result = selectAlerts(Obj, Args)
            % Select incoming alerts from database, using specified filters.
            % Input : 
            %         * Pairs of ...,key,val,...
            %           The following keys are available:                                    
            %         'Type'      : 
            %         'FromTime'  :
            %         'ToTime'    :
            % Author : Chen Tishler (07/2023)
            
            arguments
                Obj
                Args.Type         = [];
                Args.FromTime     = [];
                Args.ToTime       = [];
                Args.Limit        = -1;
            end
            
            % Prepare query
            Where = sprintf('alert_source = ''lvc'' ');
            if ~isempty(Args.Type)
                Where = [Where, sprintf(' AND alert_type = ''%s'' ', Args.Type)];
            end
            
            if ~isempty(Args.FromTime) && ~isempty(Args.ToTime)
                From = datestr(Args.FromTime, 'yyyy-mm-dd HH:MM:SS');
                To = datestr(Args.ToTime, 'yyyy-mm-dd HH:MM:SS');
                Where = [Where, sprintf(' AND alert_event_time >= ''%s'' and alert_event_time <= ''%s'' ', From, To)];
            end
            
            % Execute the query
            DataSet = Q.select('*', Obj.TableName, 'Where', Where, 'Limit', Limit, 'Order', 'pk');
            
            Result = DataSet;            
        end

    end

    methods (Static) % setup SSH tunnel (TBD)
        function Result = setupSSH(Args)
            % Setup SSH Tunnel. DO NOT USE YET, we need to solve how to send
            % password to the command line.
            % Input :  - LastDb object
            %          - Q - DbQuery object (should be Obj.Query)
            %          - TN - Table name
            %          * Pairs of ...,key,val,...
            %            The following keys are available:
            % Output  : True on success
            % Author  : Chen Tishler (02/2023)
            % Example : 
            % 'ssh -L 63331:localhost:5432 ocs@10.23.1.25 &';
            arguments
                Args.Host = 'localhost'         %
                Args.Port = 63331               % Image file name
                Args.RemoteHost = '10.23.1.25';
                Args.RemotePort = 5432;
                Args.User = 'ocs';
                Args.Password = 'physics';
            end
            
            if tools.os.iswindows()                        
                Cmd = sprintf('ssh -L %d:%s:%d %s@%s', Args.Port, Args.Host, Args.RemotePort, Args.User, Args.RemoteHost);
                io.msgLog(LogLevel.Info, 'Execute and enter password: %s', Cmd);
                Cmd = [];
            else
                if ~isempty(Args.Password)
                    Cmd = sprintf('sshpass -p %s ssh -L %d:%s:%d %s@%s &', Args.Password, Args.Port, Args.Host, Args.RemotePort, Args.User, Args.RemoteHost);             
                else
                    Cmd = sprintf('ssh -L %d:%s:%d %s@%s &', Args.Port, Args.Host, Args.RemotePort, Args.User, Args.RemoteHost);
                    io.msgLog(LogLevel.Info, 'Execute and enter password: %s', Cmd);
                    Cmd = [];
                end
            end

            %
            if ~isempty(Cmd)
                io.msgLog(LogLevel.Info, 'setupSSH: system( %s )', Cmd);
                [Status, Output] = system(Cmd);
                io.msgLog(LogLevel.Info, 'setupSSH: %d', Status);
                io.msgLog(LogLevel.Info, 'setupSSH: %s', Output);
                if Status ~= 0
                    io.msgLog(LogLevel.Error, 'setupSSH: FAILED to execute, make sure that psql is found on your PATH: %s', Cmd);
                end            
            end
        end

    end

    methods (Static) % unitTest and examples
        Result = unitTest()

    end

end
