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
                Args.Host          = 'socsrv'           % localhost'     % 'socsrv'        % Host name or IP address
                Args.Port          = 5432               % 63331           % 5432            % Port number
                Args.DatabaseName  = 'lastdb'           % 'last_operational' at last0 node
                Args.UserName      = ''                 % User name
                Args.Password      = ''                 % Password
                Args.TableName     = 'alerts.events';
            end
            
            PM = PasswordsManager;
            Args.UserName = PM.search(Args.DatabaseName).User;
            Args.Password = PM.search(Args.DatabaseName).Pass;

            %
            Obj.setName(Args.DatabaseName);
            
            % Create DbQuery object
            Obj.msgLog(LogLevel.Info, 'Connecting to server %s:%d, database: %s, user: %s/%s', Args.Host, Args.Port, Args.DatabaseName, Args.UserName, Args.Password);
            Obj.Query = db.DbQuery('Host', Args.Host, 'Port', Args.Port, 'UserName', 'postgres', 'Password', Args.Password, 'DatabaseName', Args.DatabaseName);
            
            % Query database version, to verify that we have a connection
            pgver = Obj.Query.getDbVersion();
            Obj.msgLog(LogLevel.Info, 'Connected, Postgres version: %s', pgver);
            assert(contains(pgver, 'PostgreSQL'));   
            
        end
        
    end
    
    methods % user interface functions
        
        function Result = selectAlerts(Obj, Data, Args)
            % insert image header or catalog data into an AstroDb database
            % Input : - Data: 

            %         * ...,key,val,...
            %         'Table'  : table name            
            %         'Type'   : 'img' or 'cat'
            %         'DataDir': if not empty, 'Data' should contain a filename template
            %                    e.g., 'LAST*sci*raw_Image*.fits'
            %         'Hash'   : insert a hash sum for each entry
            %         'Force'  : insert a record from the file if the same
            %                    record from the same file (same hash sum) has been inserted already
            %         'Verbose': print names of the digested files
            % Author : A. Krassilchtchikov (Jun 2023)
            
            arguments
                Obj
                Data                                % input images (file names or AstroImages) or AstroHeaders
                Args.Table        = '';             % table name (by def. take from the Object property)
                Args.Type         = 'img';          % data type: 'img' or 'cat'
                Args.FromTime     = [];
                Args.ToTime       = [];
            end
                      

            Result = true;
            
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
