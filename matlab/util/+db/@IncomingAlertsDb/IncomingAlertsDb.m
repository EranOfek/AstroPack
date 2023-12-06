% db.IncomingAlertsDb
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

classdef IncomingAlertsDb < Component

    properties (SetAccess = public)
        Query         = []      % DbQuery object (filled when a DB object is created)
        ParentFolder  = '';     %
    end

    
    methods % constructor
        
        function Obj = IncomingAlertsDb(Args)
            % Create new IncomingAlertsDb object
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
            % Output   : - New instance of IncomingAlertsDb object
            % Author   : Chen Tishler (07/2023)
            % Examples :
            %   % Create query object width default connection parameters
            %   ADB = IncomingAlertsDb()
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
            Obj.setName('IncomingAlertsDb');
            
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

        
        function Result = getAlertFilename(Obj, Alert)
            % Select incoming alerts from database, using specified filters.
            % Input : - Alert - struct with alert columns (selected from table by selectAlerts)
            %         * Pairs of ...,key,val,...
            %           The following keys are available:                                    
            %
            % Author : Chen Tishler (07/2023)
            
            arguments
                Obj
                Alert
            end            
        end
    end

    
    methods (Static) % unitTest and examples
        Result = unitTest()

    end

end
