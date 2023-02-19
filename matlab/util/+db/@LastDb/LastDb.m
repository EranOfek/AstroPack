
classdef LastDb < Component

    % Properties
    properties (SetAccess = public)
        Query = []          % DbQuery object
    end


    methods
        function Obj = LastDb(Args)
            % Create new DbQuery obeject
            % To use SSH Tunnel, run SSH on local machine, and specify its port:
            %       ssh -L 63331:localhost:5432 ocs@10.23.1.25
            %
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
            % Author   : Chen Tishler (02/2023)
            % Examples :
            %   % Create query object width default connection parameters
            %   Q = DbQuery()
            %
            arguments
                % These arguments are used when both DbQuery and DbCon are NOT set:
                Args.Host          = 'localhost'     % 'socsrv'        % Host name or IP address
                Args.Port          = 63331           % 5432            % Port number
                Args.DatabaseName  = 'lastdb'        % 'last_operational'
                Args.UserName      = 'postgres'      % User name
                Args.Password      = 'postgres'      % 'PassRoot'      % Password
            end

            %
            Obj.setName('LastDb');
            
            % Create DbQuery object
            Obj.msgLog(LogLevel.Info, 'LastDb: connecting to server %s:%d, database: %s, user: %s/%s', Args.Host, Args.Port, Args.DatabaseName, Args.UserName, Args.Password);
            Obj.Query = db.DbQuery('Host', Args.Host, 'Port', Args.Port, 'UserName', 'postgres', 'Password', Args.Password, 'DatabaseName', Args.DatabaseName);
            
            % Query database version, to verify that we have a connection
            pgver = Obj.Query.getDbVersion();
            Obj.msgLog(LogLevel.Info, 'LastDb: connected, Postgres version: %s', pgver);
            assert(contains(pgver, 'PostgreSQL'));            
        end
    end


    methods
        function Result = createTables(Obj)
            % Create or update definitions of LAST database tables
            % Input :  - LastDb object
            %          * Pairs of ...,key,val,...
            %            The following keys are available:
            % Output  : True on success
            % Author  : Chen Tishler (02/2023)
            % Example : createTables()

            Obj.createTable_raw_images();
            Result = true;
        end


        function Result = createTable_raw_images(Obj)
            % Create or update definitions of LAST database tables
            % Input :  - LastDb object
            %          * Pairs of ...,key,val,...
            %            The following keys are available:
            % Output  : True on success
            % Author  : Chen Tishler (02/2023)
            % Example : createTables()
            arguments
                Obj
            end

            % Create table
            Q = Obj.Query;
            TN = 'raw_images';
            Q.createTable('TableName', TN, 'AutoPk', 'pk', 'Drop', false);
            Result = Obj.addCommonImageColumns(Q, TN);

        end


        function Result = addCommonImageColumns(Obj, Q, TN)
            % Add/update common image columns to table
            % Input :  - LastDb object
            %          - Q - DbQuery object (should be Obj.Query)
            %          - TN - Table name
            %          * Pairs of ...,key,val,...
            %            The following keys are available:
            % Output  : True on success
            % Author  : Chen Tishler (02/2023)
            % Example : createTables()
            arguments
                Obj                 %
                Q                   %
                TN                  %
            end

            Obj.msgLog(LogLevel.Info, 'addCommonImageColumns started');

            % Columns with index
            Q.addColumn(TN, 'filename', 'varchar(256)', '', 'index', true);
            Q.addColumn(TN, 'ra',       'double', 'default 0', 'index', true);
            Q.addColumn(TN, 'dec',      'double', 'default 0', 'index', true);
            Q.addColumn(TN, 'jd',       'double', 'default 0', 'index', true);
            Q.addColumn(TN, 'mount',    'smallint', 'default 0', 'index', true);
            Q.addColumn(TN, 'camnum',   'smallint', 'default 0', 'index', true);
            Q.addColumn(TN, 'imtype',   'varchar(80)', '', 'index', true);

            % Columns without index
            Q.addColumn(TN, 'bitpix',   'smallint', 'default 0');
            Q.addColumn(TN, 'maxis1',   'smallint', 'default 0');
            Q.addColumn(TN, 'maxis2',   'smallint', 'default 0');
            Q.addColumn(TN, 'object',   'varchar', "default ''");
            Q.addColumn(TN, 'expmode',  'varchar(80)', "default ''");
            Q.addColumn(TN, 'counter',  'integer', 'default 0');
            Q.addColumn(TN, 'exptime',  'single', 'default 0');
            Q.addColumn(TN, 'gain',     'single', 'default 0');
            Q.addColumn(TN, 'readnoi',  'single', 'default 0');
            Q.addColumn(TN, 'darkcur',  'single', 'default 0');
            Q.addColumn(TN, 'saturval', 'single', 'default 0');
            Q.addColumn(TN, 'nonlin',   'single', 'default 0');
            Q.addColumn(TN, 'binx',     'smallint', 'default 0');
            Q.addColumn(TN, 'biny',     'smallint', 'default 0');
            Q.addColumn(TN, 'camname',  'varchar(80)', "default ''");
            Q.addColumn(TN, 'camtemp',  'single', 'default 0');
            Q.addColumn(TN, 'camcool',  'single', 'default 0');
            Q.addColumn(TN, 'cammode',  'smallint', 'default 0');
            Q.addColumn(TN, 'camgain',  'smallint', 'default 0');
            Q.addColumn(TN, 'camoffs',  'smallint', 'default 0');
            Q.addColumn(TN, 'projname', 'varchar(256)', "default ''");
            Q.addColumn(TN, 'obslon',   'single', 'default 0');
            Q.addColumn(TN, 'obslat',   'single', 'default 0');
            Q.addColumn(TN, 'obsalt',   'single', 'default 0');
            Q.addColumn(TN, 'lst',      'single', 'default 0');
            Q.addColumn(TN, 'date_obs', 'varchar(80)', "default ''");

            %
            Q.addColumn(TN, 'm_ra',     'double', 'default 0');
            Q.addColumn(TN, 'm_dec',    'double', 'default 0');
            Q.addColumn(TN, 'm_ha',     'double', 'default 0');
            Q.addColumn(TN, 'm_jra',    'double', 'default 0');
            Q.addColumn(TN, 'm_jdec',   'double', 'default 0');
            Q.addColumn(TN, 'm_jha',    'double', 'default 0');
            Q.addColumn(TN, 'ha',       'double', 'default 0');

            %
            Q.addColumn(TN, 'equinox',  'single', 'default 0');
            Q.addColumn(TN, 'm_az',     'single', 'default 0');
            Q.addColumn(TN, 'm_alt',    'single', 'default 0');
            Q.addColumn(TN, 'az',       'single', 'default 0');
            Q.addColumn(TN, 'alt',      'single', 'default 0');
            Q.addColumn(TN, 'airmass',  'single', 'default 0');
            Q.addColumn(TN, 'trk_ra',   'single', 'default 0');
            Q.addColumn(TN, 'trk_dec',  'single', 'default 0');
            Q.addColumn(TN, 'mnttemp',  'single', 'default 0');
            Q.addColumn(TN, 'focus',    'single', 'default 0');
            Q.addColumn(TN, 'prvfocus', 'single', 'default 0');
            
            % Additional
            Q.addColumn(TN, 'procstat', 'varchar(256)', "default ''", 'Comment', 'Additional user data');

            Obj.msgLog(LogLevel.Info, 'addCommonImageColumns done');
            Result = true;
        end

        
        function Result = setupSSH(Obj, Args)
            % Setup SSH Tunnel. DO NOT USE YET, we need to solve how to send
            % password to the command line.
            % Input :  - LastDb object
            %          - Q - DbQuery object (should be Obj.Query)
            %          - TN - Table name
            %          * Pairs of ...,key,val,...
            %            The following keys are available:
            % Output  : True on success
            % Author  : Chen Tishler (02/2023)
            % Example : createTables()
            arguments
                Obj                         %
                Args.Host = 'localhost'     %
                Args.LocalPort = 63331      % Image file name
            end
            
            Cmd = 'ssh -L 63331:localhost:5432 ocs@10.23.1.25';

            Obj.msgLog(LogLevel.Info, 'psql: system( %s )', Cmd);
            [Status, Output] = system(Cmd);
            Obj.msgLog(LogLevel.Info, 'psql: %d', Status);
            Obj.msgLog(LogLevel.Info, 'psql: %s', Output);
            if Status ~= 0
                Obj.msgLog(LogLevel.Error, 'runPsql: FAILED to execute, make sure that psql is found on your PATH: %s', Cmd);
            end            
        end
        
    end


    methods
        function Result = addRawImage(Obj, FileName, AH, AddCols)
            % Insert RAW image columns to raw_images table
            % Input :  - LastDb object
            %          - FileName
            %          - AstroHeader
            %          - Optionally additional columns in struct
            %          * Pairs of ...,key,val,...
            %            The following keys are available:
            % Output  : True on success
            % Author  : Chen Tishler (02/2023)
            % Example : createTables()
            arguments
                Obj                 %
                FileName            % Image file name
                AH                  % AstroHeader
                AddCols = []        % struct
            end

            Result = Obj.addImage('raw_images', FileName, AH, AddCols);
        end
        
        
        function Result = addProcImage(Obj, FileName, AH, AddCols)
            % Insert PROC image columns to table
            % Input :  - LastDb object
            %          - FileName
            %          - AstroHeader
            %          - Optionally additional columns in struct
            %          * Pairs of ...,key,val,...
            %            The following keys are available:
            % Output  : True on success
            % Author  : Chen Tishler (02/2023)
            % Example : createTables()
            arguments
                Obj                 %
                FileName            % Image file name
                AH                  % AstroHeader
                AddCols = []        % struct
            end

            Result = Obj.addImage('proc_images', FileName, AH, AddCols);
        end
        
                
        function Result = addImage(Obj, TableName, FileName, AH, AddCols)
            % Insert AstroHeader to specified table.
            % Input :  - LastDb object
            %          - TableName
            %          - FileName
            %          - AstroHeader
            %          - struct - Optionally additional columns. MUST BE lowercase!
            %          * Pairs of ...,key,val,...
            %            The following keys are available:
            % Output  : True on success
            % Author  : Chen Tishler (02/2023)
            % Example : createTables()
            arguments
                Obj                 %
                TableName           %
                FileName            % Image file name
                AH                  % AstroHeader
                AddCols = []        % struct
            end

            Q = Obj.Query;

            % Add FileName to header
            AH.insertKey({'filename', FileName, 'Image file name'}, 'end');
            
            % Add additional columns from struct to AstroHeader
            if ~isempty(AddCols)
                Fields = fieldnames(AddCols);
                for i=1:numel(Fields)
                    Field = Fields{i};
                    Value = AddCols.(Field);
                    Field = lower(Field);
                    AH.insertKey({Field, Value, ''}, 'end');
                end
            end
            
            % Insert AstroHeader to table
            Q.insert(AH, 'TableName', TableName, 'ColumnsOnly', true);
            Result = true;
        end
        
    end

    
    methods(Static)
        Result = unitTest()
            % LastDb Unit-Test
    end

end


