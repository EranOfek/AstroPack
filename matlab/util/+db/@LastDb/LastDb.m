
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
                Args.DatabaseName  = 'lastdb'        % Use 'postgres' to when creating databases or for general
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
            Q.addColumn(TN, 'niny',     'smallint', 'default 0');
            Q.addColumn(TN, 'camname',  'varchar(80)', "default ''");
            Q.addColumn(TN, 'camtemp',  'single', 'default 0');
            Q.addColumn(TN, 'camcool',  'single', 'default 0');      % what is this?
            Q.addColumn(TN, 'cammode',  'smallint', 'default 0');
            Q.addColumn(TN, 'camgain',  'smallint', 'default 0');
            Q.addColumn(TN, 'camoffs',  'smallint', 'default 0');
            Q.addColumn(TN, 'projname', 'varchar(256)', "default ''");
            Q.addColumn(TN, 'obslon',   'single', 'default 0');
            Q.addColumn(TN, 'obslat',   'single', 'default 0');
            Q.addColumn(TN, 'obsalt',   'single', 'default 0');
            Q.addColumn(TN, 'lst',      'single', 'default 0');
            Q.addColumn(TN, 'date_obs', 'varchar', "default ''");

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

            Obj.msgLog(LogLevel.Info, 'addCommonImageColumns done');
            Result = true;
        end

    end


    methods
        function Result = addRawImage(Obj, FileName, AH)
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
                FileName            % Image file name
                AH                  % AstroHeader
            end

            Q = Obj.Query;

            % Extract file name from full path
            [Path,FName,Ext] = fileparts(FileName);
            FName = strcat(FName, Ext);

            % Add field to header
            AH.insertKey({'filename', FName, 'Image file name'}, 'end');
            
            % Insert header to table
            Q.insert(AH, 'TableName', 'raw_images', 'ColumnsOnly', true);
            Result = true;
        end
        
    end

    
    methods(Static)
        Result = unitTest()
            % LastDb Unit-Test
    end

end


