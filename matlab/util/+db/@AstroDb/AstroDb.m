
classdef AstroDb < Component

    % Properties
    properties (SetAccess = public)
        Query = []          % DbQuery object
        Telescope = 'LAST'  % Telescope object
    end


    methods
        
        function Obj = AstroDb(Args)
            % Create new DbQuery object
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
                Args.Host          = 'socsrv' %localhost'     % 'socsrv'        % Host name or IP address
                Args.Port          = 5432 % 63331           % 5432            % Port number
                Args.DatabaseName  = 'lastdb'        % 'last_operational'
                Args.UserName      = 'postgres'      % User name
                Args.Password      = 'PassRoot' %'postgres'      % 'PassRoot'      % Password
            end

            %
            Obj.setName('LastDb');
            
            % Create DbQuery object
            Obj.msgLog(LogLevel.Info, 'Connecting to server %s:%d, database: %s, user: %s/%s', Args.Host, Args.Port, Args.DatabaseName, Args.UserName, Args.Password);
            Obj.Query = db.DbQuery('Host', Args.Host, 'Port', Args.Port, 'UserName', 'postgres', 'Password', Args.Password, 'DatabaseName', Args.DatabaseName);
            
            % Query database version, to verify that we have a connection
            pgver = Obj.Query.getDbVersion();
            Obj.msgLog(LogLevel.Info, 'Connected, Postgres version: %s', pgver);
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


        function Result = createTable_proc_images(Obj)
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
            TN = 'proc_images';
            Q.createTable('TableName', TN, 'AutoPk', 'pk', 'Drop', false);
            Result = Obj.addCommonImageColumns(Q, TN);
        end

        
        function Result = createTable_coadd_images(Obj)
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
            TN = 'coadd_images';
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
            Q.addColumn(TN, 'xxhash',   'varchar(80)', '', 'index', true);
            Q.addColumn(TN, 'ra',       'double', 'default 0', 'index', true);
            Q.addColumn(TN, 'dec',      'double', 'default 0', 'index', true);
            Q.addColumn(TN, 'jd',       'double', 'default 0', 'index', true);
            Q.addColumn(TN, 'mount',    'smallint', 'default 0', 'index', true);
            Q.addColumn(TN, 'camnum',   'smallint', 'default 0', 'index', true);
            Q.addColumn(TN, 'imtype',   'varchar(80)', '', 'index', true);

            % Columns without index
            Q.addColumn(TN, 'bitpix',   'smallint', 'default 0');
            Q.addColumn(TN, 'naxis1',   'smallint', 'default 0');
            Q.addColumn(TN, 'naxis2',   'smallint', 'default 0');
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
            
            % added by @kra:
            
            if strcmp(TN,'proc_images')
                
                Q.addColumn(TN, 'fieldid',      'varchar(80)', "default ''");
                Q.addColumn(TN, 'timezone',     'single', 'default 0');
                Q.addColumn(TN, 'ccdid',        'single', 'default 0');
                Q.addColumn(TN, 'cropid',       'single', 'default 0');
                Q.addColumn(TN, 'level',        'varchar(80)', "default ''");
                Q.addColumn(TN, 'version',      'varchar(80)', "default ''");
                Q.addColumn(TN, 'subdir',       'varchar(80)', "default ''");
                Q.addColumn(TN, 'overscan',     'varchar(80)', "default ''");
                Q.addColumn(TN, 'origgain',     'single', 'default 0');
                Q.addColumn(TN, 'ccdsec',       'varchar(80)', "default ''");
                Q.addColumn(TN, 'origsec',      'varchar(80)', "default ''");
                Q.addColumn(TN, 'origusec',     'varchar(80)', "default ''");
                Q.addColumn(TN, 'uniqsec',      'varchar(80)', "default ''");
                Q.addColumn(TN, 'meanbck',      'double', 'default 0');
                Q.addColumn(TN, 'medbck',       'double', 'default 0');
                Q.addColumn(TN, 'stdbck',       'double', 'default 0');
                Q.addColumn(TN, 'meanvar',      'double', 'default 0');
                Q.addColumn(TN, 'medvar',       'double', 'default 0');
                Q.addColumn(TN, 'ast_nsrc',     'single', 'default 0');
                Q.addColumn(TN, 'ast_arms',     'double', 'default 0');
                Q.addColumn(TN, 'ast_errm',     'double', 'default 0');
                Q.addColumn(TN, 'wcsaxes',      'smallint', 'default 0');
                Q.addColumn(TN, 'radesys',      'varchar(80)', "default ''");
                Q.addColumn(TN, 'lonpole',      'single', 'default 0');
                Q.addColumn(TN, 'latpole',      'single', 'default 0');
                Q.addColumn(TN, 'ctype1',       'varchar(80)', "default ''");
                Q.addColumn(TN, 'ctype2',       'varchar(80)', "default ''");
                Q.addColumn(TN, 'cunit1',       'varchar(80)', "default ''");
                Q.addColumn(TN, 'cunit2',       'varchar(80)', "default ''");
                Q.addColumn(TN, 'crpix1',       'double', 'default 0');
                Q.addColumn(TN, 'crpix2',       'double', 'default 0');
                Q.addColumn(TN, 'crval1',       'double', 'default 0');
                Q.addColumn(TN, 'crval2',       'double', 'default 0');
                Q.addColumn(TN, 'cd1_1',        'double', 'default 0');
                Q.addColumn(TN, 'cd1_2',        'double', 'default 0');
                Q.addColumn(TN, 'cd2_1',        'double', 'default 0');
                Q.addColumn(TN, 'cd2_2',        'double', 'default 0');
                Q.addColumn(TN, 'ra1',          'double', 'default 0');
                Q.addColumn(TN, 'ra2',          'double', 'default 0');
                Q.addColumn(TN, 'ra3',          'double', 'default 0');
                Q.addColumn(TN, 'ra4',          'double', 'default 0');
                Q.addColumn(TN, 'dec1',         'double', 'default 0');
                Q.addColumn(TN, 'dec2',         'double', 'default 0');
                Q.addColumn(TN, 'dec3',         'double', 'default 0');
                Q.addColumn(TN, 'dec4',         'double', 'default 0');
                Q.addColumn(TN, 'rau1',         'double', 'default 0');
                Q.addColumn(TN, 'rau2',         'double', 'default 0');
                Q.addColumn(TN, 'rau3',         'double', 'default 0');
                Q.addColumn(TN, 'rau4',         'double', 'default 0');
                Q.addColumn(TN, 'decu1',        'double', 'default 0');
                Q.addColumn(TN, 'decu2',        'double', 'default 0');
                Q.addColumn(TN, 'decu3',        'double', 'default 0');
                Q.addColumn(TN, 'decu4',        'double', 'default 0');
                Q.addColumn(TN, 'ph_zp',        'double', 'default 0');
                Q.addColumn(TN, 'ph_col1',      'double', 'default 0');
                Q.addColumn(TN, 'ph_medc',      'double', 'default 0');
                Q.addColumn(TN, 'ph_rms',       'double', 'default 0');
                Q.addColumn(TN, 'ph_nsrc',      'single', 'default 0');
                Q.addColumn(TN, 'ph_magsy',     'varchar(80)', "default ''");
                Q.addColumn(TN, 'linmag',       'double', 'default 0');
                Q.addColumn(TN, 'backmag',      'double', 'default 0');
                Q.addColumn(TN, 'fwhm',         'double', 'default 0');
                Q.addColumn(TN, 'med_a',        'double', 'default 0');
                Q.addColumn(TN, 'med_b',        'double', 'default 0');
                Q.addColumn(TN, 'med_th',       'double', 'default 0');
                                
            end
            
            if strcmp(TN,'coadd_images')
                
                Q.addColumn(TN, 'ncoadd',       'single', 'default 0');
                Q.addColumn(TN, 'coaddop',      'varchar(80)', "default ''");
                Q.addColumn(TN, 'avncoadd',     'single', 'default 0');
                Q.addColumn(TN, 'mincoadd',     'smallint', 'default 0');
                Q.addColumn(TN, 'midjd',        'double', 'default 0');
                Q.addColumn(TN, 'minjd',        'double', 'default 0');
                Q.addColumn(TN, 'maxjd',        'double', 'default 0');
                Q.addColumn(TN, 'sublevel',     'varchar(80)', "default ''");
                Q.addColumn(TN, 'gm_ratex',     'double', 'default 0');
                Q.addColumn(TN, 'gm_stdx',      'double', 'default 0');
                Q.addColumn(TN, 'gm_ratey',     'double', 'default 0');
                Q.addColumn(TN, 'gm_stdy',      'double', 'default 0');
                
            end

            Obj.msgLog(LogLevel.Info, 'addCommonImageColumns done');
            Result = true;
        end
        
    end


    methods
        function Result = addRawImage(Obj, FileName, AH, Args)
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
                Args.AddCols = []   % struct
                Args.xxhash = []    % Optional
                Args.Select = false %
            end
                           
            Result = Obj.addImage('raw_images', FileName, AH, 'AddCols', Args.AddCols, 'xxhash', Args.xxhash, 'Select', Args.Select);
        end
        
        
        function Result = addProcImage(Obj, FileName, AH, Args)
            % Insert PROC image columns to table
            % Input :  - LastDb object
            %          - FileName
            %          - AstroHeader
            %          - Optionally additional columns in struct
            %          * Pairs of ...,key,val,...
            %            The following keys are available:
            % Output  : True on success
            % Author  : Chen Tishler (02/2023)
            % Example : 
            arguments
                Obj                 %
                FileName            % Image file name
                AH                  % AstroHeader
                Args.AddCols = []   % struct
                Args.xxhash = []    % Optional
                Args.Select = false %
            end

            Result = Obj.addImage('proc_images', FileName, AH, 'AddCols', Args.AddCols, 'xxhash', Args.xxhash, 'Select', Args.Select);
        end
        
         
        function Result = addCoaddImage(Obj, FileName, AH, Args)
            % Insert PROC image columns to table
            % Input :  - LastDb object
            %          - FileName
            %          - AstroHeader
            %          - Optionally additional columns in struct
            %          * Pairs of ...,key,val,...
            %            The following keys are available:
            % Output  : True on success
            % Author  : Chen Tishler (02/2023)
            % Example : 
            arguments
                Obj                 %
                FileName            % Image file name
                AH                  % AstroHeader
                Args.AddCols = []   % struct
                Args.xxhash = []    % Optional
                Args.Select = false %
            end

            Result = Obj.addImage('coadd_images', FileName, AH, 'AddCols', Args.AddCols, 'xxhash', Args.xxhash, 'Select', Args.Select);
        end
                
        
        function Result = addImage(Obj, TableName, FileName, AH, Args)
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
            % Example : addImage(
            arguments
                Obj                 %
                TableName           % Table name to insert to
                FileName            % Image FITS file name
                AH                  % AstroHeader to insert 
                Args.AddCols = []   % struct - optional additional columns (i.e. AddCols.ColName = ColValue, etc.)
                Args.xxhash = []    % When specified, insert also column 'xxhash' with this value
                Args.Select = false % When true and Xxhash is specified, first check if image already exists
            end

            Q = Obj.Query;

            % Xxhash is speicified
            if ~isempty(Args.xxhash)
                
                Args.Select = true;
                
                % When Select is true, first check if row already exists
                if Args.Select
                    DataSet = Obj.Query.select('*', 'TableName', TableName, 'Where', sprintf('xxhash = ''%s''', Args.xxhash));
                    if numel(DataSet.Data) > 0
                        Result = true;
                        return;
                    end
                end
                
                % Insert it to table
                if isempty(Args.AddCols)
                    Args.AddCols = struct;
                end
                Args.AddCols.xxhash = Args.xxhash;
            end
                        
            % Add FileName to header
            AH.insertKey({'filename', FileName, 'Image file name'}, 'end');
            
            % Add additional columns from struct to AstroHeader
            if ~isempty(Args.AddCols)
                Fields = fieldnames(Args.AddCols);
                for i=1:numel(Fields)
                    Field = Fields{i};
                    Value = Args.AddCols.(Field);
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
        
        %%%%%%%%% functions added by @kra
        
        function [TupleID, Result] = populateImageDB( Obj, Data, Args )
            % Populate a database with metadata (header data) from a list of input images
            % Description: Populate a database with metadata (header data) from a list of input images
            % Input :  - an AstroDb object 
            %          - Data : a cell array containing either 
            %               a) file names of FITS images or b) a name template
            %               b) a vector of AstroImages 
            %               c) a vector of AstroHeaders
            %          * ...,key,val,...
            %          'DBname'        : DB name
            %          'DBtable'       : DB table
            %          'Hash'          : whether to calculate a hashsum of the file and add it to the table
            %          'FileNames'     : an optinal cell array of file names (if
            %          only AstroImages or AstroHeaders are provided)
            % Output : scalar success flag (0 -- images successfully added to the DB)         
            % Tested : Matlab R2020b
            % Author : A. Krassilchtchikov (May 2023)
            % Example: LDB = db.AstroDb(); 
            %          LDB.populateImageDB ( Imfiles, 'DBtype', 'LAST', 'DBtable', 'raw_images', 'Hash', Args.Hash );

            arguments

                Obj
                Data                                % input images (file names or AstroImages) or AstroHeaders
                Args.DBname       = 'LAST';         % DB name
                Args.DBtable      = 'raw_images';   % DB table
                Args.Hash logical = true;           % whether to calculate a hashsum and add it to the table
                Args.FileNames    = {};             % an optional cell array of file names (for the case the first argument is not a file list)

            end

            % determine the number of input images:

            NImg = numel(Data);

            % check whether it is possible to get files for the hash sum

            if numel(Args.FileNames) ~= NImg && ( isa(Data(1), 'AstroImage') ||  isa(Data(1), 'AstroHeader') )
                Args.Hash = false;
            end

            % populate the database

            switch lower(Args.DBname)

                case 'last'

                    for Img = 1:1:NImg

                        if isa( Data(Img), 'AstroImage' )
                            AH = AstroHeader;
                            AH.Data = Data(Img).Header;
                        elseif isa( Data(Img), 'AstroHeader' )
                            AH = Data(Img);
                        else
                            AH = AstroHeader( Data(Img), 1 ); 
                        end
                        
                        if ~isa(AH.File,string)
                            AH.File='';
                        end 

                        if Args.Hash
                            Sum_h64 = tools.checksum.xxhash('FileName', char( Data(Img) ) ); 
                        else
                            Sum_h64 = '';
                        end

                        % populate the DB

                        switch lower(Args.DBtable)          

                            case 'raw_images'

                                Obj.addRawImage(AH.File, AH, 'xxhash', Sum_h64);

                            case 'proc_images'

                                Obj.addProcImage(AH.File, AH, 'xxhash', Sum_h64);

                            case 'coadd_images'

                                Obj.addCoaddImage(AH.File, AH, 'xxhash', Sum_h64);

                            otherwise

                                error('The requested table does not exist yet, exiting..');

                        end

                    end

                otherwise

                    error('The requested DB does not exist, exiting..');

            end

            %

            cprintf('hyper','The requested DB successfully populated with image metadata.\n');
            
 %          Obj.Query.deleteRecord('TableName', 'raw_images', 'Where', 'filename like ''%143%''')          

%             Result = Obj.Query.select('pk', 'TableName',lower(Args.DBtable),'Where', 'ra > 179','OutType','Table');
            Result = Obj.Query.select('pk', 'TableName',lower(Args.DBtable),'Where', 'filename like "%UU%"','OutType','Table');

            % ask Chen to implement the "just added" criterion for the query
            if ~Result
                TupleID = 0;              
            else
                TupleID = Result.pk;
            end

        end

        %%%%%%%%%
        
        function [Result] = addImages2DB( Obj, Args )
        % Add images from a directory to a database
        % Description: Add images from a directory to a database
        % Input:   - 
        %          * ...,key,val,...
        %          'DataDir'       : the root directory of a tree to search images within
        %          'InputImages'   : the mask of the input image filenames
        %          'DBname'        : DB name
        %          'DBtable'       : DB table
        %          'Hash'          : whether to calculate a hashsum of the file and add it to the table
        % Output : - scalar success flag (0 -- images successfully added to the DB)
        % Tested : Matlab R2020b
        % Author : A. Krassilchtchikov et al. (May 2023)
        % Examples: A = db.AstroDb; 
        %           A.addImages2DB(A,'DataDir','/home/sasha/Raw2/','DBname','LAST','DBtable','raw_images');
        %           A.addImages2DB(A,'DataDir','/home/sasha/Obs2/','InputImages','LAST*sci*proc_Image*.fits','DBtable','proc_images')
        %           A.addImages2DB(A,'DataDir','/home/sasha/Obs2/','InputImages','LAST*sci*coadd_Image*.fits','DBtable','coadd_images')
    
        arguments

            Obj
            Args.DataDir        =    '/home/sasha/Raw/';                % The directory containing the input images
            Args.InputImages    =    'LAST*sci*raw_Image*.fits';         % The mask of the input image filenames
            Args.DBname         =    'LAST';
            Args.DBtable        =    'RAW';
            Args.Hash  logical  =    true;

        end

        % get a list of input files according to the input mask 

        ImageFiles  = dir ( fullfile(Args.DataDir,'**',Args.InputImages) );

        ImNum = numel(ImageFiles);

        Imfiles = repmat({''}, ImNum, 1);
        Images  = repmat(AstroImage(), ImNum, 1);
        Headers = repmat(AstroHeader(), ImNum, 1);

        for Img = 1:1:ImNum

            Imfiles{Img} = fullfile(ImageFiles(Img).folder, ImageFiles(Img).name);
            Images(Img) = AstroImage(Imfiles(Img));
            Headers(Img).Data = Images(Img).Header;

        end

        % call the sub to populate the database (3 variants)

        TupleID = Obj.populateImageDB ( Obj, Imfiles, 'DBname', Args.DBname, 'DBtable', Args.DBtable, 'Hash', Args.Hash );
        TupleID = Obj.populateImageDB ( Obj, Headers, 'DBname', Args.DBname, 'DBtable', Args.DBtable, 'Hash', Args.Hash );
        TupleID = Obj.populateImageDB ( Obj, Images,  'DBname', Args.DBname, 'DBtable', Args.DBtable, 'Hash', Args.Hash );

        fprintf('%s%d','Inserted tuples:',TupleID);

        % assign the Result value

        Result = 0;   % success

        end

        %%%%%%%%%

        

        %%%%%%%%% end functions added by @kra
         
    end
    
    
    methods(Static)
        Result = unitTest()
            % LastDb Unit-Test
    end

end
