% db.AstroDB is a class for astronomical relational DB
%
% Properties :
%         Query       - DbQuery object (filled when a DB object is created)
%         Tables      - the list of existing DB tables (filled when a DB object is created)
%         Tname       - current table name (overriden by a user input to the Obj.insert function)
%         Telescope   - Telescope name
%
% Functionality :
%
% AstroDb -
% createImageTable - Create or update definitions of image tables
% createcatalogTable - Create or update definitions of catalog tables
% createLASTtables - Create or update definitions of LAST database tables
% addCommonImageColumns - Add/update common image columns to table
% addCommonCatalogColumns - Add/update common catalog columns to table
% addImage - Insert AstroHeader data to the specified table
% addCatalog - Insert AstroCatalog records to src_catalog table
% insert - insert image header or catalog data into an AstroDb database
%          (the data injection function to be called by the user)
% updateByTupleID - update DB table column values for the specified tuple numbers
%          (the data manipulation function to be called by the user)
% 
% TODO:
% make a dependent LASTDb class with more particular table and connection parameters
%
% Examples:
% % create a DB object with the default parameters:
% A = db.AstroDb; 
% % create a DB object with some more user-defined parameters:
% A = db.AstroDb('Host', '10.23.1.1', 'DatabaseName', 'last_operational', 'UserName', 'myuser', 'Password', 'mypwd', 'Port', 5432);
% % view the list of existing tables:
% A.Tables
% % re-create the raw_images table (existing data will be lost):
% A.createImageTable('raw_images','Drop',1);
% % Add to the DB metadata from RAW level images contained in the /home/sasha/Raw2/ directory according to the internal template:
% TupleIDs = A.insert('LAST*raw*Ima*fits','DataDir','/home/sasha/Raw2/','Table','raw_images');
% % Add to the DB metadata from RAW level images contained in a vector of AstroImages (AI):
% TupleIDs = A.insert(AI,'Table','raw_images');
% % Add to the DB metadata from PROC level images contained in the /home/sasha/Obs2/ directory according to the template:
% TupleIDs = A.insert('LAST*proc*Ima*fits','DataDir','/home/sasha/Obs2/','Table','proc_images');
% % Same, but do not insert the records which already exist in the table:
% TupleIDs = A.insert('LAST*proc*Ima*fits','DataDir','/home/sasha/Obs2/','Table','proc_images','Force',0);
% % Add to the DB metadata from COADD level images contained in the /home/sasha/Obs2/ directory according to the template:
% TupleIDs = A.insert('LAST*coadd*Ima*fits','DataDir','/home/sasha/Obs2/','Table','coadd_images');
% % Add to the DB metadata from COADD level images contained in a vector of AstroHeaders (AH):
% TupleIDs = A.insert(AH,'Table','coadd_images'); 
% % Same, but also put out the processed file names:
% TupleIDs = A.insert('LAST*coadd*Ima*fits','DataDir','/home/sasha/Obs2/','Table','coadd_images','Verbose',1);
% % Add to the DB source data from COADD level catalogs in the /home/sasha/Obs2/ directory according to the template:
% TupleIDs = A.insert('LAST*coadd*Cat*fits','DataDir','/home/sasha/Obs2/','Type','cat','Table','src_catalog');
% % Add to the DB source data from COADD level catalogs contained in a vector of AstroCatalogs (AC):
% TupleIDs = A.insert(AC,'Type','cat','Table','src_catalog');
% % Change 'ra' to 218 in tuples with ids from 2 to 10 in the 'proc_images' table
% A.updateByTupleID('proc_images',[2:10],'ra',218)
% % Change 'procstat' to 'seeing 0.5' for tuples listed in the vector Tuples)
% A.updateByTupleID('raw_images',Tuples,'procstat','seeing 0.5')    
%
classdef AstroDb < Component

    properties (SetAccess = public)
        Query       = []      % DbQuery object (filled when a DB object is created)
        Tables      = []      % the list of existing DB tables (filled when a DB object is created)
        Tname       = 'raw_images'; % current table name (overriden by a user input to the Obj.insert function)
        Telescope   = 'LAST'  % Telescope name
        ConnectionEstablished = [] % will be set to true or false
        
%       These will be later transfered to the LastDb class:
%         TnRawImages     = 'raw_images';
%         TnProcImages    = 'proc_images';
%         TnCoaddImages   = 'coadd_images';       
%         TnSrcCatalog    = 'src_catalog';
    end

    methods % construction of an AstroDb object 
        
        function Obj = AstroDb(Args)
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
            % Author   : Chen Tishler (02/2023)
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
            
            PM = PasswordsManager;
            try
                Args.UserName = PM.search(Args.DatabaseName).User;
                Args.Password = PM.search(Args.DatabaseName).Pass;
            catch
                Obj.msgLog(LogLevel.Info, 'DB password file is not found');
                Obj.ConnectionEstablished = false;
                return
            end
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
            try
                pgver = Obj.Query.getDbVersion();
            catch
                Obj.msgLog(LogLevel.Info, 'Connection failed');
                Obj.ConnectionEstablished = false;
                return
            end
            Obj.ConnectionEstablished = true;
            Obj.msgLog(LogLevel.Info, 'Connected, Postgres version: %s', pgver);
            assert(contains(pgver, 'PostgreSQL'));   
            
            % List the existing tables            
            TableList = Obj.Query.select('*','TableName','pg_tables','Where','schemaname = ''public''');
            Obj.Tables = extractfield(TableList.Data,'tablename');
            Obj.msgLog(LogLevel.Info, 'The DB contains the following public tables:');
            FMT = repmat('%s ', 1, numel(Obj.Tables)); FMT = [FMT '\n'];
            Obj.msgLog(LogLevel.Info, FMT, Obj.Tables{1:numel(Obj.Tables)});                
            
        end
        
    end
    
    methods % LAST-specfic structure of image and catalog tables
        
       function Result = createLASTtables(Obj)
            % Create or update definitions of LAST database tables
            % Input :  - LastDb object
            %          * Pairs of ...,key,val,...
            %            The following keys are available:
            % Output  : True on success
            % Author  : Chen Tishler (02/2023)
            % Example : createLASTtables()

            Obj.createImageTable('raw_images','Drop',false);
            Obj.createImageTable('proc_images','Drop',false);
            Obj.createImageTable('coadd_images','Drop',false);
            Obj.createCatalogTable('proc_src_catalog','Drop',false);
            Obj.createCatalogTable('coadd_src_catalog','Drop',false);
           
            Result = true;
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
%             Q.addColumn(TN, 'm_jha',    'double', 'default 0');
%             Q.addColumn(TN, 'ha',       'double', 'default 0');
            Q.addColumn(TN, 'm_ara',    'double', 'default 0');
            Q.addColumn(TN, 'm_aha',    'double', 'default 0');
            Q.addColumn(TN, 'm_adec',   'double', 'default 0');
            Q.addColumn(TN, 'm_adra',   'double', 'default 0');
            Q.addColumn(TN, 'm_adha',   'double', 'default 0');
            Q.addColumn(TN, 'm_addec',  'double', 'default 0');                                    
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
            Q.addColumn(TN, 'procversion', 'smallint', 'default 0'); 
            
            % added by @kra:           
            if strcmp(TN, 'proc_images') || strcmp(TN, 'coadd_images')
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
                Q.addColumn(TN, 'ra1',          'double', 'default 0', 'index', true);
                Q.addColumn(TN, 'ra2',          'double', 'default 0', 'index', true);
                Q.addColumn(TN, 'ra3',          'double', 'default 0', 'index', true);
                Q.addColumn(TN, 'ra4',          'double', 'default 0', 'index', true);
                Q.addColumn(TN, 'dec1',         'double', 'default 0', 'index', true);
                Q.addColumn(TN, 'dec2',         'double', 'default 0', 'index', true);
                Q.addColumn(TN, 'dec3',         'double', 'default 0', 'index', true);
                Q.addColumn(TN, 'dec4',         'double', 'default 0', 'index', true);
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

                Q.addColumn(TN, 'pipever',  'varchar(80)', "default ''");  
                Q.addColumn(TN, 'raw_image_id',  'integer', 'default 0');  
                
            end
            
            if strcmp(TN, 'coadd_images')
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

       function Result = addCommonCatalogColumns(Obj, Q, TN)
            % Add/update common catalog columns to table
            % Input :  - LastDb object
            %          - Q - DbQuery object (should be Obj.Query)
            %          - TN - Table name
            %          * Pairs of ...,key,val,...
            %            The following keys are available:
            % Output  : True on success
            % Author  : Chen Tishler (02/2023)
            % Example : 
            arguments
                Obj                 %
                Q                   %
                TN                  %
            end

            Obj.msgLog(LogLevel.Info, 'addCommonImageColumns started');

            % Columns WITH index
            Q.addColumn(TN, 'filename', 'varchar(256)', '', 'index', true);
            Q.addColumn(TN, 'xxhash',   'varchar(80)', '', 'index', true);
%             Q.addColumn(TN, 'cattype',   'varchar(80)', '', 'index', true);
            Q.addColumn(TN, 'mount',    'smallint', 'default 0', 'index', true);
            Q.addColumn(TN, 'camnum',   'smallint', 'default 0', 'index', true);
            Q.addColumn(TN, 'node',     'smallint', 'default 0', 'index', true);

            % Columns WITHOUT index
            Q.addColumn(TN, 'xpeak',        'smallint', 'default 0');
            Q.addColumn(TN, 'ypeak',        'smallint', 'default 0');
            Q.addColumn(TN, 'x1',           'single', 'default 0');
            Q.addColumn(TN, 'y1',           'single', 'default 0');
            Q.addColumn(TN, 'x2',           'single', 'default 0');
            Q.addColumn(TN, 'y2',           'single', 'default 0');
            Q.addColumn(TN, 'xy',           'single', 'default 0');
            
            Q.addColumn(TN, 'sn_1',         'single', 'default 0');
            Q.addColumn(TN, 'sn_2',         'single', 'default 0');
            Q.addColumn(TN, 'sn_3',         'single', 'default 0');
            Q.addColumn(TN, 'sn_4',         'single', 'default 0');
            Q.addColumn(TN, 'sn_5',         'single', 'default 0');
            Q.addColumn(TN, 'back_im',      'single', 'default 0');
            Q.addColumn(TN, 'var_im',       'single', 'default 0');
            Q.addColumn(TN, 'back_annulus', 'single', 'default 0');
            Q.addColumn(TN, 'std_annulus',  'single', 'default 0');
            Q.addColumn(TN, 'flux_aper_1',  'double', 'default 0');
            Q.addColumn(TN, 'flux_aper_2',  'double', 'default 0');
            Q.addColumn(TN, 'flux_aper_3',  'double', 'default 0');
            Q.addColumn(TN, 'fluxerr_aper_1','double', 'default 0');
            Q.addColumn(TN, 'fluxerr_aper_2','double', 'default 0');
            Q.addColumn(TN, 'fluxerr_aper_3','double', 'default 0');
            Q.addColumn(TN, 'mag_aper_1',    'double', 'default 0');
            Q.addColumn(TN, 'mag_aper_2',    'double', 'default 0');
            Q.addColumn(TN, 'mag_aper_3',    'double', 'default 0');
            Q.addColumn(TN, 'magerr_aper_1', 'double', 'default 0');
            Q.addColumn(TN, 'magerr_aper_2', 'double', 'default 0');
            Q.addColumn(TN, 'magerr_aper_3', 'double', 'default 0');
            Q.addColumn(TN, 'flux_conv_1',  'double', 'default 0');
            Q.addColumn(TN, 'flux_conv_2',  'double', 'default 0');
            Q.addColumn(TN, 'flux_conv_3',  'double', 'default 0');
            Q.addColumn(TN, 'flux_conv_4',  'double', 'default 0');
            Q.addColumn(TN, 'flux_conv_5',  'double', 'default 0');
            Q.addColumn(TN, 'mag_conv_1',   'double', 'default 0');
            Q.addColumn(TN, 'mag_conv_2',   'double', 'default 0');
            Q.addColumn(TN, 'mag_conv_3',   'double', 'default 0');
            Q.addColumn(TN, 'mag_conv_4',   'double', 'default 0');
            Q.addColumn(TN, 'mag_conv_5',   'double', 'default 0');
            Q.addColumn(TN, 'magerr_conv_1','double', 'default 0');
            Q.addColumn(TN, 'magerr_conv_2','double', 'default 0');
            Q.addColumn(TN, 'magerr_conv_3','double', 'default 0');
            Q.addColumn(TN, 'magerr_conv_4','double', 'default 0');
            Q.addColumn(TN, 'magerr_conv_5','double', 'default 0');
            Q.addColumn(TN, 'flags',        'integer', 'default 0');
            Q.addColumn(TN, 'x',            'single', 'default 0');
            Q.addColumn(TN, 'y',            'single', 'default 0');
            Q.addColumn(TN, 'flux_psf',     'double', 'default 0');
            Q.addColumn(TN, 'mag_psf',      'double', 'default 0', 'index',true);
            Q.addColumn(TN, 'magerr_psf',   'double', 'default 0');
            Q.addColumn(TN, 'psf_chi2dof',  'single', 'default 0');
            Q.addColumn(TN, 'sn',           'double', 'default 0', 'index', true);
            Q.addColumn(TN, 'ra',           'double', 'default 0', 'index', true);
            Q.addColumn(TN, 'dec',          'double', 'default 0', 'index', true);
            Q.addColumn(TN, 'mergedcatmask','integer', 'default 0');
%             Q.addColumn(TN, 'nobs',         'smallint', 'default 0'); 
%                                              smallint is incompatible with NaN values!
            Q.addColumn(TN, 'nobs',         'single', 'default 0');
            Q.addColumn(TN, 'jd',           'double', 'default 0', 'index', true);
            Q.addColumn(TN, 'exptime',      'single', 'default 0');
                        
            % Additional
            Q.addColumn(TN, 'procstat',    'varchar(256)', "default ''", 'Comment', 'Additional user data');    
            Q.addColumn(TN, 'pipever', 'varchar(80)', "default ''");  
                
            Obj.msgLog(LogLevel.Info, 'addCommonCatalogColumns done');
            Result = true;
        end
        
    end

    methods % low level addImage and addCatalog functions             
               
        function Result = addImage(Obj, TableName, FileName, AH, Args)
            % Insert AstroHeader to specified table
            % NB: if Args.Force = false and we are trying to insert the same record twice, Result will be negative  
            % Input :  - LastDb object
            %          - TableName
            %          - FileName
            %          - AstroHeader
            %          - struct - Optionally additional columns. MUST BE lowercase!
            %          * Pairs of ...,key,val,...
            %            The following keys are available:
            % Output  : Pk of inserted row on success, [] on failure
            % Author  : Chen Tishler (02/2023)
            % Example : 
            arguments
                Obj                 %
                TableName           % Table name to insert to
                FileName            % Image FITS file name
                AH                  % AstroHeader to insert 
                Args.AddCols = []   % struct - optional additional columns (i.e. AddCols.ColName = ColValue, etc.)
                Args.xxhash = []    % When specified, insert also column 'xxhash' with this value
                Args.Select = false % When true and Xxhash is specified, first check if image already exists
                Args.Force  = true  % Whether to insert a record with the same hash sum, or create next version of the image

                Args.ReplaceKeyVal = {'AIRMASS','',0; 'PRVFOCUS','',0; 'FOCUS','',0; ...
                                        'M_JRA','',0;   'M_JDEC','',0; 'M_JHA','',0; ...
                                           'RA','',0;      'DEC','',0;    'HA','',0; ...
                                           'AZ','',0;      'ALT','',0};
            end

            Q = Obj.Query;
            
            ProcVers = 0;

            % Xxhash is speicified
            if ~isempty(Args.xxhash)               
                Args.Select = true;
                
                % When Select is true, first check if the record already exists
                if Args.Select  
                    DataSet = Obj.Query.select('*', 'TableName', TableName, 'Where', sprintf('xxhash = ''%s''', Args.xxhash));
                    if numel(DataSet.Data) > 0  % there is already a record with the same hash
                        if ~Args.Force          % do not insert the same record
                            Result = -DataSet.Data(end).pk;
                            return;
                        else
                            ProcVers = DataSet.Data(end).procversion + 1;
                        end
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
            
            % Check for old-type AH values and correct them:
            NrepKey = size(Args.ReplaceKeyVal,1);
            for IrepKey=1:1:NrepKey
                if AH.isKeyVal(Args.ReplaceKeyVal{IrepKey,1}, Args.ReplaceKeyVal{IrepKey,2})
                    AH.replaceVal(Args.ReplaceKeyVal{IrepKey,1}, Args.ReplaceKeyVal{IrepKey,3});
                end
            end
            
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
            Pk = Q.insert(AH, 'TableName', TableName, 'ColumnsOnly', true, 'Returning', 'pk');
            
            % The same record should be inserted with the advancing procversion number
            if ProcVers > 0 
                Obj.updateByTupleID(Pk, 'procversion', ProcVers, 'Table', TableName);
            end
            Result = Pk;
        end
        
        function Result = addCatalog(Obj, TableName, AC, Args)
            % Insert source catalog records to src_catalog table
            % Input :  - LastDb object
            %          - FileName
            %          - AstroCatalog
            %          - Optionally additional columns in struct
            %          * Pairs of ...,key,val,...
            %            The following keys are available:
            % Output  : True on success
            % Author  : Chen Tishler (02/2023)
            % Example : 
            arguments
                Obj                         %
                TableName                   %
                AC                          % AstroCatalog
                Args.FileName = []          % Not implemented yet - Allow reading the catalog from file
            end

            % Insert AstroCatalog to the table
            Pk = Obj.Query.insert(AC, 'TableName', TableName, 'ColumnsOnly', true, 'Returning', ''); % 'pk' - temporary switch off the Returning ke
            Result = Pk;
        end
                
    end

    methods % user interface functions
        
        function [TupleID, Result] = insert(Obj, Data, Args)
            % insert image header or catalog data into an AstroDb database
            % Input : - Data: 
            %               a) a cell array of FITS file names or a FITS file name template
            %               b) a vector of AstroImages 
            %               c) a vector of AstroHeaders
            %               d) a vector of AstroCatalogs
            %         * ...,key,val,...
            %         'Table'  : table name            
            %         'Type'   : 'img', 'cat', 'bulkima', 'bulkcat'
            %         'DataDir': if not empty, 'Data' should contain a filename template
            %                    e.g., 'LAST*sci*raw_Image*.fits'
            %         'Hash'   : insert a hash sum for each entry
            %         'Force'  : insert a record from the file if the same
            %                    record from the same file (same hash sum) has been inserted already
            %         'Verbose': print names of the digested files
            % Example: A = db.AstroDb;
            %          pk = A.insert(AI,'Table','raw_images');
            % Author : A. Krassilchtchikov (Jun 2023)
            arguments
                Obj
                Data                                % input images (file names or AstroImages) or AstroHeaders
                Args.Table        = '';             % table name (by def. take from the Object property)
                Args.Type         = 'img';          % data type: 'img', 'cat', 'bulkima' or 'bulkcat'
                Args.DataDir      = '';             % if not empty, 'Data' contains a filename template, 
                                                    % e.g., 'LAST*sci*raw_Image*.fits'
                Args.Hash logical = true;           % employ hash sums to check if the record is new
                Args.Force logical= true;           % if the record is inserted despite the existing copy (checked by the hashsum)
                Args.FileNames    = {};             % an optional cell array of file names (for the case the first argument is not a file list)
                Args.Verbose logical = false;       % print filenames, whose headers are inserted 
                                                    % bulk injection of LAST catalogs:
                Args.BulkFN          = [];          % FileNames
                Args.BulkCatType     = [];          % catalog type: 'proc' or 'coadd'                
                Args.BulkAI          = [];          % one of the catalog-containing AI's (for keyword extraction) 
            end
            % choose, which table to manipulate and check if it exists in the database
            if ~isempty(Args.Table)
                Table = Args.Table;
            else
                Table = Obj.Tname;
            end
            % 
            if ~ismember(Table, Obj.Tables)
                ErrorMsg = sprintf('The requested table %s does not exist in the database',Table);     
                Obj.msgLog(LogLevel.Error, ErrorMsg);
                return
            end 
            if isempty(Data)
                ErrorMsg = sprintf('db.AstroDb.insert: the input structure is empty, skipping..');     
                Obj.msgLog(LogLevel.Error, ErrorMsg);
                return
            end
            % check basic input consistency:
            if ( isa(Data(1), 'AstroImage') ||  isa(Data(1), 'AstroHeader') ) && strcmp(Args.Type,'cat') ...
               || ( isa(Data(1), 'AstroCatalog') && strcmp(Args.Type,'img') )
                error ('Actual data type does not match the Type parameter');
            end
            % check if the input is a DataDir + filename template 
            % and if so, make a cell array of data files:
            if ~isempty(Args.DataDir)   
                Files  = dir ( fullfile(Args.DataDir, '**', Data) );
                NData  = numel(Files);
                Data   = repmat({''}, NData, 1);
                for IData = 1:1:NData
                    Data{IData} = fullfile(Files(IData).folder, Files(IData).name);
                end
            end
            % determine the number of input files, images or catalogs:
            NData = numel(Data);  
            TupleID  = zeros(NData,1);
            % check whether it is possible to get files for the hash sum
            if numel(Args.FileNames) ~= NData && ...
                    ( isa(Data(1), 'AstroImage') ||  isa(Data(1), 'AstroHeader') || isa(Data(1), 'AstroCatalog') )
                Args.Hash = false;
            end
            
            switch lower(Args.Type)                
                case 'bulkcat' % bulk writing of PROC and COADD catalogs to a CSV file for further injection 
                % NB! this case is very LAST-specific!                    
                    FN = Args.BulkFN.copy;
                    FN = FN.updateIfNotEmpty('Product','Cat', 'FileType',{'csv'});
                    FN.CropID = FN.CropID(FN.validTimes);
                    FN.Time   = FN.Time(FN.validTimes);
                    if strcmpi(Args.BulkCatType,'proc')
                        CatFileName = FN.genFull{1};
                    elseif strcmpi(Args.BulkCatType,'coadd')
                        CatFileName  = FN.genFull('LevelPath','proc');
                        CatFileName  = CatFileName{1};
                    else
                        error('Incorrect catalog type in AstroDb.insert');
                    end
                    CatFileName = tools.os.relPath2absPath(CatFileName);
                    StKey = Args.BulkAI.getStructKey({'CAMNUM','MOUNTNUM','NODENUMB','JD','EXPTIME'});
                    Data.writeLargeCSV(CatFileName,...
                        'AddColNames',[{'CAMNUM'} {'MOUNT'} {'NODE'} {'JD'} {'EXPTIME'}],...
                        'AddColValues',[StKey.CAMNUM, StKey.MOUNTNUM, StKey.NODENUMB, StKey.JD, StKey.EXPTIME] );
                    
                case 'bulkima' % bulk writing of RAW, PROC, and COADD image headers to a CSV file for further injection
                % NB! this case is very LAST-specific!
                    FN = Args.BulkFN.copy;
                    FN = FN.updateIfNotEmpty('FileType',{'csv'});
                    if strcmpi(Args.BulkCatType,'raw')
                        HeaderFN = FN.genFull{1};
                        Data.writeCSV(HeaderFN,'CleanHeaderValues',1);                        
                    elseif strcmpi(Args.BulkCatType,'proc')
                        HeaderFN = FN.genFull{1};
                        AH = [Data.HeaderData];
                        AH.writeCSV(HeaderFN,'CleanHeaderValues',1);
                    elseif strcmpi(Args.BulkCatType,'coadd')
                        HeaderFN = FN.genFull('LevelPath','proc'); HeaderFN = HeaderFN{1};
                        AH = [Data.HeaderData];
                        AH.writeCSV(HeaderFN,'CleanHeaderValues',1);
                    else
                        error('Incorrect image type in AstroDb.insert');
                    end                    
                    
                otherwise  % record-by-record injection into the DB
                    
                    for IData = 1:1:NData                        
                        try                            
                            switch lower(Args.Type)
                                
                                case 'cat'  % catalogs
                                    
                                    if isa( Data(IData), 'AstroCatalog' )
                                        AC = Data(IData);
                                        if numel(Args.FileNames) == NData % a separate list of file names is provided
                                            Filename = Args.FileNames{IData};
                                            if Args.Verbose
                                                fprintf('%s\n', Filename );
                                            end
                                        else
                                            Filename = '';
                                        end
                                    else
                                        if Args.Verbose
                                            fprintf('%s\n', char( Data(IData) ) );
                                        end
                                        AC = AstroCatalog( Data(IData) ); % get AC from a file
                                        Filename =   char( Data(IData) );
                                    end
                                    
                                    % in fact, there will be many tuples for each of
                                    % the catalogs, so one does not make much sense
                                    % (TBD)
                                    TupleID(IData) = Obj.addCatalog(Table, AC, 'FileName', Filename);
                                    
                                case 'img'  % images
                                    
                                    if isa( Data(IData), 'AstroHeader' )
                                        AH = Data(IData).copy; % .copy is used in order not to influence the original AstroHeader
                                        if numel(Args.FileNames) == NData % a separate list of file names is provided
                                            Filename = Args.FileNames{IData};
                                        elseif ~isempty(AH.File)
                                            Filename = AH.File;
                                        else
                                            Filename = '';
                                        end
                                    elseif isa( Data(IData), 'AstroImage' )
                                        AH = AstroHeader;
                                        AH.Data = Data(IData).Header;
                                        if numel(Args.FileNames) == NData % a separate list of file names is provided
                                            Filename = Args.FileNames{IData};
                                        elseif ~isempty(AH.File)
                                            Filename = AH.File;
                                        else
                                            Filename = '';
                                        end
                                    else
                                        if Args.Verbose
                                            fprintf('%s\n', char( Data(IData) ) );
                                        end
                                        AH = AstroHeader( Data(IData), 1 );
                                        Filename = char( Data(IData) );
                                    end
                                    
                                    if Args.Hash && ~isempty(Filename)
                                        Filename = tools.os.relPath2absPath(Filename);
                                        Sum_h64  = tools.checksum.xxhash('FileName', Filename );
                                    else
                                        Sum_h64 = '';
                                    end
                                    
                                    TupleID(IData) = Obj.addImage(Table, Filename, AH, 'xxhash', Sum_h64, 'Force', Args.Force);                                    
                                otherwise                                    
                                    error('Illegal data type');                                    
                            end
                            
                        catch ME                            
                            ErrorMsg = sprintf('db.AstroDB.insert error at loop iteration %d: %s / funname: %s @ line: %d ', ...
                                IData, ME.message, ME.stack(1).name, ME.stack(1).line);
                            Obj.msgLog(LogLevel.Error, ErrorMsg);
                            io.msgLogEx(LogLevel.Error, ME, 'db.AstroDB exception at loop iteration %d', IData);                            
                        end                        
                    end                    
                    Obj.msgLog(LogLevel.Info, 'Processed: %d entries', NData);  
                    Obj.msgLog(LogLevel.Info, 'Table %s successfully populated with %s metadata', Table, Args.Type');
            end
            
            Result = 1;            
        end
        
        function Result = updateByTupleID(Obj, TupleID, Colname, Colval, Args)
            % Update DB table column values for the specified tuple numbers 
            % Input :  - Obj    : the database object
            %          - TupleID: a vector of tuple IDs
            %          - Colname: name of the column to be changed 
            %          - Colval : the new column value
            %          * ...,key,val,...
            %          'Table'  : table name (by default = Obj.Tname)
            %
            % Output : - success flag (1 -- images successfully changed the values in the DB)
            % Tested : Matlab R2020b
            % Author : A. Krassilchtchikov (May 2023)
            % Examples: A = db.AstroDb; 
            %           A.updateByTupleID([2:10],'ra',218,'Table','proc_images')
            %           (change 'ra' to 218 in tuples with ids from 2 to 10 in the 'proc_images' table)
            %           A.updateByTupleID(Tuples,'procstat','seeing 0.5','Table','raw_images')
            %           (change 'procstat' to 'seeing 0.5' for tuples listed in the vector Tuples)
            arguments
                Obj
                TupleID            % a vector of unique tuple ids
                Colname            % name of column to change
                Colval             % column value to insert for the given tuple ids
                Args.Table  = '';  % table name (by def. take from the Object property)
            end
            
            % choose, which table to manipulate and check if it exists in the database
            if ~isempty(Args.Table)
                Table = Args.Table;
            else
                Table = Obj.Tname;
            end
            
            if ~ismember(Table, Obj.Tables)
                error('The requested table does not exist in the database')
            end
            
            NTup = numel(TupleID);
            Val  = zeros(NTup,1);

            % update the values
            for ITup = 1:1:numel(TupleID)
                
                Cond = strcat('pk =', int2str( TupleID(ITup) ));
                
                if numel(Colval) < 2
                    Val(ITup) = Colval;
                else
                    Val(ITup) = Colval(ITup);
                end
                
                if isnumeric( Val(ITup) )
                    Action = strcat(Colname, ' = ', num2str( Val(ITup) ));
                else
                    Action = strcat(Colname, ' = ''', Val(ITup), '''');
                end

                Obj.Query.update(Action, 'TableName', Table, 'Where', Cond);
            end
            
            Result = true;
            
        end

        function Result = coneSearch(Obj, Table, RA0, Dec0, Dist, Args)
            % a cone search query in a specified catalog or image table
            % Input : - Obj   : the LastDB object
            %         - Table : the table name
            %         - RA0   : search RA center (degrees, J2000)
            %         - Dec0  : search Dec center (degrees, J2000)
            %         - Dist  : search radius (def. arcsec)
            %         * Pairs of ...,key,val,...
            %         'Columns'  : columns to put out
            %         'DistUnit' : unit of distance (def. arcsec)
            %         'AND'      : additional search condition
            % Output: - a Db.Query object (results in .Data property)
            % Example: A = db.AstroDb();
            %          Res = A.coneSearch('coadd_images',34.5,-4.3,600);
            %          Res = A.coneSearch('src_catalog',220,51,5,'DistUnit','arcmin','AND','sn>10','Columns','*');
            % Author : A. Krassilchtchikov (Jun 2023)
            arguments
                Obj
                Table 
                RA0
                Dec0
                Dist
                Args.DistUnit = 'arcsec'; 
                Args.Columns  = 'ra,dec';
                Args.AND      = ''
            end

            RAD = 180/pi;

            switch Args.DistUnit

                case 'arcsec'
                    Dist = Dist / 3600;
                case 'arcmin'
                    Dist = Dist / 60;
                case 'deg'
                    % do nothing
                otherwise
                    error('Incorrect distance unit');
            end  
            
            % β = arccos[sin(δ1)*sin(δ2)+ cos(δ1)*cos(δ2)*cos(α1 - α2)] < Dist 
            Cond = sprintf('%s%d%s%d%s%s%d%s%d%s%s%d%s%d%s%d', ...
                'acos( sin(dec/',RAD,') * (',sin(Dec0/RAD),')', ...
                   '+ cos(dec/',RAD,') * (',cos(Dec0/RAD),')', ...
                   '* cos(ra/',RAD,'-',RA0/RAD,') ) <', Dist/RAD);
            if ~isempty(Args.AND)
                Cond = strcat('(',Cond,') AND (',Args.AND,')');
            end

            Result = Obj.Query.select(Args.Columns,'TableName',Table,'Where',Cond);
        end
         
    end 
    
    methods % creation/removal of image and catalog tables (generic)
        
        function Result = createImageTable(Obj, TableName, Args)
            % Create or update definitions of image tables
            % Input :  - LastDb object
            %          * Pairs of ...,key,val,...
            %            The following keys are available:
            % Output  : True on success
            % Author  : Chen Tishler (02/2023)
            % Example : 
            arguments
                Obj
                TableName
                Args.AddCommonColumns = true
                Args.Drop = false
            end

            % Create table
            Obj.Query.createTable('TableName', TableName, 'AutoPk', 'pk', 'Drop', Args.Drop);
            if Args.AddCommonColumns                
                Result = Obj.addCommonImageColumns(Obj.Query, TableName);
            end
        end

        function Result = createCatalogTable(Obj, TableName, Args)
            % Create or update definitions of of catalog tables
            % Input :  - LastDb object
            %          * Pairs of ...,key,val,...
            %            The following keys are available:
            % Output  : True on success
            % Author  : Chen Tishler (02/2023)
            % Example : 
            arguments
                Obj
                TableName
                Args.AddCommonColumns = true
                Args.Drop = false                
            end

            % Create table
            Obj.Query.createTable('TableName', TableName, 'AutoPk', 'pk', 'Drop', Args.Drop);
            if Args.AddCommonColumns
                Result = Obj.addCommonCatalogColumns(Obj.Query, TableName);
            end
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
