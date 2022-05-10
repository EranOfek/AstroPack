
function Result = examples()
    % DbAdmin examples (see also DbAdmin.unitTest)
    %
    % Requirements:
    %
    % 1. Install Postgres v14 - See detailed instructions in files:
    %
    %       postgres_installation_ubuntu18.md
    %       postgres_shared_folder.md
    %
    % 2. Install pgAdmin4 (version 6.3) as administration tool    
    %    On Windows, use SQL Manager Lite for PostgreSQL by EMS Software
    %    On Linux, use DataGrip by JetBrains (License is required)
    %
    % 3. You need to have configuration file with database user and password, as:
    %
    %       config/local/Database.DbConnections.UnitTest.yml
    %
    % From command line (term), connect to server:
    % 
    %       psql -h gauss -U admin -d unittest -W 
    %
    % ServerSharePath : '/var/samba/pgshare'
    % MountSharePath  : '/media/gauss_pgshare' 
    %
    %----------------------------------------------------------------------
    
    % Create DbAdmin instance from arguments:
    %
    % When you connect, you must specify a database name.
    % Postgres always have a database called 'postgres' that you can connect
    % to with full permissions.
    % Default port is 5432, we connect to server named 'gauss'
    Admin = db.DbAdmin('Host', 'gauss', 'Port', 5432, 'UserName', 'postgres', 'Password', 'PassRoot', 'DatabaseName', 'postgres');
    
    % Create local configuration file for database
    % Local configuration files are stored in AstroPack/config/local/ folder
    ConfigFileName = Admin.createConnectionConfig('DatabaseName', 'unittest6', 'Host', 'gauss', 'Port', 5432, 'UserName', 'admin', 'Password', 'Passw0rd');
    fprintf('Config file created: %s\n', ConfigFileName);
        
    % Create database from XLSX file (Google Sheets)
    % NOTE: If the system() call fails, copy and paste the command in new
    % 'term' window. 
    % NOTE: bash uses 'export var=...' and tcsh uses 'setenv var ...'
    % NOTE: You need Ultrasat repoistory and ULTRASAT_PATH environment set 
    %       to its location, because the Python script is located there.
    %
    % Assume that we downloaded Google Sheets file and stored it as 'unittest5.xlsx'.
    xlsx = fullfile(tools.os.getAstroPackPath(), 'database/xlsx/unittest5.xlsx');
    if isfile(xlsx)
        % Check if database already exists
        Exists = Admin.isDatabaseExist('unittest5');
        fprintf('Before create, Exists: %d\n', Exists);
        
        % Create database from file
        Result = Admin.createDatabase('XlsFileName', xlsx);

        % Check if it was created        
        Exists = Admin.isDatabaseExist('unittest5');
        fprintf('Afetr create, Exists: %d\n', Exists);        
    end
   
    % Get list of databases
    DbList = Admin.getDbList();
    disp(DbList);
    
    % Get list of users (roles)
    UserList = Admin.getUserList();
    disp(UserList);
    
    % Add user
    % To check that user was created, use pgAdmin, right-click on the
    % database name (i.e. 'unittest5'), select 'Properties' and open tab 'Secutity'
    %
    % Add user with full permissions on specified database
    Admin.addUser('test2', 'pass', 'DatabaseName', 'unittest5', 'Permission', 'full');
    
    % Add user with read-only permissions on specified database
    Admin.addUser('test2r', 'pass', 'DatabaseName', 'unittest5', 'Permission', 'read');    

    % Remove user
    % Note that removing users may fail of there are databases or tables
    % that depends on this user.
    % In such case, it is required to first remove the dependecy using pgAdmin.
    % Admin.removeUser('Test1');
    
    Result = true;
end
