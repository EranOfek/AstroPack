
function Result = examples()
    % DbAdmin examples
    % Install Postgres v14 - See detailed instructions in file
    % 'postgressql_installation.md'
    %
    % Install pgAdmin4 (version 6.3) as administration tool    
    % On Windows, use SQL Manager Lite for PostgreSQL by EMS Software
    % On Linux, use DataGrip by JetBrains
    %
    % You need to have configuration file with database user and password:
    % config/local/Database.DbConnections.UnitTest.yml
    %
    % From command line (term), connect to server:
    % psql -h gauss -U admin -d unittest -W 
    %
    % ServerSharePath : '/var/samba/pgshare'
    % MountSharePath  : '/media/gauss_pgshare' 
    %
    
    % Create DbAdmin from arguments
    % Postgres always have a database called 'postgres' that you can connect
    % to with full permissions
    Admin = db.DbAdmin('Host', 'gauss', 'Port', 5432, 'UserName', 'postgres', 'Password', 'PassRoot', 'DatabaseName', 'postgres');
    
    % Create local configuration file for database
    ConfigFileName = Admin.createConnectionConfig('DatabaseName', 'unittest6', 'Host', 'gauss', 'Port', 5432, 'UserName', 'admin', 'Password', 'Passw0rd');
    fprintf('Config file created: %s\n', ConfigFileName);
        
    % Create database from XLSX file (Google Sheets)
    % NOTE: If the system() call fails, copy and paste the command in new
    % 'term' window. Note that bash uses 'export var=...' and tcsh uses
    % 'setenv var ...'
    % Note: You need Ultrasat repoistory and ULTRASAT_PATH environment set to its location
    xlsx = fullfile(tools.os.getAstroPackPath(), 'database/xlsx/unittest5.xlsx');
    if isfile(xlsx)
        % Check if database exists
        Exists = Admin.isDatabaseExist('unittest5');
        fprintf('Before create, Exists: %d\n', Exists);
        
        % Create
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
    Admin.addUser('test2', 'pass', 'DatabaseName', 'unittest5', 'Permission', 'full');
    Admin.addUser('test2r', 'pass', 'DatabaseName', 'unittest5', 'Permission', 'read');    

    % Remove user
    % Admin.removeUser('Test1');
    
    Result = true;
end
