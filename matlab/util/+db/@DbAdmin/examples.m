
function Result = examples()
    % DbAdmin examples
    % Install Postgres v14
    % Install pgAdmin4 (version 6.3) as administration tool    
    % On Windows, use SQL Manager Lite for PostgreSQL by EMS Software
    % On Linux, use DataGrip by JetBrains
    %
    % You need to have configuration file with database user and password:
    % config/local/Database.DbConnections.UnitTest.yml
    
    % From command line (term), connect to server:
    % psql -h gauss -U admin -d unittest -W
       
    % Create DbAdmin from arguments
    % Postgres always have a database called 'postgres' that you can connect
    % to with full permissions
    Admin = db.DbAdmin('Host', 'gauss', 'Port', 5432, 'UserName', 'postgres', 'Password', 'PassRoot', 'DatabaseName', 'postgres');
    
    % Create configuration file
    ConfigFileName = Admin.createConnectionConfig('DatabaseName', 'unittest5', 'Host', 'gauss', 'Port', 5432, 'UserName', 'admin', 'Password', 'Passw0rd');
    fprintf('Config file created: %s\n', ConfigFileName);
        
    % Create database from XLSX file (Google Sheets)
    Admin.createDatabase('XlsFileName', 'D:\Ultrasat\AstroPack.git\database\xlsx\unittest5.xlsx');
   
    % Get list of databases
    DbList = Admin.getDbList();
    disp(DbList);
    
    % Get list of users (roles)
    UserList = Admin.getUserList();
    disp(UserList);
      
    % Add user
    Admin.addUser('Test1', 'Password1', 'DatabaseName', 'unittest5', 'Permission', 'full');
    Admin.addUser('Test1r', 'Password1', 'DatabaseName', 'unittest5', 'Permission', 'read');    

    % Remove user
    % Admin.removeUser('Test1');
    
    Result = true;
end
