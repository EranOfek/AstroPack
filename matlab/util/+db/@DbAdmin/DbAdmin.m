% DbQuery - SQL Database query
%
% Basic usage:
%
%   Select and load records, automatically convert to output type:
%
%     Q = db.DbQuery('unittest:master_table');
%     Mat = Q.select('fdouble1,fdouble2,fdouble3', 'Where', 'fdouble1 > fdouble2', 'OutType', 'mat', 'Limit', 100000);
%
%
%   Insert single record
%
%   Insert records with callback to add fields not in input:
%
%
%     function Result = makePK(Q, Rec, First, Last)
%        for i=First:Last
%            Rec.Data(i).recid = sprintf('PK_%s', Rec.newKey());
%        end
%        Result = true;
%     end
%
%     Mat = rand(10, 3);
%     R = db.DbRecord(Mat, 'ColNames', 'fdouble1,fdouble2,fdouble3');
%     Q.insert(R, 'InsertRecFunc', @makePK, 'BatchSize', 10000);
%
%
% PostgreSQL V14 - Installation instructions for Linux:
%
%     https://techviewleo.com/how-to-install-postgresql-database-on-ubuntu/
%
% Create database on remote server (password: 'Passw0rd')
%
%     psql -h gauss -p 5432 -U admin -W -d postgres -f unittest.sql
%
% Using COPY:
% Still need to find a solution for this:
% https://gpdb.docs.pivotal.io/6-9/admin_guide/load/topics/g-loading-data-with-copy.html
% The COPY source file must be accessible to the postgres process on the master host. 
% Specify the COPY source file name relative to the data directory on the master host, or specify an absolute path.
%
%--------------------------------------------------------------------------

classdef DbAdmin < Component

    % Properties
    properties (SetAccess = public)

        % Connection details
        Conn            = []        % DbConnection
        Query           = []        % DbQuery
        
        % Current SQL statement data
        SqlText         = ''        % SQL text

    end

    %----------------------------------------------------------------------
    methods % Constructor

        % Constructor
        function Obj = DbAdmin(DbTableOrConn, Args)
            % Create new DbQuery obeject
            % Input:
            %   DbTableOrConn - Database alias from Database.yml, with
            %   optional table name, for example: 'UnitTest'
            %
            % Examples:
            %   % Create query object for 'UnitTest' database alias 'UnitTest'
            %   Q = DbQuery('UnitTest')
            %
            %   % Create query object for 'UnitTest' database and table
            %   'master_table'
            %   Q = DbQuery(UnitTest:master_table')
            %
            %   % Create query object for custom database connection (not
            %   from Database.yml)
            %   MyConn = DbConnection('Db', 'MyAlias', ...)
            %   Q.DbQuery(MyConn)
            %
            arguments
                DbTableOrConn   = []        % DbAlias / DbAlias:TableName / DbConnection object
                Args.TableName              % Set TableName when not included in DbTable parameter
                Args.PrimaryKey             % Primary key(s)
                Args.InsertRecFunc
            end

            % Setup component
            Obj.setName('DbQuery');
            Obj.needUuid();
            Obj.DebugMode = true;
            %Obj.msgLog(LogLevel.Debug, 'created: %s', Obj.Uuid);

            % Set connection
            Obj.setConnection(DbTableOrConn);

            % Override TableName and set other properties
            Obj.setProps(Args);

        end


        % Destructor
        function delete(Obj)
            Obj.clear();
            Obj.msgLog(LogLevel.Debug, 'deleted: %s', Obj.Uuid);
        end
    end

    %----------------------------------------------------------------------
    methods(Static)    
        
        function Result = createDatabase(Obj, DbName, Args)
            % Create database
            % Input:   DbName
            % Output:  true on success
            % Example: -             
            
            arguments
                Obj
                DbName              %
                Args.Script = ''    % Script text
                Args.Fields = []    %
            end

        end

        
        function Result = createConnection(Obj, DbName, Args)
            % Create database
            % Input:   DbName
            % Output:  true on success
            % Example: -             
            
            arguments
                Obj
                DbName              %
                Args.Script = ''    % Script text
                Args.Fields = []    %
            end

        end
        
              
        
        function Result = createTable()
            %
            % Input:   
            % Output:  
            % Example: 
            % https://www.postgresql.org/docs/8.0/sql-createuser.html

%         DROP TABLE IF EXISTS customers CASCADE;
% 
%         CREATE TABLE customers (
%             id SERIAL PRIMARY KEY,
%             customer_name VARCHAR NOT NULL
%         );
        end
        
        
        function Result = addColumn(TableName, ColumnName, DataType)                   
            %
            % Input:   
            % Output:  
            % Example: 
            arguments
                TableName 
                ColumnName
                DataType
            end

            
            % https://www.postgresqltutorial.com/postgresql-add-column/
            
%             ALTER TABLE table_name
%             ADD COLUMN new_column_name data_type constraint;

%             ALTER TABLE table_name
%             ADD COLUMN column_name1 data_type constraint,
%             ADD COLUMN column_name2 data_type constraint,
%             ...
%             ADD COLUMN column_namen data_type constraint;
        end

        
        function Result = addIndex()
            %
            % Input:   
            % Output:  
            % Example: 
            %
            % https://www.postgresql.org/docs/9.1/sql-createindex.html
            % https://www.postgresqltutorial.com/postgresql-indexes/postgresql-create-index/
            %for field in self.field_list:
            %if field.index:
            %    index_name = self.table_name + '_idx_' + field.field_name
            %    self.write('CREATE INDEX {} ON public.{}\n  USING {} ({});\n\n'.format(index_name, self.table_name, field.index_method, field.field_name))
            % CREATE INDEX index_name ON table_name [USING method]
            %(
            %    column_name [ASC | DESC] [NULLS {FIRST | LAST }],
            %    ...
            %);

            
        end        
        
        
        
        function Result = addUser(UserName, Password)
            %
            % Input:   
            % Output:  
            % Example: 
            arguments
                UserName 
                Password

            end

            
            % https://www.postgresql.org/docs/8.0/sql-createuser.html
            % create user user_name with encrypted password 'mypassword';
            % grant all privileges on database sample_db to user_name;
        end
        
        
        function Result = removeUser(UserName)
            %
            % Input:   
            % Output:  
            % Example: 
            
            arguments
                UserName 

            end
            
            % https://www.postgresql.org/docs/9.4/sql-dropuser.html
            % DROP USER [ IF EXISTS ] name [, ...]
        end        
        
        

        function Result = writeLocalConfig(DatabaseName)
            % config/local/Database.DbConnections.UnitTest.yml
            % Input:   
            % Output:  
            % Example: 
            
            arguments
                DatabaseName 

            end
            
            ConfigPath = '';
            FileName = fullfile(ConfigPath, 'local', strcat('Database.DbConnections.', DatabaseName, '.yml');
            
            Fid = fopen(Obj.FileName, 'wt');
            
            fprintf(Fid, '# %s\n', FileName);

            fprintf(Fid, 'DatabaseName    : ''%s''        # Database name\n');
            fprintf(Fid, 'Host            : ''%s''           # Host name or IP address\n');
            fprintf(Fid, 'DriverName      : ''%s''        # Driver name\n');
            fprintf(Fid, 'UserName        : ''%s''           # Login user\n');
            fprintf(Fid, 'Password        : ''%s''        # Login password\n');
            fprintf(Fid, 'Port            : %d              # Port number\n');

            fclose(Fid);
            
        end        

        
        
        function Result = xls2sql(XlsFileName, Args)
            % 
            % Input:   XlsFileName
            % Output:  
            % Example: db.DbQuery.xls2sql('c:\temp\_xls\unittest.xlsx')
            arguments
                XlsFileName
                Args.CreateDb = true;
            end
            
            if ~isfile(XlsFileName)
                Result = false;
                return;
            end
                
            PWD = pwd;
            try
                
                [Path, FName] = fileparts(XlsFileName);
                cd(Path);
                Py = fullfile(tools.os.getAstroPackPath, 'python', 'utils', 'database_utils', 'xlsx2sql.py');
                Cmd = sprintf('python3 %s -f %s', Py, XlsFileName);
                io.msgLog(LogLevel.Info, 'xlsx2sql.py: %s', Cmd);
                [Status, Output] = system(Cmd);
                io.msgLog(LogLevel.Info, '%s', Output);
                
                SqlFileName = sprintf('%s%s%s.sql', FName, filesep, FName);
                if isfile(SqlFileName)
                    
                    if Args.CreateDb
                        Psql = sprintf('psql --port=5433 -U postgres -f %s', SqlFileName);
                        io.msgLog(LogLevel.Info, 'xls2sql: %s', Psql);
                        [Status, Output] = system(Psql);
                        io.msgLog(LogLevel.Info, 'psql: %s', Output);
                    end
                end
                
            catch Ex
            end
            cd(PWD);
        end
        
    end


    methods(Static) % Unit-Tests

        Result = unitTest()
            % Unit-Test

    end
end
