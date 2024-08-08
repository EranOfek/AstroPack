% Postgres interface class
% Chen Tishler, Created: 04/08/2024
% https://chatgpt.com/c/dbbdbb28-9dfa-4d92-992c-d2b0ee401163

classdef Db < DbBase
    properties (Access = private)
        Connection % Property to hold the database connection
    end
    
    methods
        function Obj = Db()
            % Constructor to initialize the database connection
            pyModule = py.importlib.import_module('psycopg2');
            conn = pyModule.connect(pyargs('dbname', 'socdb', ...
                                           'user', 'postgres', ...
                                           'password', 'PassRoot', ...
                                           'host', 'socsrv', ...
                                           'port', '5432'));
            Obj.Connection = conn;
        end



        function delete(Obj)
            % Destructor to close the database connection
            if ~isempty(Obj.Connection)
                Obj.Connection.close();
            end
        end

        
        function data = select(Obj, query)
            % Method to execute a query and fetch data
            cur = Obj.Connection.cursor();
            cur.execute(query);
            result = cur.fetchall();
            cur.close();

            data = Obj.convertPyToCell(result);
        end
    end
end

