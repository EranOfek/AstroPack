% ClickHouse database interface
% Chen Tishler, 04/08/2024

classdef ClickHouseDb < DbBase
    properties (Access = private)
        Connection % Property to hold the database connection
    end
    
    methods
        function obj = ClickHouseDb()
            % Constructor to initialize the database connection
            py.importlib.import_module('clickhouse_driver');
            conn = py.clickhouse_driver.Client('socsrv', pyargs('user', 'default', ...
                                           'password', 'PassRoot', ...
                                           'database', 'sources_test', ...
                                           'port', '9000'));
            obj.Connection = conn;
        end
        

        function delete(Obj)
            % Destructor to close the database connection
            % ClickHouse Client doesn't require explicit close operation
        end


        function data = select(Obj, query)
            % Method to execute a query and fetch data
            %result = Obj.Connection.execute(query);
            
            %result = py.clickhouse_helper3.select(Obj.Connection, query);

            helper = py.clickhouse_class2.DBHelper(Obj.Connection);
            result = helper.select(query);
            data = Obj.convertPyToCell(result);
        end
    end
end

