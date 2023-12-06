% File: Database.m
classdef Database < handle
    properties (Access = private)
        m_datasource = 'avionics';
        m_username = 'jack';
        m_password = 'jackdaniels';
        m_driver = 'org.postgresql.Driver';
        m_url = 'jdbc:postgresql://localhost:5432/avionics';
        m_schema = 'public';
        m_conn = 0;
    end
    
    properties (Access = public)
        record = 0;
    end
    
    methods (Access = protected)
        
        function connect(obj)
            obj.m_conn = database(obj.m_datasource, obj.m_username, obj.m_password, obj.m_driver, obj.m_url);
            
            switch isopen(obj.m_conn)
                case 1
                    disp('Connection with database is OK.')
                otherwise
                    error('Failed to connection with database.')
            end
            fprintf(1, '\n')
        end
        
    end

    methods (Access = public)
        
        function obj = Database()
            disp('Initialize database...')
            obj.connect();
        end
        
        function disconnect(obj)
            disp('Closing connect with database...')
            close(obj.m_conn)
        end
        
        function sqlGetAircraftRoute(obj, aircraft)
            try
                tic
                fprintf('Running query to get (aircraft route). Please wait...\n')
                q = "SELECT * FROM " + obj.m_schema + ".func_gis_route(0) WHERE aircraft=" + aircraft;
                fprintf('\tSQL> %s\n', q)
                obj.record = select(obj.m_conn, q);
                toc
                fprintf(1, '\n')
            catch
                warning('Database:sqlGetAircraftRoute',...
                    'Failed to execute query: %s', q)
            end
        end
        
    end
end
