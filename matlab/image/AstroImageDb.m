% AstroImage database adaptor

classdef AstroImageDb < Component
    
    properties (Hidden, SetAccess = public)

    
    end
    

    methods % Constructor    
        
        function Obj = AstroImageDb
            Obj.setName('AstroImageDb')
            
        end

    end
    
    
    methods
        
        
        function Result = insertAstroHeader(Obj, Header, Query, TableName, Args)
            % Insert AstroHeader object to the specified database table
            arguments
                Obj
                Header AstroHeader
                Query io.db.DbQuery
                TableName string                
                Args.Fields = {};       % As
            end
            
            % Get list of table's fields 
            FieldNames = Query.getTableFieldList(TableName);
            
            % Iterate all headers in array, treat each one as independent data
            for i=1:numel(Header)
                
                HeaderData = Header(i).Data;
                DataSize = size(HeaderData);
                
                % Prepare SQL statement
                % sql = sprintf("INSERT INTO master_table(RecID, FInt) VALUES ('%s', %d)", uuid, 1).char;
                SqlText = ['INSERT INTO ', string(TableName).char, ' ('];
                SqlFields = '';
                SqlValues = ' VALUES (';
                              
                % Iterate keys
                for f=1:DataSize(1)
                    Key = HeaderData{i, 1};
                    
                    % Check if there is a field matching the header key
                    if any(contains(FieldNames, Key))
                        
                        % 
                        if numel(SqlFields) > 0:
                            SqlText = [SqlText ',' Key];
                            SqlValues = [SqlValues ',' Value];
                        else
                            SqlText = [SqlText Key];
                            SqlValues = [SqlValues Value];
                        end
                    else
                    end
                    
                end
                
                % Now we have keys and values
            end
            
            
        end
    end
    
    
    
    
    % static methods
    methods (Static)
       
    end
    
    % 
    
    % setters/getters
    methods
                
    end
    
    %----------------------------------------------------------------------
    % Unit test
    methods(Static)
        function Result = unitTest()
            io.msgStyle(LogLevel.Test, '@start', 'AstroImageDb test started')
    
            

            io.msgStyle(LogLevel.Test, '@passed', 'AstroImageDb test passed')
            Result = true;
        end
    end    
             

end

            
