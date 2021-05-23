% AstroImage database adaptor

classdef AstroImageDb < Component
    
    properties (Hidden, SetAccess = public)

    
    end
    

    methods % Constructor    
        
        function Obj = AstroImageDb
            
        end

    end
    
    
    methods
        function Result = insertAstroHeader(Obj, Header, Query, TableName, Args)
            arguments
                Obj
                Header AstroHeader
                Query io.db.DbQuery
                TableName string                
                Args.Fields = {};       % As
            end
            
            % Iterate headers
            for i=1:numel(Header)
                FieldNames = Query.getFieldList();
                Keys = Header(i).KeyDict.FieldNames;
                
                % Iterate keys
                % sql = sprintf("INSERT INTO master_table(RecID, FInt) VALUES ('%s', %d)", uuid, 1).char;
                SqlText = ['INSERT INTO ', string(TableName).char, ' ('];
                SqlFields = '';
                SqlValues = ' VALUES (';
                for f=1:numel(Keys)
                    Key = Keys{f};
                    if any(contains(FieldNames, Key))
                        if length(SqlFields) > 0:
                            SqlText = [SqlText ',' Key];
                            SqlValues = [SqlValues ',' Value];
                        else
                            SqlText = [SqlText Key];
                            SqlValues = [SqlValues Value];
                        end
                    else
                    end
                    
                end
                
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

            
