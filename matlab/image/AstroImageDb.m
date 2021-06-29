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
                TableName char                
                Args.Fields = {};       % As
            end
            
            Obj.msgLog(LogLevel.Debug, 'insertAstroHeader');
            
            % Get list of table's fields 
            FieldNames = Query.getTableFieldList(TableName);
            
            % Iterate all headers in array, treat each one as independent data
            for i=1:numel(Header)
                
                HeaderData = Header(i).Data;
                DataSize = size(HeaderData);
                
                % Prepare data record
                HeaderRec = struct;
                
                % @Todo: ???
                HeaderRec.recid = Component.newUuid();
                
                % Iterate all fields
                NumKeys = DataSize(1);
                for f = 1:NumKeys
                    
                    % Add field name and value to record
                    Key = HeaderData{f, 1};
                    if isempty(Key)
                        continue
                    end
                    
                    Value = HeaderData{f, 2};
                    
                    % Convert key to valid database field name
                    FieldName = Query.getValidFieldName(Key);
                    
                    if ~strcmp(Key, FieldName)
                        Obj.msgLog(LogLevel.Debug, 'Key: %s, Field: %s', Key, FieldName);
                    end
                    
                    HeaderRec.(FieldName) = Value;
                    
                end
                
                Query.insertRecord(TableName, HeaderRec);
                
                
                % Prepare SQL statement

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
    
            % Get db connection
            Conn = io.db.Db.getUnitTest();
            Q = io.db.DbQuery(Conn);
            Q.query('SELECT version()');
            assert(Q.ColCount == 1);
            pgver = Q.getField('version');
            io.msgLog(LogLevel.Test, 'Version: %s', pgver);
            assert(contains(pgver, 'PostgreSQL'));
            TableName = 'master_table';
            
            % Create db adaptor
            db = AstroImageDb;                        
            
            % Load header from sample fit
            DataSampleDir = tools.os.getTestDataDir;                                 
            H = AstroHeader(fullfile(DataSampleDir, 'WFPC2ASSNu5780205bx.fits'));
            assert(all(size(H.Data)));

            % Insert header to table
            res = db.insertAstroHeader(H, Q, TableName);
                
            io.msgStyle(LogLevel.Test, '@passed', 'AstroImageDb test passed')
            Result = true;
        end
    end    
             

end

            
