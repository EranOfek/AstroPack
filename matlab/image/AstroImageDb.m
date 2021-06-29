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
            
            Obj.msgLog(LogLevel.Debug, 'insertAstroHeader started');
                        
            % Iterate all headers in array, treat each one as independent data
            for i=1:numel(Header)
                
                % Get header as cell{key, value, comment}
                HeaderData = Header(i).Data;
                
                ExFields = struct;
                ExFields.rawimageid = Component.newUuid();
                Result = Query.insertCell(TableName, HeaderData, 'ExFields', ExFields);
            end
                        
            Obj.msgLog(LogLevel.Debug, 'insertAstroHeader done');
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
            Conn = io.db.Db.getLast();
            Q = io.db.DbQuery(Conn);
            Q.query('SELECT version()');
            assert(Q.ColCount == 1);
            pgver = Q.getField('version');
            io.msgLog(LogLevel.Test, 'Version: %s', pgver);
            assert(contains(pgver, 'PostgreSQL'));
            TableName = 'raw_images';
            
            % Create fits file with header
            Folder = tempdir;            
            Header = { 'RA_NE', [1], 'Comment 1';  'RA_SE', [2], 'Comment 2'; 'RA_SW', [3], 'Comment 3';  'RA_NW', [4], 'Comment 4' };
            ImageData = zeros(10, 10);
            ImageName = fullfile(Folder, 'AstroImageDbTest.fits');
            fitswrite(ImageData, ImageName)            
            FITS.write_keys(ImageName, Header);
            
            % Create db adaptor
            db = AstroImageDb;                        
            
            % Load header from sample fit
            %DataSampleDir = tools.os.getTestDataDir;                                 
            %H = AstroHeader(fullfile(DataSampleDir, 'WFPC2ASSNu5780205bx.fits'));
            
            H = AstroHeader(ImageName);
            assert(all(size(H.Data)));

            % Insert header to table
            for i=1:10
                res = db.insertAstroHeader(H, Q, TableName);
            end
                
            io.msgStyle(LogLevel.Test, '@passed', 'AstroImageDb test passed')
            Result = true;
        end
    end    
             

end

            
