% AstroImage database adaptor

classdef AstroDb < Component
    
    properties (Hidden, SetAccess = public)

    
    end
    

    methods % Constructor    
        
        function Obj = AstroDb
            Obj.setName('AstroImageDb')
            
        end

    end
    
    
    methods
               
        function Result = insertHeader(Obj, Input, TableName, Args)
            % Insert AstroHeader/AstroImage object to the specified database table
            arguments
                Obj
                Input                           % AstroHeader / AstroImage
                TableName char                
                Args.Fields = {}                % As
                Args.Uuid = []                  % Empty uses AstroHeader.Uuid
                Args.Query = []                 % io.db.DbQuery
            end
            
            Obj.msgLog(LogLevel.Debug, 'insertHeader started');
            if isa(Input, 'AstroHeader')
                Obj.msgLog(LogLevel.Debug, 'insertHeader input is AstroHeader');
            elseif isa(Input, 'AstroImage')
                Obj.msgLog(LogLevel.Debug, 'insertHeader input is AstroImage');
            else
                error('insertHeader: Input type not supported');
            end
            
            % Set Query
            if isempty(Args.Query)
                Query = Obj.getDefaultQuery(Args);
            else
                Query = Args.Query;
            end
            
            if isa(Input, 'AstroImage')                
                % Iterate all headers in array, treat each one as independent data
                for i=1:numel(Input)

                    for j=1:numel(Input.Header)
                        % Get header as cell{key, value, comment}                
                        HeaderData = Input(i).Header(j).Data;
                        ExFields = struct;
                        ExFields.rawimageid = Component.newUuid();
                        Result = Query.insertCell(TableName, HeaderData, 'ExFields', ExFields);
                    end
                end                    
            elseif isa(Input, 'AstroHeader')
                % Iterate all headers in array, treat each one as independent data
                for i=1:numel(Input)
                    HeaderData = Input(i).Data;                  
                    ExFields = struct;
                    ExFields.rawimageid = Component.newUuid();
                    Result = Query.insertCell(TableName, HeaderData, 'ExFields', ExFields);
                end
            end
            
            Obj.msgLog(LogLevel.Debug, 'insertHeader done');
        end
        
        
        
        function Result = insertCatalog(Obj, Input, TableName, Args)
            % Insert AstroCatalog / AstroTable to the specified database table
            % Note that AstroCatalog is derived from AstroTable
            
            % Values in AstroCatalog.Catalog = [];
            % Field names in AstroCatalog.ColNames cell = {};            
            arguments
                Obj
                Input                   % AstroHeader / AstroImage                
                TableName char                
                Args.Fields = {}        % As
                Args.Uuid = []          % Empty uses AstroHeader.Uuid
                Args.Query = []         % io.db.DbQuery
            end
            
            Obj.msgLog(LogLevel.Debug, 'insertCatalog started');                        

            if isa(Input, 'AstroCatalog')
                Obj.msgLog(LogLevel.Debug, 'insertCatalog input is AstroCatalog');
            elseif isa(Input, 'AstroTable')
                Obj.msgLog(LogLevel.Debug, 'insertCatalog input is AstroTable');
            else
                error('insertCatalog: Input type not supported');
            end
            
            % Set Query
            if isempty(Args.Query)
                Query = Obj.getDefaultQuery(Args);
            else
                Query = Args.Query;
            end
            
            % Iterate all headers in array, treat each one as independent data
            for i=1:numel(Input)
                
                % Get header as cell{key, value, comment}
                Data = struct;
                Data.sourceid = Component.newUuid();
                
                ColCount = numel(Input(i).ColNames);
                for j=1:ColCount
                    ColName = Input(i).ColNames{j};
                    Data.(ColName) = Input(i).Catalog(j);
                end
                                              
                Result = Query.insertRecord(TableName, Data);
            end
                        
            Obj.msgLog(LogLevel.Debug, 'insertCatalog done');
        end        
    end
             
    %
    methods % (Static???)
        function Result = getDefaultQuery(Obj, Args)
            % Get default database query
            arguments
                Obj
                Args
            end
            
            Conn = io.db.Db.getLast();
            Query = io.db.DbQuery(Conn);            
            Result = Query;
        end
        
    end
    
    % 
    
    % setters/getters
    methods
                
    end
    
    %----------------------------------------------------------------------
    % Unit test
    methods(Static)
        function Result = unitTest()
            io.msgStyle(LogLevel.Test, '@start', 'AstroDb test started')
               
            % Create db adaptor
            db = AstroDb;                                   
            
            % Get db connection
            Conn = io.db.Db.getLast();
            Q = io.db.DbQuery(Conn);
            Q.query('SELECT version()');
            assert(Q.ColCount == 1);
            pgver = Q.getField('version');
            io.msgLog(LogLevel.Test, 'Version: %s', pgver);
            assert(contains(pgver, 'PostgreSQL'));
            
            HeaderTableName = 'raw_images';
            CatalogTableName = 'sources';
            
            % Create fits file with header
            Folder = tempdir; 
            %Folder = 'c:\temp';
            Header = { 'RA_NE', [1], 'Comment 1';  'RA_SE', [2], 'Comment 2'; 'RA_SW', [3], 'Comment 3';  'RA_NW', [4], 'Comment 4' };
            ImageData = zeros(10, 10);
            ImageName = fullfile(Folder, 'AstroImageDbTest.fits');
            if isfile(ImageName)
                delete(ImageName);
            end
                
            fitswrite(ImageData, ImageName)            
            FITS.write_keys(ImageName, Header);
            AH = AstroHeader(ImageName);
            assert(all(size(AH.Data)));

            % Create catalog
            AC = AstroTable({rand(10, 4), rand(10, 4)}, 'ColNames', {'A', 'B', 'C', 'D'});
            
            % Insert header to table
            tic;
            Count = 10;
            for i=1:Count
                res = db.insertHeader(AH, HeaderTableName);
            end
            toc
                
            % Insert catalog to table
            Count = 10;
            for i=1:Count
                res = db.insertCatalog(AC, CatalogTableName);
            end
                
            %----------------------------------------------------- Batch
            
            % Insert many with batch = 1
            Count = 0;  %1000;
            tic();
            for i=1:Count
                res = db.insertHeader(AH, HeaderTableName);
            end            
            Toc = toc();
            io.msgLog(LogLevel.Info, 'insertHeader time (Count=%d): %.6f', Count, Toc);  
                                
            % Insert many with batch = 1000
            Count = 0; %100;
            BatchSize = 1000;
            tic();
            for i=1:Count
                res = db.insertHeader(AH, HeaderTableName, 'BatchSize', BatchSize);
            end
            
            Toc = toc();
            io.msgLog(LogLevel.Info, 'insertHeader time (Count=%d, Batch=%d): %.6f', Count, BatchSize, Toc);  
                        
            
            io.msgStyle(LogLevel.Test, '@passed', 'AstroDb test passed')
            Result = true;
        end
    end    
             

end

            
