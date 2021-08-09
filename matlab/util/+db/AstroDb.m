% AstroImage database adaptor

% Usage: Use the static functions
%
%     db.AstroDb.insertHeader
%     db.AstroDb.insertCatalog
%     db.AstroDb.getDefaultQuery

% Threads
% https://undocumentedmatlab.com/articles/explicit-multi-threading-in-matlab-part1
% https://undocumentedmatlab.com/articles/explicit-multi-threading-in-matlab-part2

classdef AstroDb < Component
    
    properties (Hidden, SetAccess = public)
        OperQueue       % Queue of AstroDbOper
    
    end
    

    methods % Constructor    
        
        function Obj = AstroDb
            Obj.setName('AstroDb')
            
        end

    end
    
    
    methods
               
        function Result = insertHeaderImpl(Obj, Input, TableName, Args)
            % Insert AstroHeader/AstroImage object to the specified database table
            arguments
                Obj
                Input                           % AstroHeader / AstroImage
                TableName char                
                Args.Fields = {}                % As
                Args.Uuid = []                  % Empty uses AstroHeader.Uuid
                Args.Query = []                 % db.DbQuery
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
        
        
        
        function Result = insertCatalogImpl(Obj, Input, TableName, Args)
            % Insert AstroCatalog / AstroTable to the specified database table
            % Note that AstroCatalog is derived from AstroTable
            
            % Values in AstroCatalog.Catalog = [];
            % Field names in AstroCatalog.ColNames cell = {};            
            arguments
                Obj
                Input                   % AstroHeader / AstroImage                
                TableName char                
                Args.KeyField = 'proc_iid'
                Args.Fields = {}        % As
                Args.Uuid = []          % Empty uses AstroHeader.Uuid
                Args.Query = []         % db.DbQuery
                Args.BatchSize = 1000   % Insert batch size
            end
            
            
            KeyField = Args.KeyField;
            
            Obj.msgLog(LogLevel.Debug, 'insertCatalogImpl started');                        

            if isa(Input, 'AstroCatalog')
                Obj.msgLog(LogLevel.Debug, 'insertCatalogImpl input is AstroCatalog');
            elseif isa(Input, 'AstroTable')
                Obj.msgLog(LogLevel.Debug, 'insertCatalogImpl input is AstroTable');
            else
                error('insertCatalogImpl: Input type not supported');
            end
            
            % Set Query
            if isempty(Args.Query)
                Query = Obj.getDefaultQuery(Args);
            else
                Query = Args.Query;
            end

            
            % Iterate all catalogs
            % @Todo: Key record?
            for i = 1:numel(Input)
                               
                % Prepare catalog as struct array
                % @Todo: Ask @Eran if there is a better/faster way to do it
                ColNames = Input(i).ColNames;
                [Rows, Cols] = size(Input(i).Catalog);
                s = [];
                T = tic();
                Uuid = Component.newUuid();
                for Row = 1:Rows
                    s(Row).(KeyField) = sprintf('%s-%07d', Uuid, Row);
                    s(Row).('src_id') = Row;
                    for Col = 1:Cols
                        s(Row).(ColNames{Col}) = Input(i).Catalog(Row, Col);
                    end                    
                end
                Time = toc(T);
                Obj.msgLog(LogLevel.Info, 'insertCatalogImpl: prepare struct array: Rows: %d, Cols: %d, Time: %f', Rows, Cols, Time);
            
                % Prepare
                Obj.msgLog(LogLevel.Info, 'insertCatalogImpl: Records: %d, Batch: %d', Rows, Args.BatchSize);
                Count1 = Query.selectCount(TableName);                
                T = tic();
                
                Result = Query.insertRecord(TableName, s, 'BatchSize', Args.BatchSize);
                Time = toc(T);
                Obj.msgLog(LogLevel.Info, 'insertCatalogImpl: insertRecord time: %f per %d records, %f per record', Time, Rows, Time / Rows);
            
                % Validate that we added the correct number of records
                % (Might be bigger if other process inserts records concurrently)
                Count2 = Query.selectCount(TableName);
                if Count2 < Count1 + Rows
                    Obj.msgLog(LogLevel.Info, 'insertCatalogImpl: Wrong number of records: Count1: %d, Count2: %d', Count1, Count2);
                end                 
            end  
            
            Obj.msgLog(LogLevel.Debug, 'insertCatalogImpl done');
        end        
    end
       
    
    %
    methods %
        function Result = manage(Obj)
            % Manage queue of pending operations

            % Get next operation from queue
            Count = 0;
            Max = 1;
            while (true)
                Count = Count + 1;
                if Count >= Max
                    break;
                end
            end
            
            Result = true;
        end
        
    end

    
    methods (Static) % Static functions
        
        function Result = insertHeader(varargin) % Input, TableName, Args)
%             % Insert AstroHeader/AstroImage object to the specified database table
%             arguments
%                 Input                           % AstroHeader / AstroImage
%                 TableName char                
%                 Args.Fields = {}                % As
%                 Args.Uuid = []                  % Empty uses AstroHeader.Uuid
%                 Args.Query = []                 % db.DbQuery
%             end

            Result = db.AstroDb.get().insertHeaderImpl(varargin{:});  %Input, TableName, Args);
        end
        
        
        
        function Result = insertCatalog(varargin) % Input, TableName, Args)
            % Insert AstroCatalog / AstroTable to the specified database table
            % Note that AstroCatalog is derived from AstroTable
            
            % Values in AstroCatalog.Catalog = [];
            % Field names in AstroCatalog.ColNames cell = {};            
%             arguments
%                 Input                   % AstroHeader / AstroImage                
%                 TableName char                
%                 Args.Fields = {}        % As
%                 Args.Uuid = []          % Empty uses AstroHeader.Uuid
%                 Args.Query = []         % db.DbQuery
%             end
        
            Result = db.AstroDb.get().insertCatalogImpl(varargin{:}); %Input, TableName, Args);
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
            
            Conn = db.Db.getLast();
            Query = db.DbQuery(Conn);            
            Result = Query;
        end
        
    end
    
    
    %
    methods(Static)
        function Result = get()
            persistent Obj
            if isempty(Obj)
                Obj = db.AstroDb();
            end
            Result = Obj;
        end        
    end    
    
    
    %----------------------------------------------------------------------
    % Unit test
    methods(Static)
        function Result = stressTest()
            io.msgStyle(LogLevel.Test, '@start', 'AstroDb stressTest started')              
            
            % Get db connection
            Conn = db.Db.getLast();
            Q = db.DbQuery(Conn);
            Q.query('SELECT version()');
            assert(Q.ColCount == 1);
            pgver = Q.getField('version');
            io.msgLog(LogLevel.Test, 'Version: %s', pgver);
            assert(contains(pgver, 'PostgreSQL'));
            
            %HeaderTableName = 'raw_images';
            CatalogTableName = 'sources_proc_cropped';
           
            MsgLogger.setLogLevel(LogLevel.Error, 'type', 'file');            
            MsgLogger.setLogLevel(LogLevel.Test, 'type', 'disp');            
                        
            % Create catalog with column names matching all fields with data type Double
            SqlText = ['SELECT * from ', CatalogTableName, ' LIMIT 1'];
            Q.query(SqlText);
            ColNames = Q.getFieldNamesOfType('Double');                       
            Cols = numel(ColNames);
            Rows = 100*1000;            
            io.msgLog(LogLevel.Test, 'Preparing test Catalog: Rows: %d, Cols: %d', Rows, Cols);
            AC = AstroTable({rand(Rows, Cols)}, 'ColNames', ColNames);
            
            % Insert catalog to table (with default BatchSize)
            Count = 1000;
            for i=1:Count
                db.AstroDb.insertCatalog(AC, CatalogTableName);
            end
            
            io.msgStyle(LogLevel.Test, '@passed', 'AstroDb stressTest started')
            Result = true;            
        end
    end
    
    %----------------------------------------------------------------------
    % Unit test
    methods(Static)
        function Result = unitTest()
            io.msgStyle(LogLevel.Test, '@start', 'AstroDb test started')

            % Get db connection
            Conn = db.Db.getLast();
            Q = db.DbQuery(Conn);
            Q.query('SELECT version()');
            assert(Q.ColCount == 1);
            pgver = Q.getField('version');
            io.msgLog(LogLevel.Test, 'Version: %s', pgver);
            assert(contains(pgver, 'PostgreSQL'));
            
            HeaderTableName = 'raw_images';
            CatalogTableName = 'sources_proc_cropped';

            MsgLogger.setLogLevel(LogLevel.Error, 'type', 'file');            
            MsgLogger.setLogLevel(LogLevel.Test, 'type', 'disp');            
                        
            % Create catalog
            SqlText = ['SELECT * from ', CatalogTableName, ' LIMIT 1'];
            Q.query(SqlText);
            ColNames = Q.getFieldNamesOfType('Double');            
            Cols = numel(ColNames);
            Rows = 1000;                        
            io.msgLog(LogLevel.Test, 'Preparing test Catalog: Rows: %d, Cols: %d', Rows, Cols);
            AC = AstroTable({rand(Rows, Cols)}, 'ColNames', ColNames);
            
            % Insert catalog to table
            res = db.AstroDb.insertCatalog(AC, CatalogTableName);
            
            %------------------------------------------------- Prepare test data          
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
            AC = AstroTable({rand(10, 4), rand(10, 4)}, 'ColNames', {'ra', 'dec', 'sn_best', 'sn_delta'});
            
            
            % Insert catalog to table
            Count = 10;
            for i=1:Count
                res = db.AstroDb.insertCatalog(AC, CatalogTableName);
            end
            
            
            %------------------------------------------------- Insert Header
            % Insert header to table
            tic;
            Count = 10;
            for i=1:Count
                res = db.AstroDb.insertHeader(AH, HeaderTableName);
            end
            toc
               
            %------------------------------------------------- Insert Catalog
            % Insert catalog to table
            Count = 10;
            for i=1:Count
                res = db.AstroDb.insertCatalog(AC, CatalogTableName);
            end
                
            %----------------------------------------------------- Batch
            
            % Insert many with batch = 1
            Count = 0;  %1000;
            tic();
            for i=1:Count
                res = db.AstroDb.insertHeader(AH, HeaderTableName);
            end            
            Toc = toc();
            io.msgLog(LogLevel.Info, 'insertHeader time (Count=%d): %.6f', Count, Toc);  
                                
            % Insert many with batch = 1000
            Count = 0; %100;
            BatchSize = 1000;
            tic();
            for i=1:Count
                res = db.AstroDb.insertHeader(AH, HeaderTableName, 'BatchSize', BatchSize);
            end
            
            Toc = toc();
            io.msgLog(LogLevel.Info, 'insertHeader time (Count=%d, Batch=%d): %.6f', Count, BatchSize, Toc);  
                        
            
            io.msgStyle(LogLevel.Test, '@passed', 'AstroDb test passed')
            Result = true;
        end
    end    
             

end

            
