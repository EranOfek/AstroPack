% AstroImage database adaptor
%
% Usage: Use the static functions
%
%     db.AstroDb.insertHeader
%     db.AstroDb.insertCatalog
%     db.AstroDb.getDefaultQuery
%
%
% Threads - For optional future use
%
% https://undocumentedmatlab.com/articles/explicit-multi-threading-in-matlab-part1
% https://undocumentedmatlab.com/articles/explicit-multi-threading-in-matlab-part2

% #functions (autogen)
% AstroDb -
% get -
% getDefaultQuery - Get default database query
% insertCatalog - Insert AstroCatalog / AstroTable to the specified database table Note that AstroCatalog is derived from AstroTable
% insertCatalogImpl - Internal implementations Insert AstroCatalog / AstroTable to the specified database table Note that AstroCatalog is derived from AstroTable
% insertHeader - % Insert AstroHeader/AstroImage object to the specified database table             arguments                 Input                           % AstroHeader / AstroImage                 TableName char                 Args.Fields = {}                % As
% insertHeaderImpl - Internal implementation Insert AstroHeader/AstroImage object to the specified database table
% manage - @Todo: Manage queue of pending operations
% perfTest -
% stressTest -
% #/functions (autogen)
%

classdef AstroDb < Component
    
    properties (Hidden, SetAccess = public)
        OperQueue       % Queue of AstroDbOper, future use
    end
    

    methods % Constructor
        
        function Obj = AstroDb
            Obj.setName('AstroDb')
        end

    end
    
    
    methods % Internal implementations, do not use directly
               
        function Result = insertHeaderImpl(Obj, Input, TableName, Args)
            % Internal implementation
            % Insert AstroHeader/AstroImage object to the specified database table
            arguments
                Obj
                Input               % AstroHeader / AstroImage
                TableName char      % Table name
                Args.Fields = {}    % As
                Args.Uuid = []      % Empty uses AstroHeader.Uuid
                Args.Query = []     % db.DbQuery
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
            % Internal implementations
            % Insert AstroCatalog / AstroTable to the specified database table
            % Note that AstroCatalog is derived from AstroTable
            
            % Values in AstroCatalog.Catalog = [];
            % Field names in AstroCatalog.ColNames cell = {};
            arguments
                Obj
                Input                   % AstroHeader / AstroImage
                TableName char          %
                Args.KeyField = 'proc_iid'
                Args.Fields = {}        % As
                Args.Uuid = []          % Empty uses AstroHeader.Uuid
                Args.Query = []         % db.DbQuery
                Args.BatchSize = 1000   % Insert batch size
            end
                        
            Obj.msgLog(LogLevel.Debug, 'insertCatalogImpl started');
            KeyField = Args.KeyField;
            
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
    methods % Manager, caller from timer
        function Result = manage(Obj)
            % @Todo: Manage queue of pending operations

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
    % Performance Test
    methods(Static)
        function Result = perfTest()
            io.msgStyle(LogLevel.Test, '@start', 'AstroDb perfTest started')

            Cols = 40;
            ColNames = { 'recid' };
            for i = 1:Cols
                if i < Cols
                    ColNames{end+1} = sprintf('F%02d', i);  % = strcat(ColNames,
                else
                    ColNames{end+1} = sprintf('F%02d', i);  % = strcat(ColNames,
                end
            end
            %ColNames = strip(ColNames);
            Cols = numel(ColNames);
           
            Rows = 10;
            for Iter=1:5
                
                data = rand(Rows, Cols);
                
                % Create random table
                io.msgLog(LogLevel.Test, 'Preparing rand Catalog: Rows: %d, Cols: %d', Rows, Cols);
                tic();
                AC = AstroTable({data}, 'ColNames', ColNames);
                T = toc();
                io.msgLog(LogLevel.Test, 'Preparing rand Catalog: Rows: %d, Cols: %d: = %.4f', Rows, Cols, T);
                
                % Set uuid
                %for j = 1:AC.
                
                %
                tic();
                FileName = sprintf('c:\\temp\\Cat-%d.csv', Rows);
                AC.csvWrite(FileName);
                T = toc();
                io.msgLog(LogLevel.Test, 'csvWrite: Rows: %d, Cols: %d = %.4f', Rows, Cols, T);
                
                Rows = Rows * 10;
            end
                        
            io.msgStyle(LogLevel.Test, '@passed', 'AstroDb perfTest done')
            Result = true;
        end
    end
    
    %----------------------------------------------------------------------
    % Stress Test
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
            
            io.msgStyle(LogLevel.Test, '@passed', 'AstroDb stressTest done')
            Result = true;
        end
    end
    
    %----------------------------------------------------------------------
    % Unit test
    methods(Static)
        Result = unitTest()
    end
             
end
        
