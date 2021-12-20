% Database record with dynamic properties
% Similar to struct, but based on dynamicprops class
% Used by DbQuery with select and insert SQL operations.

% #functions (autogen)
% DbRecord - Constructor - @Todo - discuss corret row,col order! Data: struct array, table, cell array, matrix
% convert2AstroCatalog - Convert record(s) to AstroCatalog
% convert2AstroTable - Convert record(s) to AstroTable
% convert2cell - Convert record(s) to cell
% convert2mat - Convert record(s) to matrix, non-numeric fields are
% convert2table - Convert record(s) to table
% delete -
% getFieldNames - Get list of field names, properties ending with '_' are excluded
% merge - Merge struct array with current data Usefull when we constructed from matrix and need key fields
% newKey - Generate unique id, as Uuid or SerialStr (more compact and fast)
% #/functions (autogen)
%

classdef DbRecord < Base
    
    % Properties
    properties (SetAccess = public)
        Name         = 'DbRecord'
        Query        = []           % Linked DbQuery
        KeyField     = ''           % Key field(s)
        Uuid         = ''           % Used when UseUuid is true
        UseUuid      = true;        % True to use Uuid, otherwise the faster SerialStr is used
        
        ColCount     = 0;           % Number of columns
        ColNames     = [];          % cell
        ColType      = [];          % cell
        
        Data struct                 % Array of data struct per table row
    end
    
    %--------------------------------------------------------
    methods % Constructor
        function Obj = DbRecord(Data, Args)
            % Constructor
            % Input:   Data - struct array, table, cell array, matrix, AstroTable, AstroCatalog
            % Example: MyRec = db.DbRecord(
            arguments
                Data = [];
                Args.ColNames = [];  % Required when Data is Cell or Matrix
            end
            
            if ischar(Args.ColNames)
                Args.ColNames = strsplit(Args.ColNames, ',');
            end
            
            % Check what we need to transpose
            if ~isempty(Data)
                if ischar(Data)
                    Obj.Data = table2struct(readtable(Data));
                elseif isstruct(Data)
                    Obj.Data = Data;
                elseif istable(Data)
                    Obj.Data = table2struct(Data);
                elseif iscell(Data)
                    Obj.Data = cell2struct(Data, Args.ColNames, 2);
                elseif isnumeric(Data)
                    
                    % @Perf - Need to be improved, it works very slow with
                    % arrays > 10,000
                    Obj.Data = cell2struct(num2cell(Data, size(Data, 1)), Args.ColNames, 2);  %numel(Args.ColNames));
                elseif isa(Data, 'AstroCatalog')
                    % Obj.Data = @Todo
                elseif isa(Data, 'AstroTable')
                    % Obj.Data = @Todo
                end
            end
            
        end
      
        
        function delete(Obj)
            % Destructor            
            %io.msgLog(LogLevel.Debug, 'DbRecord deleted: %s', Obj.Uuid);
        end
    end

    
    methods % Main functions
        
        function Result = getFieldNames(Obj)
            % Get list of field names, properties ending with '_' are excluded
            % Input:   -
            % Output:  cell-array of field-names
            % Example: FieldNames = Obj.getFieldNames()
            Result = fieldnames(Obj.Data);
        end
        
        
        function Result = merge(Obj, Stru)
            % Merge struct array with current data
            % Usefull when we constructed from matrix and need key fields
            % Input:   Stru - 
            % Output:  -
            % Example: Merged = Obj.merge(MyStructArray)
            FieldList = fieldnames(Stru);
            StruRows = numel(Stru);
            for Row=1:numel(Obj.Data)
                for Field=1:numel(FieldList)
                    FieldName = FieldList{Field};
                    if Row <= StruRows
                        Obj.Data(Row).(FieldName) = Stru(Row).(FieldName);
                    else
                        Obj.Data(Row).(FieldName) = Stru(StruRows).(FieldName);
                    end
                end
            end
            Result = true;
            
        end
        
        
        function Result = newKey(Obj)
            % Generate unique id, as Uuid or SerialStr (more compact and fast)
            % Input:   - 
            % Output:  -
            % Example: -            
            if Obj.UseUuid
                Result = Component.newUuid();
            else
                Result = Component.newSerialStr('DbRecord');
            end
        end
        
    end
    
    
    methods % Convert2...
                                  
        function Result = convert2table(Obj)
            % Convert record(s) to table
            % Input:   - 
            % Output:  Table
            % Example: Tab = Obj.convert2table()       
            if ~isempty(Obj.Data)
                Result = struct2table(Obj.Data);
                Size = size(Result);
                assert(numel(Obj.Data) == Size(1));
            else
                Result = [];
            end
        end

        
        function Result = convert2cell(Obj)
            % Convert record(s) to cell
            % Note that we need to transpose it
            % Input:   - 
            % Output:  Cell-array
            % Example: Cell = Obj.convert2cell()
            if ~isempty(Obj.Data)            
                Result = squeeze(struct2cell(Obj.Data))';
                Size = size(Result);
                assert(numel(Obj.Data) == Size(1));
            else
                Result = [];
            end
        end


        function Result = convert2mat(Obj)
            % Convert record(s) to matrix, non-numeric fields are
            % Note that we need to transpose it
            % Input:   - 
            % Output:  Matrix
            % Example: Mat = Obj.convert2mat()
            if ~isempty(Obj.Data)            
                Result = cell2mat(squeeze(struct2cell(Obj.Data)))';
                Size = size(Result);
                assert(numel(Obj.Data) == Size(1));
            else
                Result = [];
            end                
        end

        
        function Result = convert2AstroTable(Obj)
            % Convert record(s) to AstroTable
            % Input:   - 
            % Output:  AstroTable object
            % Example: AT = Obj.convert2AstroTable()
            if ~isempty(Obj.Data)            
                Mat = cell2mat(squeeze(struct2cell(Obj.Data)))';
                Result = AstroTable({Mat}, 'ColNames', Obj.ColNames);
                Size = size(Result.Catalog);
                assert(numel(Obj.Data) == Size(1));
            else
                Result = [];
            end
        end

        
        function Result = convert2AstroCatalog(Obj)
            % Convert record(s) to AstroCatalog
            % Input:   - 
            % Output:  AstroCatalog object
            % Example: AC = Obj.convert2AstroCatalog()            
            if ~isempty(Obj.Data)            
                Mat = cell2mat(squeeze(struct2cell(Obj.Data)))';
                Result = AstroCatalog({Mat}, 'ColNames', Obj.ColNames);
                Size = size(Result.Catalog);
                assert(numel(Obj.Data) == Size(1));
            else
                Result = [];
            end            
        end
           

        function Result = convert2(Obj, OutType)                  
            % Convert Obj.Data struct array to given OutType
            % Input:   OutType: 'table', 'cell', 'mat', 'astrotable', 'astrocatalog'
            % Output:  Table/Cell-array/Matrix/AstroTable/AstroCatalog
            % Example: Mat = Obj.conevrt2('mat')
            OutType = lower(OutType);
            if strcmp(OutType, 'table')
                Result = Obj.convert2table();
            elseif strcmp(OutType, 'cell')
                Result = Obj.convert2cell();
            elseif strcmp(OutType, 'mat')
                Result = Obj.convert2mat();
            elseif strcmp(OutType, 'astrotable')
                Result = Obj.convert2AstroTable();
            elseif strcmp(OutType, 'astrocatalog')
                Result = Obj.convert2AstroCatalog();
            else
                error('convert2: unknown output type: %s', OutType);
            end
        end
                    
                        
        function Result = writeCsv(Obj, FileName, Args)            
            % Write Obj.Data struct array to CSV file, using mex optimization
            % @Todo - to be tested
            % Input:   FileName     -
            %          Args.Header  - 
            % Output:  true on sucess
            % Example: Obj.writeCsv('/tmp/data1.csv', 'Header', @TBD)            
            arguments
                Obj
                FileName            % File name
                Args.Header         % Header, @TBD
            end
            
            % Use MEX version which is x30 faster than MATLAB version
            mex_WriteMatrix2(FileName, Rec.Data, '%.5f', ',', 'w+', Args.Header, Obj.Data);
            Result = true;
        end
        
        
        function Result = readCsv(Obj, FileName)
            % Read from CSV file to Obj.Data struct-array
            % @Todo - Not implemented yet
            % Input:   - FileName - CSV file name
            % Output:  - @TBD
            % Example: - CsvData = Obj.readCsv('/tmp/data1.csv')
            Result = [];          
        end        
        
        
        function Result = getRowCount(Obj)
            % Get numer of rows in Data struct array
            % Input:   - 
            % Output:  Number of rows in Obj.Data
            % Example: Count = Obj.getRowCount()
            Result = numel(Obj.Data);
        end
            
    end
        
    %----------------------------------------------------------------------
    methods(Static) % Unit test
                         
        Result = unitTest()
            % Unit-Test
            
    end
        
end
