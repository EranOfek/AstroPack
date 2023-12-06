% File:    DbRecord.m
% Class:   DbRecord
% Title:   Data container that holds struct array of database table rows.
% Author:  Chen Tishler
% Created: July 2021
%--------------------------------------------------------------------------
% Description:
%
% DbRecord - Data container that holds struct array of database table data.
% Used with DbQuery.insert(), select(), etc.
% Construct DbRecord before calling DbQuery.insert().
%--------------------------------------------------------------------------
%#docgen
%
% Methods:
%    DbRecord - Constructor Input:   Data          - struct array, table, cell array, matrix,                          AstroTable, AstroCatalog, AstroHeader                          If type is char, load data from CSV file, header                          line with field names is required
%    convert2 - Convert Obj.Data struct array to given OutType Input:   OutType: 'table', 'cell', 'mat', 'astrotable', 'astrocatalog',           'astroheader' Output:  Table/Cell-array/Matrix/AstroTable/AstroCatalog Example: Mat = Obj.conevrt2('mat')
%    convert2AstroCatalog - Convert record(s) to AstroCatalog Input:   - Output:  AstroCatalog object Example: AC = Obj.convert2AstroCatalog()
%    convert2AstroHeader - Convert record(s) to AstroCatalog Input:   - Output:  AstroCatalog object Example: AC = Obj.convert2AstroCatalog()
%    convert2AstroTable - Convert record(s) to AstroTable Input:   - Output:  AstroTable object Example: AT = Obj.convert2AstroTable()
%    convert2cell - Convert record(s) to cell Note that we need to transpose it Input:   - Output:  Cell-array Example: Cell = Obj.convert2cell()
%    convert2mat - Convert record(s) to matrix, non-numeric fields are Note that we need to transpose it Input:   - Output:  Matrix Example: Mat = Obj.convert2mat()
%    convert2table - Convert record(s) to table Input:   - Output:  Table Example: Tab = Obj.convert2table()
%    delete - Destructor io.msgLog(LogLevel.Debug, 'DbRecord deleted: %s', Obj.Uuid);
%    getFieldNames - Get list of field names, properties ending with '_' are excluded Input:   - Output:  cell-array of field-names Example: FieldNames = Obj.getFieldNames()
%    getRowCount - Get numer of rows in Data struct array Input:   - Output:  Number of rows in Obj.Data Example: Count = Obj.getRowCount()
%    merge - Merge input struct array with current data
%    newKey - Generate unique id, as Uuid or SerialStr (more compact and fast) Input:   - Output:  New Uuid or SerialStr Example: Key = Obj.newKey()
%    readCsv - Read from CSV file to Obj.Data struct-array @Todo - Use MEX optimized version? (need to develop) Input:   - FileName - CSV input file name Output:  - true on success, data will be loaded to Obj.Data Example: - Rec.readCsv('/tmp/data1.csv')
%    writeCsv - Write Obj.Data struct array to CSV file @Todo - Use MEX optimized version, (to be complted) Input:   FileName     - CSV output fle name Output:  true on sucess Example: Obj.writeCsv('/tmp/data1.csv', 'Header', @TBD)
%
%#/docgen

classdef DbRecord < Base
    
    % Properties
    properties (SetAccess = public)
        Name         = 'DbRecord'   % Object name
        Query        = []           % Linked DbQuery (optional)
        KeyField     = ''           % Key field(s)
        UseUuid      = true;        % True to use Uuid, otherwise SerialStr() is used
        ColCount     = 0;           % Number of columns
        ColNames     = [];          % cell - Field names
        ColType      = [];          % cell - Field data types
        Data struct                 % Array of data struct per table row
    end
    
    %--------------------------------------------------------
    methods % Constructor
        function Obj = DbRecord(Data, Args)
            % Constructor
            % Input   : - struct array, table, cell array, matrix,
            %             AstroTable, AstroCatalog, AstroHeader.
            %             CSV: If type is char, it specifies file name.
            %                  Load data from CSV file, header line with 
            %                  field names is required.
            %           * Pairs of ...,key,val,...
            %             The following keys are available:            			            
            %             'ColNames' - list of column names, char comma separated, or cell
            %                          array. Required when Data is Cell or Matrix.
            % Output  : - New instance of DbRecord object
            % Author  : Chen Tishler (2021)
            % Example : MyRec = db.DbRecord(Mat, 'FieldA,FieldB');
            %           MyRec = db.DbRecord('csvfile.csv');
            arguments
                Data = [];              % Input data
                Args.ColNames = [];     % Required when Data is Cell or Matrix
                Args.Lowercase = true;  %
            end
            
            if ischar(Args.ColNames)
                Args.ColNames = strip(strsplit(Args.ColNames, ','));
            end
            
            % Load data
            if ~isempty(Data)
                
                % Load from CSV file
                if ischar(Data)
                    Obj.Data = table2struct(io.files.readtable1(Data));
                    
                % Load from memory data
                elseif isstruct(Data)
                    Obj.Data = Data;
                elseif istable(Data)
                    Obj.Data = table2struct(Data);
                elseif iscell(Data)
                    Obj.Data = cell2struct(Data, Args.ColNames, 2);
                elseif isnumeric(Data)
                    % @Perf - Need to be improved, it works very slow with arrays > 10,000
                    Obj.Data = cell2struct(num2cell(Data, size(Data, 1)), Args.ColNames, 2);  %numel(Args.ColNames));
                elseif isa(Data, 'AstroTable') || isa(Data, 'AstroCatalog')
                    if numel(Data) == 1
                        Tab = Data.array2table();
                        Obj.Data = table2struct(Tab.Catalog);
                        
                        % Validate conversion
                        Sz = size(Tab.Catalog);
                        assert(Sz(1) == numel(Obj.Data));
                        assert(Sz(2) == numel(fieldnames(Obj.Data(1))));
                    else
                        error('DbRecord currently supports only single AstroTable/AstroCatalog');
                    end
                elseif isa(Data, 'AstroHeader')
                    % Load from AstroHeader
                    if numel(Data) == 1
                        Data = Data.Data;
                        Sz = size(Data);
                        Rows = Sz(1);
                        Stru = struct;
                        for Row=1:Rows
                            Key = Data{Row, 1};
                            Value = Data{Row, 2};                            
                            
                            % Convert key to lowercase and replace invalid chars with '_'
                            if ischar(Key)
                                if Args.Lowercase
                                    Key = lower(Key);
                                end
                                
                                Key = replace(Key, '-', '_');                                                        
                                Stru.(Key) = Value;
                            end
                        end
                        Obj.Data = Stru;
                    else
                        error('DbRecord currently supports only single AstroHeader');
                    end
                end
            end
            
        end
      
        
        function delete(Obj)
            % Destructor
            % Internally called by Matlab when the object is destroyed.
            %io.msgLog(LogLevel.Debug, 'DbRecord deleted: %s', Obj.Uuid);
        end
    end

    
    methods % Main functions
        
        function Result = getFieldNames(Obj)
            % Get list of field names of the currently stored struct array.
            % Properties ending with '_' are excluded (they are used internally).
            % Input   : - DbRecord object
            % Output  : - cell-array of field-names
            % Author  : Chen Tishler (2021)
            % Example : FieldNames = Obj.getFieldNames()
            Result = fieldnames(Obj.Data);
        end
        
        
        function merge(Obj, Stru)
            % Merge input struct array with current data.
            % Usefull when we constructed DbRecrord from matrix and need string key field.
            % Input   : - DbRecord object
            %           Stru - Struct array to merge into Obj.Data
            % Output  : -
            % Author  : Chen Tishler (2021)
            % Example : Obj.merge(MyStructArray)
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
        end
        
        
        function Result = newKey(Obj)
            % Generate unique id, as Uuid or SerialStr (more compact and fast).
            % See Component.newUuid() and Component.newSerialStr()
            % Input   : - DbRecord object
            % Output  : - New Uuid or SerialStr (char)
            % Author  : Chen Tishler (2021)
            % Example : Key = Obj.newKey()
            if Obj.UseUuid
                Result = Component.newUuid();
            else
                Result = Component.newSerialStr('DbRecord');
            end
        end
        
    end
    
    
    methods % Convert2... - Conversion functions
                                  
        function Result = convert2table(Obj)
            % Convert Obj.Data to table
            % Input   : - DbRecord object
            % Output  : - Table
            % Author  : Chen Tishler (2021)
            % Example : Tab = Obj.convert2table()
            if ~isempty(Obj.Data)
                Result = struct2table(Obj.Data);
                Size = size(Result);
                assert(numel(Obj.Data) == Size(1));
            else
                Result = [];
            end
        end

        
        function Result = convert2cell(Obj)
            % Convert Obj.Data to cell array
            % Note that we need to transpose the output of struct2cell().
            % Input   : - DbRecord object
            % Output  : - Cell-array
            % Author  : Chen Tishler (2021)
            % Example : Cell = Obj.convert2cell()
            if ~isempty(Obj.Data)
                Result = squeeze(struct2cell(Obj.Data))';
                Size = size(Result);
                assert(numel(Obj.Data) == Size(1));
            else
                Result = [];
            end
        end


        function Result = convert2mat(Obj)
            % Convert Obj.Data to matrix, fails if there are non-numeric fields.
            % Note that we need to transpose the output of struct2cell().
            % Input   : - DbRecord object
            % Output  : - Matrix
            % Author  : Chen Tishler (2021)
            % Example : Mat = Obj.convert2mat()
            if ~isempty(Obj.Data)
                Result = cell2mat(squeeze(struct2cell(Obj.Data)))';
                Size = size(Result);
                assert(numel(Obj.Data) == Size(1));
            else
                Result = [];
            end
        end

        
        function Result = convert2AstroTable(Obj)
            % Convert Obj.Data to AstroTable
            % Input   : - DbRecord object
            % Output  : - AstroTable object
            % Author  : Chen Tishler (2021)
            % Example : AT = Obj.convert2AstroTable()
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
            % Convert Obj.Data to AstroCatalog
            % Input   : - DbRecord object
            % Output  : - AstroCatalog object
            % Author  : Chen Tishler (2021)
            % Example : AC = Obj.convert2AstroCatalog()
            if ~isempty(Obj.Data)
                Mat = cell2mat(squeeze(struct2cell(Obj.Data)))';
                Result = AstroCatalog({Mat}, 'ColNames', Obj.ColNames);
                Size = size(Result.Catalog);
                assert(numel(Obj.Data) == Size(1));
            else
                Result = [];
            end
        end
           
        
        function Result = convert2AstroHeader(Obj)
            % Convert Obj.Data to AstroCatalog
            % Input   : - DbRecord object
            % Output  : - AstroCatalog object
            % Author  : Chen Tishler (2021)
            % Example : AC = Obj.convert2AstroCatalog()
            if ~isempty(Obj.Data)
                for n=1:numel(Obj.Data)
                    Result(n) = AstroHeader();
                    for i=1:numel(Obj.ColNames)
                        Comment = '';
                        Result.insertKey({ Obj.ColNames{i}, Obj.Data(n).(Obj.ColNames{i}), Comment}, 'end-1');
                    end
                end
            else
                Result = [];
            end
        end
        
        
        function Result = convert2(Obj, OutType)
            % Convert Obj.Data to given OutType
            % Input   : - DbRecord object
            %           - OutType: 'table', 'cell', 'mat', 'astrotable', 'astrocatalog',
            %           'astroheader'
            % Output  : - Table/Cell-array/Matrix/AstroTable/AstroCatalog
            % Author  : Chen Tishler (2021)
            % Example : Mat = Obj.conevrt2('mat')
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
            elseif strcmp(OutType, 'astroheader')
                Result = Obj.convert2AstroHeader();
            else
                error('convert2: unknown output type: %s', OutType);
            end
        end
                    
                        
        function Result = writeCsv(Obj, FileName)
            % Write Obj.Data to CSV file.
            % @Todo - Use MEX optimized version, (to be completed)
            % Input   : - DbRecord object
            %           - FileName - CSV output fle name
            % Output  : - true on success
            % Author  : Chen Tishler (2021)
            % Example : Obj.writeCsv('/tmp/data1.csv')
            if ~isempty(Obj.Data)
                Table = struct2table(Obj.Data);
            else
                Table = table;
            end
            writetable(Table, FileName);
            Result = true;
            
            % @Todo: Use MEX version which is x30 faster than MATLAB version
            %mex_WriteMatrix2(FileName, Rec.Data, '%.5f', ',', 'w+', Args.Header, Obj.Data);
        end
        
        
        function Result = readCsv(Obj, FileName)
            % Read CSV file to Obj.Data struct-array
            % @Todo - Use MEX optimized version? (need to develop)
            % Input   : - DbRecord object
            %           - FileName - CSV input file name
            % Output  : - true on success, data will be loaded to Obj.Data
            % Author  : Chen Tishler (2021)
            % Example : Rec.readCsv('/tmp/data1.csv')
            Table = io.files.readtable1(FileName);
            Obj.Data = table2struct(Table);
            Result = true;
        end
        
        
        function Result = getRowCount(Obj)
            % Get number of rows in Obj.Data struct array
            % Input   : - DbRecord object
            % Output  : - Number of rows in Obj.Data
            % Author  : Chen Tishler (2021)
            % Example : Count = Obj.getRowCount()
            Result = numel(Obj.Data);
        end
            
    end
        
    %----------------------------------------------------------------------
    methods(Static) % Unit test
                         
        Result = unitTest()
            % Unit-Test
            
    end
        
end
