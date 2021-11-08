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
            % Constructor - @Todo - discuss corret row,col order!
            % Data: struct array, table, cell array, matrix
            arguments
                Data = [];
                Args.ColNames cell = [];  % Required when Data is Cell or Matrix
            end
            
            % Check what we need to transpose
            if ~isempty(Data)
                if isstruct(Data)
                    Obj.Data = Data;
                elseif istable(Data)
                    Obj.Data = table2struct(Data);
                elseif iscell(Data)
                    Obj.Data = cell2struct(Data, Args.ColNames, 2);
                elseif isnumeric(Data)
                    Obj.Data = cell2struct(num2cell(Data, size(Data, 1)), Args.ColNames, 2);  %numel(Args.ColNames));
                end
            end
            
        end
        
%
%             % Constructor
%             %   DbRecord()          - Create new empty record object
%             %   DbRecord(DbQuery)   - Create object linked to specified query
%
%
%             % Generate unique id, as Uuid or SerialStr (more compact and fast)
%             if Obj.UseUuid
%                 Obj.Uuid = Component.newUuid();
%             else
%                 Obj.Uuid = Component.newSerialStr('DbRecord');
%             end
%
%             % Set DbQuery
%             if numel(varargin) == 1
%                 Obj.Query = varargin{1};
%                 io.msgLog(LogLevel.Debug, 'DbRecord created: %s, DbQuery: %s', Obj.Uuid_, Obj.Query_.Uuid);
%             else
%                 io.msgLog(LogLevel.Debug, 'DbRecord created: %s', Obj.Uuid_);
%             end
%         end
        
        
        % Destructor
        function delete(Obj)
            io.msgLog(LogLevel.Debug, 'DbRecord deleted: %s', Obj.Uuid);
        end
    end

    
    methods % Main functions
        
        function Result = getFieldNames(Obj)
            % Get list of field names, properties ending with '_' are excluded
            Result = fieldnames(Obj.Data);
        end
        
        
        function Result = merge(Obj, Stru)
            % Merge struct array with current data
            % Usefull when we constructed from matrix and need key fields
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
            Result = struct2table(Obj.Data);
            Size = size(Result);
            assert(numel(Obj.Data) == Size(1));
        end

        
        function Result = convert2cell(Obj)
            % Convert record(s) to cell
            % Note that we need to transpose it
            Result = squeeze(struct2cell(Obj.Data))';
            Size = size(Result);
            assert(numel(Obj.Data) == Size(1));
        end


        function Result = convert2mat(Obj)
            % Convert record(s) to matrix, non-numeric fields are
            % Note that we need to transpose it
            Result = cell2mat(squeeze(struct2cell(Obj.Data)))';
            Size = size(Result);
            assert(numel(Obj.Data) == Size(1));
        end

        
        function Result = convert2AstroTable(Obj)
            % Convert record(s) to AstroTable
            Mat = cell2mat(squeeze(struct2cell(Obj.Data)))';
            Result = AstroTable({Mat}, 'ColNames', Obj.ColNames);
            Size = size(Result.Catalog);
            assert(numel(Obj.Data) == Size(1));
        end

        
        function Result = convert2AstroCatalog(Obj)
            % Convert record(s) to AstroCatalog
            Mat = cell2mat(squeeze(struct2cell(Obj.Data)))';
            Result = AstroCatalog({Mat}, 'ColNames', Obj.ColNames);
            Size = size(Result.Catalog);
            assert(numel(Obj.Data) == Size(1));
        end
           
        
        function Result = writeCsv(Obj)
            
        end
        
    end
        
    %----------------------------------------------------------------------
    methods(Static) % Unit test
                         
        Result = unitTest()
            % Unit-Test
            
    end
        
end
