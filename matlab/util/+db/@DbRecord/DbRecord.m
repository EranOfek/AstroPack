% Database record with dynamic properties
% Similar to struct, but based on dynamicprops class
% Used by DbQuery with select and insert SQL operations.

% #functions (autogen)
% DbRecord - Constructor DbRecord()          - Create new empty record object DbRecord(DbQuery)   - Create object linked to specified query
% Equal - Compare two records, return true if equal
% addProp - Add new property with value
% delete -
% getFieldNames - Get list of field names, properties ending with '_' are excluded
% getStruct - Return new struct with field values Field names ending with '_' are ignored
% loadFile - Load specified file to property @Todo - not implemented yet
% loadStruct - Load all struct fields to properties
% #/functions (autogen)
%

classdef DbRecord < Base
    
    % Properties
    properties (SetAccess = public)
        Name         = 'DbRecord'
        Query        = []           % Linked DbQuery
        KeyField     = ''           % Key field(s)
        Uuid         = ''           % Used when UseUuid is true
        UseUuid      = false;       % True to use Uuid, otherwise the faster SerialStr is used
        
        ColCount     = 0;           % Number of columns
        ColNames     = [];          % cell
        ColType      = [];          % cell
        
        Data struct                 % Array of data struct per table row
    end
    
    %--------------------------------------------------------
    methods % Constructor
        function Obj = DbRecord(Data, Args)
            % Constructor
            % Data: struct array, table, cell array, matrix
            arguments
                Data = [];
                Args.ColNames cell = [];  % Required when Data is Cell or Matrix
            end
            
            if ~isempty(Data)
                if isstruct(Data)
                    Obj.Data = Data;
                elseif istable(Data)
                    Obj.Data = table2struct(Data);
                elseif iscell(Data)
                    Obj.Data = cell2struct(Data, Args.ColNames, 1);
                elseif isnumeric(Data)
                    Obj.Data = cell2struct(num2cell(Data, size(Data, 1)), Args.ColNames, 1);
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
        
                        
        function Result = newKey(Obj)
            % Generate unique id, as Uuid or SerialStr (more compact and fast)            
            if Obj.UseUuid
                Result = Component.newUuid();
            else
                Result = Component.newSerialStr('DbRecord');
            end
        end         
        
    end
    
    
    methods % Convert
                                  
        function Result = convert2table(Obj)
            % Convert record(s) to table
            Result = struct2table(Obj.Data);
        end

        
        function Result = convert2cell(Obj)
            % Convert record(s) to cell
            Result = squeeze(struct2cell(Obj.Data));
        end


        function Result = convert2mat(Obj)
            % Convert record(s) to matrix, non-numeric fields are 
            Result = cell2mat(squeeze(struct2cell(Obj.Data)));
        end

        
        function Result = convert2AstroTable(Obj)
            % Convert record(s) to AstroTable
            Mat = cell2mat(squeeze(struct2cell(Obj.Data)));
            Result = AstroTable({Mat}, 'ColNames', Obj.ColNames)
        end      

        
        function Result = convert2AstroCatalog(Obj)
            % Convert record(s) to AstroCatalog
            Mat = cell2mat(squeeze(struct2cell(Obj.Data)));
            Result = AstroCatalog({Mat}, 'ColNames', Obj.ColNames)
        end      
                        
    end
    
    %----------------------------------------------------------------------
    methods(Static) % Unit test
                         
        Result = unitTest()
            % Unit-Test
            
    end
        
end
