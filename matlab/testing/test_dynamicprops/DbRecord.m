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

classdef DbRecord < dynamicprops
    
    % Properties
    properties (SetAccess = public)
        Name_        = 'DbRecord'
        Query_       = []           % Linked DbQuery
        KeyField_    = ''           % Key field(s)
        Uuid_        = ''           % Used when UseUuid is true
        UseUuid_     = false        % True to use Uuid, otherwise the faster SerialStr is used
    end
    
    %--------------------------------------------------------
    methods % Constructor
        function Obj = DbRecord(varargin)
            % Constructor
            %   DbRecord()          - Create new empty record object
            %   DbRecord(DbQuery)   - Create object linked to specified query
            
            
            % Generate unique id, as Uuid or SerialStr (more compact and fast)
            if Obj.UseUuid_
                Obj.Uuid_ = Component.newUuid();
            else
                Obj.Uuid_ = Component.newSerialStr('DbRecord');
            end
            
            % Set DbQuery
            if numel(varargin) == 1
                Obj.Query_ = varargin{1};
                io.msgLog(LogLevel.Debug, 'DbRecord created: %s, DbQuery: %s', Obj.Uuid_, Obj.Query_.Uuid);
            else
                io.msgLog(LogLevel.Debug, 'DbRecord created: %s', Obj.Uuid_);
            end
        end
        
        
        % Destructor
        function delete(Obj)
            io.msgLog(LogLevel.Debug, 'DbRecord deleted: %s', Obj.Uuid_);
        end
    end

    
    methods % Main functions
    
        function loadFile(Obj, FileName)
            % Load specified file to property
            % @Todo - not implemented yet
            try
                [~, name, ~] = fileparts(FileName);
                PropName = name;
                if isprop(Obj, PropName)
                %if isfield(Obj.Data, PropName)
                    io.msgLog(LogLevel.Warning, 'Property already exist: %s', PropName);
                else
                    io.msgLog(LogLevel.Info, 'Adding property: %s', PropName);
                    %Obj.addprop(PropName);
                end
      
            catch
                io.msgLog(LogLevel.Error, 'loadFile: Exception: %s', FileName);
            end
        end
        
 
        function Result = loadStruct(Obj, Struct)
            % Load all struct fields to properties
            
            % Iterate all struct fields
            FieldNames = fieldnames(Struct);
            for i = 1:numel(FieldNames)
                Field = FieldNames{i};
                
                % int, double, char, bool, string
                if isnumeric(Struct.(Field)) || ischar(Struct.(Field)) || ...
                   islogical(Struct.(Field)) || isstring(Struct.(Field))
                    if ~isprop(Obj, Field)
                        Obj.addprop(Field);
                    end
                    Obj.(Field) = Struct.(Field);
                    
                % Not supported
                else
                    io.msgLog(LogLevel.Error, 'DbRecod.loadStruct: Field type not supported: %s', Field);
                end
            end
            Result = true;
        end

        
        function Struct = getStruct(Obj)
            % Return new struct with field values
            % Field names ending with '_' are ignored
            
            Struct = struct;
            PropNames = properties(Obj);
            for i = 1:numel(PropNames)
                Prop = PropNames{i};
                if ~endsWith(Prop, '_')
                    Struct.(Prop) = Obj.(Prop);
                end
            end
        end
 

        function Result = getFieldNames(Obj)
            % Get list of field names, properties ending with '_' are excluded
            
            Result = {};
            PropNames = properties(Obj);
            for i = 1:numel(PropNames)
                Prop = PropNames{i};
                if ~endsWith(Prop, '_')
                    Result{end+1} = Prop;
                end
            end
        end
        
                        
        function addProp(Obj, Prop, Value)
            % Add new property with value
            addprop(Obj, Prop);
            Obj.(Prop) = Value;
        end
        
        
        function Result = Equal(Obj, Other)
            % Compare two records, return true if equal
            
            Result = false;
            Props = properties(Obj);
            Others = properties(Other);
            if numel(Props) == numel(Others)
                Result = true;
                for i = 1:numel(Props)
                    Prop = Props{i};
                    if isprop(Other, Prop)
                        if Obj.(Prop) ~= Other.(Prop)
                            Result = false;
                            break;
                        end
                    else
                        Result = false;
                        break;
                    end
                end
            end
        end
        
    end
    
    %----------------------------------------------------------------------
    methods(Static) % Unit test
                         
        Result = unitTest()

    end
        
end
