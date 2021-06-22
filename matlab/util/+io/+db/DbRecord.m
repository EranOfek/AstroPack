% Database record

%--------------------------------------------------------------------------

classdef DbRecord < dynamicprops
    
    % Properties
    properties (SetAccess = public)
        Query_       = []        % Linked DbQuery
        KeyField_    = ''        % Key field(s)
        Uuid_        = ''        % Used when UseUuid is true
        UseUuid_     = false;    % 
    end
    
    %-------------------------------------------------------- 
    methods % Constructor            
        function Obj = DbRecord(varargin)           
            
            % General unique id, as Uuid or SerialStr (more compact)
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
                else
                    io.msgLog(LogLevel.Error, 'DbRecod.loadStruct: Field type not supported: %s', Field);                    
                end        
            end
            Result = true;
        end        

        
        function Struct = getStruct(Obj)
            % Load all struct fields to properties
            
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
            % Load all struct fields to properties
            
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
            %
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
                         
        function Result = unitTest()
            % Unit test
            io.msgStyle(LogLevel.Test, '@start', 'DbRecord test started');
      
            S.MyX = 1;
            S.MyY = 2;
            S.MyZ = 3;
            R = io.db.DbRecord;
            R.loadStruct(S);
            assert(R.MyX == S.MyX);
            assert(R.MyY == S.MyY);
            assert(R.MyX ~= S.MyY);
            
            %
            Q = io.db.DbRecord;
            Q.loadStruct(S);
            assert(R.Equal(Q));
            
            % Done
            io.msgStyle(LogLevel.Test, '@passed', 'DbRecord test passed');
            Result = true;
        end
    end
        
end

