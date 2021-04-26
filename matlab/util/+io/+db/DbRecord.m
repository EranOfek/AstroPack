% Database record

%--------------------------------------------------------------------------

classdef DbRecord < dynamicprops
    
    % Properties
    properties (SetAccess = public)
        Query = []
        KeyField = ''
    end
    
    %-------------------------------------------------------- 
    methods % Constructor            
        function Obj = DbRecord(varargin)           
            
            io.msgLog(LogLevel.Debug, 'DbRecord created');
            
            if numel(varargin) == 1
                Obj.Query = varargin{1};
            end
            
            
        end
        
        
        % Destructor
        function delete(Obj)
            io.msgLog(LogLevel.Debug, 'DbRecord deleted');
        end
    end

    
    methods % Main functions
    
        function loadFile(Obj, FileName)
            % Load specified file to property            
            
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
        
 
        function Result = loadStruct(Obj, Stru)
            % Load all struct fields
            
            FieldNames = fieldnames(Stru);
            for i = 1:numel(FieldNames)
                Field = FieldNames{i};
                
                % int, double, char, bool, string
                if isnumeric(Stru.(Field)) || ischar(Stru.(Field)) || ...
                   islogical(Stru.(Field)) || isstring(Stru.(Field))
                    if ~isprop(Obj, Field)
                        Obj.addprop(Field);
                    end
                    Obj.(Field) = Stru.(Field);
                else
                    io.msgLog(LogLevel.Error, 'DbRecod.loadStruct: Field type not supported: %s', Field);                    
                end        
            end
            Result = true;
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
            io.msgLog(LogLevel.Test, 'DbRecord test started');
      
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
            io.msgLog(LogLevel.Test, 'DbRecord test passed');
            Result = true;
        end
    end
        
end

