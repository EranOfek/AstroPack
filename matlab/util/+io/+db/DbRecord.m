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
            if numel(varargin) == 1
                Obj.Query = varargin{1};
            end
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
                        Obj.addprop(Field)
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
            
            Result = true;
            Props = properties(Obj);
            for i = 1:numel(Props)
                Prop = Props{i};
                if Other.isprop(Prop)
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
    
    %----------------------------------------------------------------------   
    methods(Static) % Unit test
        
%         function Result = unitTest()
%             try
%                 Result = Configuration.doUnitTest();
%             catch
%                 Result = false;
%                 io.msgLog(LogLevel.Error, 'unitTest: Exception');
%             end
%         end
        
            
        function Result = unitTest()
            io.msgLog(LogLevel.Test, 'DbRecord test started');
      
            % Done
            io.msgLog(LogLevel.Test, 'DbRecord test passed');
            Result = true;
        end
    end
        
end

