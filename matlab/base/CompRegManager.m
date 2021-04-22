% 
%--------------------------------------------------------------------------

classdef CompRegManager < Component
    
    % Properties
    properties (SetAccess = public)
        
        % Map object is a data structure that allows you to retrieve values 
        % using a corresponding key. Keys can be real numbers or character 
        % vectors. As a result, they provide more flexibility for data 
        % access than array indices, which must be positive integers. 
        % Values can be scalar or nonscalar arrays.
        Map = []            % containers.Map - List of objects
    end
    
    %-------------------------------------------------------- 
    methods % Constructor 
        
        function Obj = CompRegManager()
            Obj.msgLog(LogLevel.Debug, 'CompRegManager created');
            Obj.Map = containers.Map();
            
        end
        
        
        function delete(Obj)
            % Destructor
            Obj.msgLog(LogLevel.Debug, 'CompRegManager deleted');
            release();
        end
    end
    
    
    methods    
        function register(Obj, Comp)
            Key = Obj.getKey(Comp);
            Obj.msgLog(LogLevel.Info, 'register: %s', Key);
            
            
            if ~Obj.Map.isKey(Key)
                Obj.Map(Key) = Comp;
            else
                Obj.msgLog(ObjLevel.Warning, 'register: Component already exists in map: %s', Key);
            end
        end
        
        
        function unregister(Obj, Comp)
            Key = Obj.getKey(Comp);
            Obj.msgLog(LogLevel.Debug, 'unregister: %s', Key);            
            
            if Obj.Map.isKey(Key)
                Obj.Map.remove(Key)
            else
                Obj.msgLog(ObjLevel.Warning, 'unregister: Component does not exist in map: %s', Key);
            end            
            
        end
        
        
        function Result = getComp(Obj, CompKey)
            if Obj.Map.isKey(CompKey)
                Result = Obj.Map(CompKey);
            else
                Result = [];
            end
        end
        
        
        function Result = getKey(Obj, Comp)
            Result = Comp.needRegKey();
        end
        
        
        function release(Obj)
            for Key=Obj.Map.keys
                Obj.Map.remove(Key);
            end
            
            % Make sure that everything was removed
            assert(Obj.Map.Count == 0);
        end        
 
    end

    
    methods(Static)
        
%         function Result = getSingle()
%             persistent Manager
%             if isempty(Manager)
%                 Manager = SqlDbManager;
%             end
%             Result = Manager;
%         end
    end
     
    
    methods(Static) % Unit test
        function Result = unitTest()
            io.msgLog(LogLevel.Test, 'CompRegManager test started');

            %Manager = SqlDbManager.getSingle();
            
            % See tests in VirtImage.m
            
            io.msgLog(LogLevel.Test, 'CompRegManager test passed');
            Result = true;
        end
    end    
        
end


