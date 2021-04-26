% 
%--------------------------------------------------------------------------

classdef ComponentMap < Base
    
    % Properties
    properties (SetAccess = public)
        
        % Map object is a data structure that allows you to retrieve values 
        % using a corresponding key. Keys can be real numbers or character 
        % vectors. As a result, they provide more flexibility for data 
        % access than array indices, which must be positive integers. 
        % Values can be scalar or nonscalar arrays.
        Map = []            % containers.Map - List of CompDirMap
        Name = [] 
    end
    
    %-------------------------------------------------------- 
    methods % Constructor 
        
        function Obj = ComponentMap(varargin)
            if numel(varargin) > 0
                Obj.Name = varargin{1};
            else
                Obj.Name = '(unnamed)';
            end
            Obj.msgLog(LogLevel.Debug, 'ComponentMap created: %s', Obj.Name);
            Obj.Map = containers.Map();
            
        end
        
        
        function delete(Obj)
            % Destructor
            Obj.msgLog(LogLevel.Debug, 'ComponentMap deleted: %s', Obj.Name);
            release();
        end
    end
    
    
    methods    
        function add(Obj, Comp)
            Key = Obj.getKey(Comp);
            Obj.msgLog(LogLevel.Info, 'add: %s', Key);            
            
            if ~Obj.Map.isKey(Key)
                Obj.Map(Key) = Comp;
            else
                Obj.msgLog(ObjLevel.Warning, 'add: Component already exists in map: %s', Key);
            end
        end
        
        
        function remove(Obj, Comp)
            Key = Obj.getKey(Comp);
            Obj.msgLog(LogLevel.Debug, 'remove: %s', Key);            
            
            if Obj.Map.isKey(Key)
                Obj.Map.remove(Key)
            else
                Obj.msgLog(ObjLevel.Warning, 'remove: Component does not exist in map: %s', Key);
            end            
            
        end
              
        
        function Result = find(Obj, CompKey)
            % Return component by key
            if Obj.Map.isKey(CompKey)
                Result = Obj.Map(CompKey);
            else
                Result = [];
            end
        end
        
        
        function Result = getKey(Obj, Comp)
            % Get/make component key
            Result = Comp.needMapKey();
        end
        
        
        function release(Obj)
            % Release all components from map
            
            for Key = Obj.Map.keys
                Comp = Obj.Map(Key);
                
                % @TODO: Need release or delete??
                %Comp.release();
                Obj.Map.remove(Key);
            end
            
            % Make sure that everything was removed
            assert(Obj.Map.Count == 0);
        end        
        
        
        function msgLog(Obj, Level, varargin)  
            % Write message to log
            %Obj.Log.msgLog(Level, varargin{:});
            io.msgLog(Level, varargin{:});
        end
        
 
    end


    methods(Static) % Unit test    
    
        function Result = getSingleton()
            % Return singleton object
            persistent PersObj
            if isempty(PersObj)
                PersObj = ComponentMap;
            end
            Result = PersObj;
        end    
    end
    
    
        
    methods(Static) % Unit test
        function Result = unitTest()
            io.msgLog(LogLevel.Test, 'ComponentMap test started');

            Map = ComponentMap;
            
            Comp1 = Component;
            Map.add(Comp1);
            assert(~isempty(Map.find(Comp1.MapKey)));
            
            Map.remove(Comp1);
            assert(isempty(Map.find(Comp1.MapKey)));
            
            io.msgLog(LogLevel.Test, 'ComponentMap test passed');
            Result = true;
        end
    end    
        
end


