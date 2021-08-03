% Virtual Image Manager

% See Datastore (v2021a and up)
% https://www.mathworks.com/help/matlab/datastore.html

%--------------------------------------------------------------------------

classdef VirtImageManager < ComponentMap
    % Properties
    properties (SetAccess = public)
        VirtPath = '';      % Default path to storge virtual image files

    end
    %-------------------------------------------------------- 
    methods % Constructor 
        
        function Obj = VirtImageManager()            
            Obj@ComponentMap('VirtImage');
            Obj.msgLog(LogLevel.Debug, 'VirtImageManager created');
            
        end
        
        
        function delete(Obj)
            % Destructor
            Obj.msgLog(LogLevel.Debug, 'VirtImageManager deleted');
            release();
        end
    end
    
    
    methods    
        function add(Obj, Image)
            Obj.msgLog(LogLevel.Debug, 'VirtImageManager.add: %s', Image.Uuid);
          
            add@ComponentMap(Obj, Image);
        end
        
        
        function remove(Obj, Image)
            Obj.msgLog(LogLevel.Debug, 'VirtImageManager.remove: %s', Image.Uuid);
            
            remove@ComponentMap(Obj, Image);
                        
        end
        
        
        function release(Obj)
            for Key = Obj.Map.Map.keys
                Obj.Map.remove(Key);
            end
            
            % Make sure that everything was removed
            assert(Obj.Map.Map.Count == 0);
        end
        
    end

    
    methods(Static)
        
        function Result = getSingleton()
            persistent PersObj
            if isempty(PersObj)
                PersObj = VirtImageManager;
            end
            Result = PersObj;
        end
    end
     
    
    methods(Static) % Unit test
        function Result = unitTest()
            io.msgLog(LogLevel.Test, 'VirtImageManager test started');

            Manager = VirtImageManager.getSingleton();
            
            % See tests in VirtImage.m
            
            io.msgLog(LogLevel.Test, 'VirtImageManager test passed');
            Result = true;
        end
    end    
        
end


