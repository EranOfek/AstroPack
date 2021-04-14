% Virtual Image Manager
%--------------------------------------------------------------------------


classdef VirtImageManager < Component
    % Properties
    properties (SetAccess = public)
        VirtPath = '';      % Default path to storge virtual image files
        ImageList = []      % containers.Map - List of VirtImage objects
    end
    
    %-------------------------------------------------------- 
    methods % Constructor 
        
        function Obj = VirtImageManager()
            Obj.msgLog(LogLevel.Debug, 'VirtImageManager created');
            Obj.ImageList = containers.Map();
            
        end
        
        
        function delete(Obj)
            % Destructor
            Obj.msgLog(LogLevel.Debug, 'VirtImageManager deleted');
        end
    end
    
    
    methods    
        function registerImage(Obj, Image)
            Obj.msgLog(LogLevel.Debug, 'registerImage: %s', Image.Uuid);
        end
        
        
        function unregisterImage(Obj, Image)
            Obj.msgLog(LogLevel.Debug, 'unregisterImage: %s', Image.Uuid);
        end
        
    end

    
    methods(Static)
        
        function Result = getSingle()
            persistent Manager
            if isempty(Manager)
                Manager = VirtImageManager;
            end
            Result = Manager;
        end
    end
     
    
    methods(Static) % Unit test
        function Result = unitTest()
            io.msgLog(LogLevel.Test, 'VirtImageManager test started');

            Manager = VirtImageManager.getSingle();
            
            
            
            io.msgLog(LogLevel.Test, 'VirtImageManager test passed');
            Result = true;
        end
    end    
        
end


