% Virtual Image Manager
%--------------------------------------------------------------------------

classdef VirtImageManager < Component
    % Properties
    properties (SetAccess = public)
        VirtPath = '';      % Default path to storge virtual image files
        
        % Map object is a data structure that allows you to retrieve values 
        % using a corresponding key. Keys can be real numbers or character 
        % vectors. As a result, they provide more flexibility for data 
        % access than array indices, which must be positive integers. 
        % Values can be scalar or nonscalar arrays.
        Map = []            % containers.Map - List of VirtImage objects
    end
    
    %-------------------------------------------------------- 
    methods % Constructor 
        
        function Obj = VirtImageManager()
            Obj.msgLog(LogLevel.Debug, 'VirtImageManager created');
            Obj.Map = containers.Map();
            
        end
        
        
        function delete(Obj)
            % Destructor
            Obj.msgLog(LogLevel.Debug, 'VirtImageManager deleted');
            release();
        end
    end
    
    
    methods    
        function registerImage(Obj, Image)
            Obj.msgLog(LogLevel.Debug, 'registerImage: %s', Image.Uuid);
            
            Key = Obj.getImageKey(Image);
            if ~Obj.Map.isKey(Key)
                Obj.Map(Key) = Image;
            else
                Obj.msgLog(ObjLevel.Warning, 'registerImage: Image already exists in map: %s', Key);
            end
        end
        
        
        function unregisterImage(Obj, Image)
            Obj.msgLog(LogLevel.Debug, 'unregisterImage: %s', Image.Uuid);
            
            Key = Obj.getImageKey(Image);
            if Obj.Map.isKey(Key)
                Obj.Map.remove(Key)
            else
                Obj.msgLog(ObjLevel.Warning, 'unregisterImage: Image does not exist in map: %s', Key);
            end            
            
        end
        
        
        function Result = getImagKey(Obj, Image)
            Result = Image.Uuid;
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
        
        function Result = getSingleton()
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

            Manager = VirtImageManager.getSingleton();
            
            % See tests in VirtImage.m
            
            io.msgLog(LogLevel.Test, 'VirtImageManager test passed');
            Result = true;
        end
    end    
        
end


