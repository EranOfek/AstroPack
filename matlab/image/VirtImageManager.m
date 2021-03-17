% @Chen

% Configuration base Class
% Package: 
% Description:
%--------------------------------------------------------------------------


classdef VirtImageManager < Component
    % Properties
    properties (SetAccess = public)
        
        filename        
        configPath = "";
        data
        lines
        userData
        imageList
        virtPath = "";
        
        ImageList = []
    end
    
    %-------------------------------------------------------- 
    methods
        % Constructor    
        function Obj = VirtImageManager()
            Obj.log("VirtImageManager created");
            Obj.ImageList = containers.Map();
            
        end
        
        
        function delete(Obj)
            Obj.log("VirtImageManager deleted");
        end
        
        
        function registerImage(Obj, Image)
            Obj.log("registerImage: ");
        end
        
        
        function unregisterImage(Obj, Image)
            Obj.log("unregisterImage: ");
        end
        
    end

    
    methods(Static)
        
        function Result = getManager()
            persistent Manager
            if isempty(Manager)
                Manager = VirtImageManager;
            end
            Result = Manager;
        end
    end
    
    
    % Unit test
    methods(Static)
        function Result = unitTest()
            Result = true;
        end
    end    
        
end


