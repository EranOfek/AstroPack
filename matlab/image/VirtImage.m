% @Chen

% Configuration base Class
% Package: 
% Description:
%--------------------------------------------------------------------------


classdef VirtImage < Component
    
    % Properties
    properties (SetAccess = public)
        Data
        Manager
    end
    
    %-------------------------------------------------------- 
    methods
        % Constructor    
        function Obj = VirtImage()
            Obj.Manager.registerImage(Obj);
        end
        
        % Destructor
        function delete(Obj)
            Obj.Manager.unregisterImage(Obj);
        end
        
        % Get Manager
        function Result = get.Manager(Obj)
            Result = VirtImageManager.getManager();
        end
        
        % Get data as matrix
        function Result = getData(Obj)            
            Result = Obj.Data;
        end
        
        % Set data from matrix
        function setData(Obj, NewData)
            Obj.Data = NewData;
        end
        
    end
    
    
    % Unit test
    methods(Static)
        function Result = unitTest()
            
            Image1 = VirtImage;
            Image2 = VirtImage;
            
            delete(Image1);
            delete(Image2);
            
            Result = true;
        end
    end    
        
end
