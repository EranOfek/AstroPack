% Virtual Image Class
%--------------------------------------------------------------------------

classdef VirtImage < Component
    
    % Properties
    properties (SetAccess = public)
        Data                        %
        FileName = ''               %
        Manager VirtImageManager    %
    end
    
    %-------------------------------------------------------- 
    methods
        % Constructor    
        function Obj = VirtImage()
            Obj.Manager = VirtImageManager.getSingleton();
            Obj.needUuid();
            Obj.Manager.registerImage(Obj);
        end
        
        % Destructor
        function delete(Obj)
            Obj.Manager.unregisterImage(Obj);
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
            io.msgLog(LogLevel.Test, 'VirtImage test started');
            
            Image1 = VirtImage;
            Image2 = VirtImage;
            
            delete(Image1);
            delete(Image2);
            
            io.msgLog(LogLevel.Test, 'VirtImage test passed');
            Result = true;
        end
    end    
        
end
