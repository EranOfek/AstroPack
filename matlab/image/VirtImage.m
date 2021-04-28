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
			% @Todo: should we take care of VirtImage array?
            Obj.needUuid();
            Obj.Manager = VirtImageManager.getSingleton();            
            Obj.Manager.add(Obj);
        end
        
        % Destructor
        function delete(Obj)
            Obj.Manager.remove(Obj);
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
            
            Manager = VirtImageManager.getSingleton();
            
            % Add images
            Count = Manager.getCount();
            Image1 = VirtImage;
            Image2 = VirtImage;                     
            assert(Manager.getCount() == Count+2);
            
            % Delete images
            delete(Image1);
            delete(Image2);
            assert(Manager.getCount() == Count);
            
            io.msgLog(LogLevel.Test, 'VirtImage test passed');
            Result = true;
        end
    end    
        
end
