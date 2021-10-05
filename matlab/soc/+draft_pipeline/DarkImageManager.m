% Dark Image Management

classdef DarkImageManager < PipelineProcess
    
    % Properties
    properties (SetAccess = public)
        ImageList = []
        startTime = 0
    end
    
    %-------------------------------------------------------- 
    methods  
               
        % Constructor    
        function Obj = DarkImageManager()
        end

        
        function init(Obj)
        end
        
        
        % Process dark images in specified folder
        function processFolder(Obj, Path)            
            List = Obj.getFilesInFolder(Path, "*.fits");
            for i = 1:length(List)
                processNextImage(List(i));                
            end            
            
        end
        
        
        function reset(Obj)
        end
        
        
        function processNextImage(Obj, Image)
            
        end
        
 
    end

    
    % Unit test
    methods(Static)   
        function Result = unitTest()
            fprintf("Started\n");
            Result = true;
        end
    end    
        
end

