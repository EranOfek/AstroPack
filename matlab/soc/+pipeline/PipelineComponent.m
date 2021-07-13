
classdef PipelineComponent < Component
    % Parent class for Pipeline processing components
    
    
    % Properties
    properties (SetAccess = public)
        
        filename        
        configPath = "";
        data
        lines
        userData
        
        inputImagePath
        inputImageExt
    end
    
    %-------------------------------------------------------- 
    methods
        % Constructor    
        function Obj = PipelineComponent()
        end
        

        function FilesList = getFilesInFolder(Path, Mask)
            FilesList = [];
            List = dir(fullfile(Path, Mask));
            for i = 1:length(List)
                if ~List(i).isdir
                    FileName = fullfile(List(i).folder, List(i).name);
                    FilesList = [FilesList, FileName];
                end
            end
        end
    end
    
    
    % Unit test
    methods(Static)
        function Result = unitTest()
            fprintf("Started\n");
            result = true;
        end
    end    
        
end


