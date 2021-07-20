% @Chen

% Configuration base Class
% Package: 
% Description:
%--------------------------------------------------------------------------


classdef AlertsManager < PipelineComponent
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
        function obj = AlertsManager()
        end
        
        % Read file to lines
        function result = pollInputImage(obj)
            
            obj.filename = fullfile(obj.configPath, fname);
            obj.data = fileread(obj.filename);
            obj.lines = strsplit(obj.data);
            result = true;
        end       
 
    end

    
    % Unit test
    methods(Static)
        function result = uTest()
            fprintf("Started\n");
       
            result = true;
        end
    end    
        
end


