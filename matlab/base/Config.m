% @Chen

% Configuration base Class
% Package: 
% Description:
%--------------------------------------------------------------------------

classdef TConfig < handle
    % Properties
    properties (SetAccess = public)
        FileName        
        Yaml        
    end
    
    %-------------------------------------------------------- 
    methods
        % Constructor    
        function Obj = TConfig()            
        end
        
        % Read file to lines
        function Result = load(Obj, FileName)
            Obj.FileName = FileName;
            disp("Config: Loading file: " + Obj.FileName);
            Obj.Yaml = yaml.ReadYaml(Obj.FileName);
            Result = true;
        end
        
    
    % Unit test
    methods(Static)
        function Result = unitTest()
            fprintf("Started\n");

            Result = true;
        end
    end    
        
end

