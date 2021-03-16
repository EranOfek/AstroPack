% @Chen

% Configuration base Class
% Package: 
% Description:
%--------------------------------------------------------------------------

classdef Config < Base
    % Properties
    properties (SetAccess = public)
        FileName    % Must be single quoted (why?)
        Yaml        
    end
    
    %-------------------------------------------------------- 
    methods
        % Constructor    
        function Obj = Config(FileName)
            if FileName ~= ""
                Obj.load(FileName)
            end
        end
        
        % Read file to lines
        function Result = load(Obj, FileName)
            Obj.FileName = FileName;
            disp("Config: Loading file: " + Obj.FileName);
            Obj.Yaml = yaml.ReadYaml(Obj.FileName);
            Result = true;
        end
        
        function Len = listLen(Obj, List)
            [~, Len] = size(List);
        end
        
        function Value = listItem(Obj, List, Index)
            Value = List
        end
        
    end
    
    %----------------------------------------------------------------------
    % Unit test
    methods(Static)
        function Result = unitTest()
            fprintf("Started\n");
            
            addpath("D:\Ultrasat\AstroPack.git\matlab\external");

            FileName = 'D:\Ultrasat\AstroPack.git\config\pipeline.yml';
            conf = Config(FileName);
            
            %
            disp(conf.Yaml.UnitTest);
            
            %
            disp(conf.Yaml.UnitTest.Key1);
            disp(conf.Yaml.UnitTest.Key2);
            disp(conf.Yaml.UnitTest.Key0x2D3);
            disp(conf.Yaml.UnitTest.x0x2DKeyMinus);
            
            disp(conf.listLen(conf.Yaml.UnitTest.NonUniqueKeys));
            Result = true;
        end
    end    
        
end

