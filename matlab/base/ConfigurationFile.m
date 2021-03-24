% Single Yaml configuration file

classdef ConfigurationFile < Base
    % Properties
    properties (SetAccess = public)
        FileName    % .yml file name
        Yaml        % Struct loaded from the YAML file
    end
    
    %-------------------------------------------------------- 
    methods
        % Constructor    
        function Obj = ConfigurationFile()
        end
        
        % Read file to lines
        function Result = load(Obj, FileName)
            Obj.FileName = FileName;
            disp("Config: Loading file: " + Obj.FileName);
            Obj.Yaml = yaml.ReadYaml(string(Obj.FileName).char);
            Result = true;
        end
        
        
        function reload(Obj)
            load(Obj, Obj.FileName);
        end       
    end
       
    %----------------------------------------------------------------------
    
    methods(Static) % Unit test
        function Result = unitTest()
            fprintf("Started\n");
            
            addpath("D:\Ultrasat\AstroPack.git\matlab\external");

            FileName = 'D:\Ultrasat\AstroPack.git\config\unittest.yml';
            conf = ConfigurationFile(FileName);
            
            %
            disp(conf.Yaml.UnitTest);
            
            %
            disp(conf.Yaml.UnitTest.Key1);
            disp(conf.Yaml.UnitTest.Key2);
            disp(conf.Yaml.UnitTest.Key0x2D3);
            disp(conf.Yaml.UnitTest.x0x2DKeyMinus);
 
            Result = true;
        end
    end    
        
end

