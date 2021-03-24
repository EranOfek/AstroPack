% @Chen

% Configuration base Class
% Package: 
% Description:
%--------------------------------------------------------------------------

classdef Configuration < Base
    
    % Properties
    properties (SetAccess = public)
        ConfigName      % Optional name for the entire configuration
        Path            %
        
        % 
        System ConfigurationFile
        Pipeline ConfigurationFile
        UnitTest ConfigurationFile
    end
    
    %-------------------------------------------------------- 
    methods % Constructor            
        function Obj = Configuration()
        end
    end

    
    methods % Main functions
        %
        function Result = load(Obj, Path)
            Obj.Path = Path
            Obj.ConfigName = '';
            disp("Config: Loading file: " + Obj.Path);
            
            %
            Obj.System.load(fullfile(Obj.Path, 'system.yml'));
            Obj.Pipeline.load(fullfile(Obj.Path, 'pipeline.yml'));
            Obj.UnitTest.load(fullfile(Obj.Path, 'unittest.yml'));
            
            Result = true;
        end
        
        
        function reload(Obj)
            load(Obj, Obj.Path);
        end
    end
    
    
    methods % Helper functions
        
        % Replace macros in string with values from struct
        % Str="$Root/abc", MacrosStruct.Root="xyz" -> "xyz/abc"
        % conf.unmacro(conf.Yaml.DarkImage.InputFolder, conf.Yaml.EnvFolders)
        function Result = unmacro(Obj, Str, MacrosStruct)
            FieldNames = fieldnames(MacrosStruct);
            for i = 1:numel(FieldNames)
                Var = FieldNames{i};
                Macro = "$" + Var;
                Value = MacrosStruct.(Var);
                if ~isempty(strfind(Str, Macro))
                    NewStr = strrep(Str, Macro, Value);
                    Str = NewStr;
                end                    
            end
            Result = Str;
        end
        
        
        % [min, max] = conf.getRange(conf.Yaml.DarkImage.TemperatureRange)
        function [Min, Max] = getRange(Obj, Cell)
            Min = Cell{1};
            Max = Cell{2};
        end
        
        
        function Len = listLen(Obj, List)
            [~, Len] = size(List);
        end
        
        function Value = listItem(Obj, List, Index)
            Value = List
        end
        
    end
    
    
   
    methods(Static) % Static functions
        
        % Return singeton object
        function Result = getDefaultConfig()
            persistent Conf
            if isempty(Conf)
                Conf = Config;
            end
            Result = Conf;
        end
    end    
    
    %----------------------------------------------------------------------
    % Unit test
    methods(Static)
        function Result = unitTest()
            fprintf("Started\n");
            
            addpath("D:\Ultrasat\AstroPack.git\matlab\external");

            Path = 'D:\Ultrasat\AstroPack.git\config';
            conf = Configuration();
            conf.load(Path);
            
            %
            disp(conf.UnitTest.Yaml.UnitTest);
            
            %
            disp(conf.Yaml.UnitTest.Key1);
            disp(conf.Yaml.UnitTest.Key2);
            disp(conf.Yaml.UnitTest.Key0x2D3);
            disp(conf.Yaml.UnitTest.x0x2DKeyMinus);
            
            disp(conf.listLen(conf.Yaml.UnitTest.NonUniqueKeys));
            
            disp(conf.unmacro("$Root/abc", conf.Yaml.DarkImage.InputFolder, conf.Yaml.EnvFolders));
            Result = true;
        end
    end    
        
end

