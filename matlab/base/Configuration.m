% Top-level configuration
%
% Load multiple configuration files as properties
%
% Load all YML files in folder
% Access each file as property of the Configuration object.
%--------------------------------------------------------------------------

classdef Configuration < dynamicprops
    
    % Properties
    properties (SetAccess = public)
        ConfigName      % Optional name for the entire configuration
        Path            % Path of configuration files
    end
    
    %-------------------------------------------------------- 
    methods % Constructor            
        function Obj = Configuration()
            % Replace it with env? move to startup.m?
            addpath('D:\Ultrasat\AstroPack.git\matlab\external');
        end
    end

    
    methods % Main functions
         
        function loadConfig(Obj)
            
            % Load default folder
            Path = 'D:\Ultrasat\AstroPack.git\config';
            Obj.loadFolder(Path);
            
            % Load local files
            Obj.loadFolder(fullfile(Path, 'local'));
        end
        
        
        function loadFile(Obj, FileName)
            % Load specified file to property            
            
            try
                [~, name, ~] = fileparts(FileName);
                PropName = name;
                if isprop(Obj, PropName)
                    msgLog('Property already exist: %s', PropName);
                else
                    msgLog('Adding property: %s', PropName);
                    Obj.addprop(PropName);
                end
                Yml = Configuration.loadYaml(FileName);
                Obj.(PropName) = Yml;
            catch
                msgLog('loadFile: Exception: %s', FileName);
            end
        end
        
        
        function loadFolder(Obj, Path)
            % Load specify folder to properties

            Obj.Path = Path;
            Obj.ConfigName = 'Config';
            msgLog('loadFolder: %s', Obj.Path);
            
            List = dir(fullfile(Path, '*.yml'));
            for i = 1:length(List)
                if ~List(i).isdir
                    FileName = fullfile(List(i).folder, List(i).name);
                    Obj.loadFile(FileName);
                end
            end
        end
       
     
        function reloadFile(Obj, YamlStruct)
            % Reload 
            Configuration.reload(Obj.(YamlStruct));
        end        
       
     
        function reloadFolder(Obj)
            % Reload all configuration files from folder
            loadFolder(Obj, Obj.Path);
        end        
        
        
        function Result = expandFolder(Obj, Path)
            if isprop(Obj, 'System') && isfield(Obj.System, 'EnvFolders')
                Result = Configuration.unmacro(Path, Obj.System.EnvFolders);
            else
                Result = Path;
            end
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
    methods(Static) % Static functions    
        
        function YamlStruct = loadYaml(FileName)
            % Read YAML file to struct, add FileName field
            msgLog('loadYaml: Loading file: %s', FileName);
            try
                YamlStruct = yaml.ReadYaml(string(FileName).char);
                YamlStruct.FileName = FileName;
            catch
                msgLog('loadYaml: Exception loading file: %s', FileName);
            end
        end
     
        
        function NewYamlStruct = reloadYaml(YamlStruct)
            % Reload file
            if isfield(YamlStruct, 'FileName')
                NewYamlStruct = Configuration.loadYaml(YamlStruct.FileName);
            else
                msgLog('loadYaml: reloadYaml: no FileName property');
                NewYamlStruct = YamlStruct;
            end
        end       
    end
    
    
    methods(Static)
   
        function Result = unmacro(Str, MacrosStruct)
            % Replace macros in string with values from struct
            % Str="$Root/abc", MacrosStruct.Root="xyz" -> "xyz/abc"
            % conf.unmacro(conf.Yaml.DarkImage.InputFolder, conf.Yaml.EnvFolders)
            
            FieldNames = fieldnames(MacrosStruct);
            for i = 1:numel(FieldNames)
                Var = FieldNames{i};
                Macro = "$" + Var;
                Value = MacrosStruct.(Var);
                if contains(Str, Macro)
                    NewStr = strrep(Str, Macro, Value);
                    Str = NewStr;
                end                    
            end
            Result = Str;
        end
              

        function [Min, Max] = getRange(Cell)
            % Get minimum and maximum values from cell array
            % Example: [min, max] = conf.getRange(conf.Yaml.DarkImage.TemperatureRange)            
            Min = Cell{1};
            Max = Cell{2};
        end
               
        
        function Len = listLen(List)
            [~, Len] = size(List);
        end
        
        
        function Value = listItem(List, Index)
            Value = List;
        end
        
    end
    
    %----------------------------------------------------------------------
    % Unit test
    methods(Static)
        
        function Result = unitTest()
            try
                Result = Configuration.doUnitTest();
            catch
                Result = false;
                msgLog('unitTest: Exception');
            end
        end
        
            
        function Result = doUnitTest()
            msgLog('Started\n');
            
            addpath('D:\Ultrasat\AstroPack.git\matlab\external');

            ConfigPath = 'D:\Ultrasat\AstroPack.git\config';
            ConfigFileName = 'D:\Ultrasat\AstroPack.git\config\UnitTest.yml';
            
            % Test low level loading of Yaml struct
            msgLog('Testing low level functions');
            yml = Configuration.loadYaml(ConfigFileName);
            uTest = yml;
            msgLog(uTest.Key1);
            msgLog(uTest.Key2);
            msgLog(uTest.Key0x2D3);
            msgLog(uTest.x0x2DKeyMinus);
            yml = Configuration.reloadYaml(yml);
            uTest = yml;
            msgLog(uTest.Key1);      
            
            
            % Test Configuration class
            msgLog('Testing Configuration object');
            conf = Configuration();
            conf.loadFile(ConfigFileName);
            
            %
            msgLog('FileName: %s', conf.UnitTest.FileName);
            disp(conf.UnitTest);         
            
            %
            msgLog(conf.UnitTest.Key1);
            msgLog(conf.UnitTest.Key2);
            msgLog(conf.UnitTest.Key0x2D3);
            msgLog(conf.UnitTest.x0x2DKeyMinus);
            
            %disp(conf.listLen(conf.UnitTest.NonUniqueKeys));
                    
            
            msgLog('Testing folder');
            conf.loadFolder(ConfigPath);
            disp(conf.System.EnvFolders);
            
            msgLog('Testing utility functions');
            msgLog(Configuration.unmacro("$ROOT/abc", conf.System.EnvFolders));
            
            msgLog(conf.expandFolder("$ROOT/abc"));
            
            % Done
            Result = true;
        end
    end
        
end



function msgLog(varargin)
    %fprintf('Configuration: ');
    fprintf(varargin{:});
    fprintf('\n');
end

