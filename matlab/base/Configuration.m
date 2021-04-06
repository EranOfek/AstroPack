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
            
            Obj.Path = 'D:\Ultrasat\AstroPack.git\config';
        end
    end

    
    methods % Main functions
         
        function loadConfig(Obj)
            % Load entire Configuration: all files in folder
            
            % Load default folder            
            Obj.loadFolder(Obj.Path);
            
            % Load local files
            Obj.loadFolder(fullfile(Obj.Path, 'local'));
        end
        
        
        function loadFile(Obj, FileName)
            % Load specified file to property            
            
            try
                [~, name, ~] = fileparts(FileName);
                PropName = name;
                if isprop(Obj, PropName)
                    io.msgLog(LogLevel.Warning, 'Property already exist: %s', PropName);
                else
                    io.msgLog(LogLevel.Info, 'Adding property: %s', PropName);
                    Obj.addprop(PropName);
                end
                Yml = Configuration.loadYaml(FileName);
                Obj.(PropName) = Yml;
            catch
                io.msgLog(LogLevel.Error, 'loadFile: Exception: %s', FileName);
            end
        end
        
        
        function loadFolder(Obj, Path)
            % Load specified folder to properties

            Obj.Path = Path;
            Obj.ConfigName = 'Config';
            io.msgLog(LogLevel.Info, 'loadFolder: %s', Obj.Path);
            
            List = dir(fullfile(Path, '*.yml'));
            for i = 1:length(List)
                if ~List(i).isdir
                    FileName = fullfile(List(i).folder, List(i).name);
                    Obj.loadFile(FileName);
                end
            end
        end
       
     
        function reloadFile(Obj, YamlStruct)
            % Reload specified configuration file            
            Configuration.reload(Obj.(YamlStruct));
        end        
       
     
        function reloadFolder(Obj)
            % Reload all configuration files from folder            
            loadFolder(Obj, Obj.Path);
        end        
        
        
        function Result = expandFolder(Obj, Path)
            % Expand Path with macros from Configuration.System.EnvFolders
            if isprop(Obj, 'System') && isfield(Obj.System, 'EnvFolders')
                Result = Configuration.unmacro(Path, Obj.System.EnvFolders);
            else
                Result = Path;
            end
        end
        
    end

    %----------------------------------------------------------------------   
    methods(Static) % Static functions
                
        function Result = getDefaultConfig()
            % Return singleton Configuration object
            persistent Conf
            
            % Create instance and load
            if isempty(Conf)
                Conf = Configuration;
                Conf.loadConfig();
            end
            Result = Conf;
        end
            
        
        function YamlStruct = loadYaml(FileName)
            % Read YAML file to struct, add FileName field
            io.msgLog(LogLevel.Info, 'loadYaml: Loading file: %s', FileName);
            try
                YamlStruct = yaml.ReadYaml(string(FileName).char);
                YamlStruct.FileName = FileName;
            catch
                io.msgLog(LogLevel.Error, 'loadYaml: Exception loading file: %s', FileName);
            end
        end
     
        
        function NewYamlStruct = reloadYaml(YamlStruct)
            % Reload configurastion file, 'FileName' field must exist
            if isfield(YamlStruct, 'FileName')
                NewYamlStruct = Configuration.loadYaml(YamlStruct.FileName);
            else
                msgLog('loadYaml: reloadYaml: no FileName property');
                NewYamlStruct = YamlStruct;
            end
        end       
    end
    
    
    methods(Static) % Helper functions
   
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
            % Return list length
            [~, Len] = size(List);
        end
        
        
        function Value = listItem(List, Index)
            Value = List;
        end
        
    end
    
    %----------------------------------------------------------------------   
    methods(Static) % Unit test
        
%         function Result = unitTest()
%             try
%                 Result = Configuration.doUnitTest();
%             catch
%                 Result = false;
%                 io.msgLog(LogLevel.Error, 'unitTest: Exception');
%             end
%         end
        
            
        function Result = unitTest()
            io.msgLog(LogLevel.Test, 'Configuration test started');
            
            addpath('D:\Ultrasat\AstroPack.git\matlab\external');

            ConfigPath = 'D:\Ultrasat\AstroPack.git\config';
            ConfigFileName = 'D:\Ultrasat\AstroPack.git\config\UnitTest.yml';
            
            % Test low level loading of Yaml struct
            io.msgLog(LogLevel.Test, 'Testing low level functions');
            yml = Configuration.loadYaml(ConfigFileName);
            uTest = yml;
            io.msgLog(LogLevel.Test, 'Key1: %s', uTest.Key1);
            io.msgLog(LogLevel.Test, 'Key2: %s', uTest.Key2);
            io.msgLog(LogLevel.Test, 'Key: %s', uTest.Key0x2D3);
            io.msgLog(LogLevel.Test, 'Key: %s', uTest.x0x2DKeyMinus);
            yml = Configuration.reloadYaml(yml);
            uTest = yml;
            io.msgLog(LogLevel.Test, 'Key1: %s', uTest.Key1);                  
            
            % Test Configuration class
            io. msgLog(LogLevel.Test, 'Testing Configuration object');
            conf = Configuration();
            conf.loadFile(ConfigFileName);
            
            %
            io.msgLog(LogLevel.Test, 'FileName: %s', conf.UnitTest.FileName);
            disp(conf.UnitTest);         
            
            %
            io.msgLog(LogLevel.Test, 'Key1: %s', conf.UnitTest.Key1);
            io.msgLog(LogLevel.Test, 'Key2: %s', conf.UnitTest.Key2);
            io.msgLog(LogLevel.Test, 'Key: %s', conf.UnitTest.Key0x2D3);
            io.msgLog(LogLevel.Test, 'Key: %s', conf.UnitTest.x0x2DKeyMinus);
            
            %disp(conf.listLen(conf.UnitTest.NonUniqueKeys));
                    
            % Load all config files in folder
            io.msgLog(LogLevel.Test, 'Testing folder');
            conf.loadFolder(ConfigPath);
            disp(conf.System.EnvFolders);
            
            io.msgLog(LogLevel.Test, 'Testing utility functions');
            io.msgLog(LogLevel.Test, 'unmacro: %s', Configuration.unmacro("$ROOT/abc", conf.System.EnvFolders));
            
            io.msgLog(LogLevel.Test, 'expandFolder: %s', conf.expandFolder("$ROOT/abc"));
            
            % Done
            io.msgLog(LogLevel.Test, 'Configuration test passed');
            Result = true;
        end
    end
        
end

