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
        ConfigName              % Optional name for the entire configuration
        Path                    % Path of configuration files
        External                % Path to external packages
        Data struct = struct()  % Initialize empty struct, all YML files are added here in tree structure
    end
    
    %-------------------------------------------------------- 
    methods % Constructor            
        function Obj = Configuration()
            
            % Get full path and name of the file in which the call occurs, 
            % not including the filename extension
            MyFileName = mfilename('fullpath');       
            [MyPath, ~, ~] = fileparts(MyFileName);            
            
            % Set path to configuration files
            % @FFU: overide with env???
            Obj.Path = fullfile(MyPath, '..', '..', 'config');
            
            % Set path to yaml external package
            % Replace it with env? move to startup.m?
            Obj.External = fullfile(MyPath, '..', 'external');
            addpath(Obj.External);            
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
                %if isfield(Obj.Data, PropName)
                    io.msgLog(LogLevel.Warning, 'Property already exist: %s', PropName);
                else
                    io.msgLog(LogLevel.Info, 'Adding property: %s', PropName);                    
                    %Obj.addprop(PropName);
                end
                Yml = Configuration.loadYaml(FileName);
                
                % When name contains dots, create tree of structs (i.e. 'x.y.z')
                s = sprintf('Obj.Data.%s=Yml', name);
                eval(s);
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
                
        function Result = init()
            % Return singleton Configuration object
            persistent Conf
            if isempty(Conf)
                Conf = Configuration;
            end
            Result = Conf;
        end
        
        
        function Result = getSingleton()
            % Return singleton Configuration object
            Conf = Configuration.init();
            if isempty(Conf.Data) || numel(fieldnames(Conf.Data)) == 0
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
            
            Conf = Configuration.init();
            assert(~isempty(Conf.Path));
            assert(~isempty(Conf.External));
            
            fprintf('Conf.Path: %s\n', Conf.Path);
            fprintf('Conf.External: %s\n', Conf.External);

            ConfigPath = Conf.Path;
            ConfigFileName = fullfile(ConfigPath, 'UnitTest.yml');
            
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
            Conf.loadFile(ConfigFileName);
            
            confUnitTest = Conf.Data.UnitTest;
            
            %
            io.msgLog(LogLevel.Test, 'FileName: %s', confUnitTest.FileName);
            disp(Conf.Data.UnitTest);         
            
            %
            io.msgLog(LogLevel.Test, 'Key1: %s', confUnitTest.Key1);
            io.msgLog(LogLevel.Test, 'Key2: %s', confUnitTest.Key2);
            io.msgLog(LogLevel.Test, 'Key: %s', confUnitTest.Key0x2D3);
            io.msgLog(LogLevel.Test, 'Key: %s', confUnitTest.x0x2DKeyMinus);
            
            %disp(conf.listLen(conf.UnitTest.NonUniqueKeys));
                    
            % Load all config files in folder
            io.msgLog(LogLevel.Test, 'Testing folder');
            Conf.loadFolder(ConfigPath);
            disp(Conf.Data.System.EnvFolders);
            
            io.msgLog(LogLevel.Test, 'Testing utility functions');
            io.msgLog(LogLevel.Test, 'unmacro: %s', Configuration.unmacro("$ROOT/abc", Conf.Data.System.EnvFolders));
            
            io.msgLog(LogLevel.Test, 'expandFolder: %s', Conf.expandFolder("$ROOT/abc"));
            
            % Done
            io.msgLog(LogLevel.Test, 'Configuration test passed');
            Result = true;
        end
    end
        
end

