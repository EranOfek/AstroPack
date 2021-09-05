% Configuration class
% Load multiple configuration files as properties
%
% Load all YML files in folder
% Access each file as property of the Configuration object.
%
% Usually we work with only one singleton configuration object in the
% system.
%
% Note: Since Configuration.getSingleton() uses persistant object,
%       in order to load fresh configuration you need to do 'clear all'
%--------------------------------------------------------------------------

% #functions
% Configuration -
% expandFolder - Expand Path with macros from Configuration.System.EnvFolders
% getRange (Static) - Get minimum and maximum values from cell array Example: [min, max] = conf.getRange(conf.Yaml.DarkImage.TemperatureRange)
% getSingleton (Static) - Return singleton Configuration object
% init (Static) - Return singleton Configuration object
% listItem (Static) -
% listLen (Static) - Return list length
% loadConfig - Load ALL configuration files in Obj.Path folder
% loadFile - Load specified file to property
% loadFolder - Load specified folder to properties
% loadYaml (Static) - Read YAML file to struct, add FileName field
% reloadFile - Reload specified configuration object (file name)
% reloadFolder - Reload all configuration files from default folder
% reloadYaml (Static) - Reload configurastion file, 'FileName' field must exist
% unitTest - Configuration.unitTest
% unmacro (Static) - Replace macros in string with values from struct Str="$Root/abc", MacrosStruct.Root="xyz" -> "xyz/abc" conf.unmacro(conf.Yaml.DarkImage.InputFolder, conf.Yaml.EnvFolders)
% #/functions
%
classdef Configuration < handle
    % Note that this class is derived from Base and not from Component

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
            EnvPath = getenv('ASTROPACK_CONFIG_PATH');
            if ~isempty(EnvPath)
                Obj.Path = EnvPath;
                io.msgLog(LogLevel.Info, 'Configuration: Using env path: %s', Obj.Path);
            else
                Obj.Path = fullfile(MyPath, '..', '..', '..', 'config');
                io.msgLog(LogLevel.Info, 'Configuration: Using git path: %s', Obj.Path);
            end

            % Set path to yaml external package
            % Replace it with env? move to startup.m?
            Obj.External = fullfile(MyPath, '..', '..', 'external');

            % commented out by Enrico. Obtrusive. If there is really a
            %  value in these messages, make them conditioned to when I'm
            %  not using the class
            % fprintf('Configuration Path: %s\n', Obj.Path);
            % fprintf('Configuration External: %s\n', Obj.External);
            % fprintf('Master Configuration files are located in AstroPack/config\n');

            % Validate
            assert(~isempty(Obj.Path));
            assert(~isempty(Obj.External));
            assert(isfolder(Obj.Path));
            assert(isfolder(Obj.External));

            addpath(Obj.External);
        end
    end


    methods % Main functions

        function loadConfig(Obj)
            % Load ALL configuration files in Obj.Path folder

            % Load files in default folder
            Obj.loadFolder(Obj.Path);

            % Load local files from local/ subfolder
            % These files should be excluded from git (in config/.gitignore)
            Obj.loadFolder(fullfile(Obj.Path, 'local'));
        end


        function Result = loadFile(Obj, FileName, Args)
            % Load specified file to property

            arguments
                Obj         %
                FileName    % Configuration file name

                % True to create new property in Obj.Data with the file
                % name, otherwise YML keys are loaded directly under
                % Obj.Data property
                Args.Field logical = true;
            end

            Result = false;
            try
                [~, name, ~] = fileparts(FileName);
                PropName = name;
                if isfield(Obj.Data, PropName)
                    io.msgLog(LogLevel.Warning, 'Property already exist: Data.%s', PropName);
                else
                    io.msgLog(LogLevel.Info, 'Adding property: %s', PropName);
                end

                % Yml is used used below by eval()
                try
                    Yml = Configuration.loadYaml(FileName); %#ok<NASGU>
                    Result = true;
                catch
                    io.msgLog(LogLevel.Error, 'Configuration.loadYaml failed: %s', FileName);
                    Yml = struct; %#ok<NASGU>
                end

                % When name contains dots, create tree of structs (i.e. 'x.y.z')
                if Args.Field
                    s = sprintf('Obj.Data.%s=Yml;', name);
                else
                    s = sprintf('Obj.Data=Yml;');
                end
                eval(s);
            catch
                io.msgStyle(LogLevel.Error, '@error', 'loadFile: Exception: %s', FileName);
            end
        end


        function loadFolder(Obj, Path)
            % Load specified folder to properties

            %@Todo: fix
            %Obj.Path = Path;
            %Obj.ConfigName = 'Config';
            io.msgLog(LogLevel.Info, 'loadFolder: %s', Obj.Path);

            % Scan folder for files
            List = dir(fullfile(Path, '*.yml'));
            for i = 1:length(List)
                if ~List(i).isdir
                    FileName = fullfile(List(i).folder, List(i).name);
                    Obj.loadFile(FileName);
                end
            end
        end


        function reloadFile(Obj, YamlStruct)
            % Reload specified configuration object (file name)
            Configuration.reload(Obj.(YamlStruct));
        end


        function reloadFolder(Obj)
            % Reload all configuration files from default folder
            loadFolder(Obj, Obj.Path);
        end


        function Result = expandFolder(Obj, Path)
            % Expand Path with macros from Configuration.System.EnvFolders
            if isfield(Obj.Data, 'System') && isfield(Obj.Data.System, 'EnvFolders')
                Result = Configuration.unmacro(Path, Obj.Data.System.EnvFolders);
            else
                Result = Path;
            end
        end

    end

    %----------------------------------------------------------------------
    methods(Static) % Static functions

        function Result = init(varargin)
            % Return singleton Configuration object
            persistent Conf

            % Clear configuration
            if numel(varargin) > 0
                Conf = [];
            end

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
                if ~isfile(FileName)
                    io.msgLog(LogLevel.Error, 'loadYaml: File not found: %s', FileName);
                end
                YamlStruct = yaml.ReadYaml(string(FileName).char);
                YamlStruct.FileName = FileName;
            catch
                io.msgStyle(LogLevel.Error, '@error', 'loadYaml: Exception loading file: %s', FileName);
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

        Result = unitTest()
    end

end

