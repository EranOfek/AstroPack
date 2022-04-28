% #autogen:_off
%
% Configuration class for YML files, based on Java.
% Each yml file is loaded as struct under the Data property of the object.
%
% Author: Chen Tishler (Apr 2021)
%
% There is a singleton configuration object which loads the system configuration.
% Note: Since Configuration.getSingleton() uses persistant object,
%       in order to load fresh configuration you need to do 'clear all'
%--------------------------------------------------------------------------
% ### Usage
% Configuration may be used in two ways:
%
%  1. 'Global' configuration object (stored as singleton object), there is
%     always such Configuration object which is linked by default to
%     Component.Config when Component is created.
%     Global configuration is loaded from:
%        A. By environment variable ASTROPACK_CONFIG_PATH, and if not found.
%        B. From the 'config/' folder inside the source code repository (i.e.,
%           '../../../config/')
%     Note that files under config/local folder are excluded from git.
%
%  2. User defined configuration object, may be used to load specific YML
%     files which are not part of the system configuration folder.
%--------------------------------------------------------------------------
% Each YML file is loaded to a property inside Obj.Data that match the YML
% file name. For example, config/unitTest.yml is loaded to struct Obj.Data.unittest.
%
% Additional **FileName** property is added to the struct, to keep tracking of
% which file was loaded, allowed a reload if required.
%
% When you create a decended of Component, it has a Config property which
% is linked to the singleton Configuration object.
% The first instace of Component which is created triggers loading of the
% system configuration.
%
% ### Example
%   Access global configuration from Component decendent:
%   Get the value of 'Key1' inside configuration file 'unittest.yml'.
%   This will load the entire configuration folder if not loaded yet.
%
%       Comp = Component
%       disp(Comp.Config.Data.unittest.Key1)
%
% ### Example
%    Access global configuration without Component:
%    Get the value of 'Key1' inside configuration file 'unittest.yml'.
%    This will load the entire configuration folder if not loaded yet.
%
%       disp(Configuration.getSingleton().Data.unittest.Key1)
%
%
% Create/load configuration into singelton:
%
%       Configuration.loadSysConfig()
%
% Reload entire system configuration into singelton:
%
%       Configuration.reloadSysConfig()
%
%--------------------------------------------------------------------------
% Working with user defined Configuration instances
% (i.e. not using the global singleton object):
%
% Private configuration file, load directly to Data
%
% ### Example: Load unittest.yml to Data.unittest:
%     MyConf = Configuration;
%     MyConf.loadFile('c:/temp/unittest.yml');
%     disp(MyConf.Data.unittest.Key1)
%
% ### Example: Load unittest.yml to Data:
%     MyConf = Configuration;
%     MyConf.loadFile('c:/temp/unittest.yml', 'Field', false);
%     disp(MyConf.Data.Key1)
%
% ### Load entire folder
%
% Suppose you have two files, 'c:/temp/myconfig/unittest.yml'
% and 'c:/temp/myconfig/anothertest.yml':
%
%     MyConf = Configuration;
%     MyConf.loadFolder('c:/temp/myconfig');
%     disp(MyConf.Data.unittest.Key1)
%     disp(MyConf.Data.anothertest.Key1)
%
% Reload single configuration file:
% Use online YAML validators to check your YML files:
%    http://www.yamllint.com/ (Use the GO button...)
%
%--------------------------------------------------------------------------
%
%#docgen
%
% Methods:
%    Configuration - Constructor Intput: * ...,key,val,... 'Name' - Optionall name for the configuration object 'File' - YML file name to load 'Folder' - Folder to load
%    expandFolder - Expand Path with macros from Configuration.System.EnvFolders This functions assume that we already loaded a configuration file called System.yml which has EnvFolders section. Input: Output:
%    internal_convertStruct - Recursive scan and replace data in struct: '@FuncName' -> Function handle 'eval(...)' -> Call to eval, can be used to create object, run any function, etc.
%    internal_loadYaml - Read YAML file to struct, add FileName field Input: FileName - File name of YAML file to be loaded Output: struct with hierarchical data loaded from YAML file Example: MyStruct = Configuration.internal_loadYaml('conf.yml')
%    internal_reloadYaml - Reload configuration file, YamlStruct.FileName property must exist FileName is created by Configuration.loadYaml() on loading. Input: - Output: - Example: - MyStruct = internal_reloadYaml(MyStruct)
%    loadFile - Load specified file to new property inside Obj.Data. When Args.Field is true, a new property based on the file name will be created in Obj.Data Use this function when working with user Confguuration object, or when you want to explictly load/reload specific
%    loadFolder - Load all configuration files inside the specified folder Each file is loaded to Obj.Data.FileName struct. Input: - A Configuration object. - Path - folder name to look for *.yml files * ...,key,val,...
%    reload - Reload all configuration files from folder used by last call to loadFolder(), if Obj.Path is empty, it reloads existing structs by the stored FileName field Example: MyConfig.reloadFolder()
%    reloadFile - Reload specified configuration object (file name) Input: - A Configuration objeect. - YamlStructOrFileName - if char, it specified the file name to be loaded, otherwise it is assumed to be struct that contains also FileName field, and this file is reloaded
%
% Methods: Static
%    getSingleton - Return the singleton Configuration object, this is the 'Global' configuration object of the system Example: Conf = Configuration.getSingleton()
%    getSysConfigPath - Get path to system configuration file, from ASTROPACK_CONFIG_PATH or repository This function is called from MsgLogger and MUST NOT use any msgLog() call! Output: Path of configuration files
%    internal_initSysConfig - **Internal function** Return singleton Configuration object, clear entire configuration if argument is 'clear' This function DOES NOT load any configuration file, just create/clear the object Output: New Configuration object Example: Conf = Configuration.internal_initSysConfig('clear')
%    loadSysConfig - Load entire system configuration, same as getSingleton() Example: Configuration.loadSysConfig()
%    reloadSysConfig - Reload entire system configuration, Warning: calls 'clear java' Example: Configuration.reloadSysConfig()
%    unmacro - @Todo help Replace macros in string with values from struct Input: - Output: - Example: -
%
%#/docgen


classdef Configuration < handle
    % Note that this class is derived from Base and not from Component
    
    % Properties
    properties (SetAccess = public)
        ConfigName              % Optional name for the entire configuration
        Path                    % Path of configuration files, set by
        ExternalPath            % Path to external packages
        
        % Consider making Data read-only property to avoid system-wide
        % modifications to configuration at run-time (27/10/2021)
        Data struct = struct()  % Initialize empty struct, all YML files are added here in tree structure
    end

    %--------------------------------------------------------
    methods % Constructor
        function Obj = Configuration(Args)
            % Constructor
            % Intput: * ...,key,val,...
            %           'Name'   - Optionall name for the configuration object
            %            'File'   - YML file name to load
            %            'Folder' - Folder to load
            % Output:  - A non-singelton configuration object.
            % Example: Conf = Configuration('File', '~/conf/conf.yml')
            arguments
                Args.Name = '';         %
                Args.File = '';         %
                Args.Folder = '';       %
            end
            
            % Constructor: Validate that we have access to 'external/' folder
            % that contains the yaml package.
            % Master Configuration files are located in AstroPack/config
            
            % Set path to yaml external package
            % @Todo: Replace it with env? move to startup.m?
            MyFileName = mfilename('fullpath');
            [MyPath, ~, ~] = fileparts(MyFileName);
            Obj.ExternalPath = fullfile(MyPath, '..', '..', 'external');

            % Validate access to folders, make sure that we will find the yaml package
            assert(~isempty(Obj.ExternalPath));
            assert(isfolder(Obj.ExternalPath));
            
            % Call addpath() only once
            persistent AddPath;
            if isempty(AddPath)
                AddPath = true;
                addpath(Obj.ExternalPath);
            end
            
            % Store name
            if ~isempty(Args.Name)
                Obj.ConfigName = Args.Name;
            end
            
            % Load file or entire folder
            if ~isempty(Args.File)
                Obj.loadFile(Args.File);
            elseif ~isempty(Args.Folder)
                Obj.loadFolder(Args.Folder);
            else
                % do nothing
            end
        end
    end


    methods % Load functions

        function Result = loadFile(Obj, FileName, Args)
            % Load specified file to new property inside Obj.Data.
            % When Args.Field is true, a new property based on the file
            % name will be created in Obj.Data
            % Use this function when working with user Confguuration
            % object, or when you want to explictly load/reload specific
            % file.
            % Input: - A Configuration object.
            %        - FileName - YML file name to load
            %        * ...,key,val,...
            %          'Field'  - true: field with the file name is created instide Obj.Data
            %               i.e., when loading 'MyConf.yml', it will be loaded to
            %               Obj.Data.MyConf...
            %               false: it will be loaded to Obj.Data...
            % Output: true on success
            % Example:
            % 	 MyConfig = Configuration();
            % 	 MyConfig.loadFile('C:/Temp/MyConfig.yml');

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
                % Extract property name from file name
                [~, name, ~] = fileparts(FileName);
                PropName = name;
                if isfield(Obj.Data, PropName)
                    io.msgLog(LogLevel.Debug, 'Property already exist: Data.%s', PropName);
                else
                    io.msgLog(LogLevel.Debug, 'Adding property: %s', PropName);
                end

                % Note: Yml is used below by eval()
                try
                    Yml = Configuration.internal_loadYaml(FileName); %#ok<NASGU>
                    Result = true;
                catch
                    io.msgLog(LogLevel.Error, 'Configuration.loadYaml failed: %s', FileName);
                    Yml = struct; %#ok<NASGU>
                end

                % Load to property inside Data, or directly do Data
                if Args.Field
                    s = sprintf('Obj.Data.%s=Yml;', PropName);
                else
                    s = sprintf('Obj.Data=Yml;');
                end
                eval(s);
            catch
                io.msgStyle(LogLevel.Error, '@error', 'loadFile: Exception: %s', FileName);
            end
        end


        function loadFolder(Obj, Path, Args)
            % Load all configuration files inside the specified folder
            % Each file is loaded to Obj.Data.FileName struct.
            % Input: - A Configuration object.
            %        - Path - folder name to look for *.yml files
            %        * ...,key,val,...
            %          'Recurse' - true: Load files in sub-folders.
            %               Default is false.
            % Example:
            % 	 MyConfig = Configuration();
            % 	 MyConfig.loadFile('C:/Temp/MyConfigFolder');
            arguments
                Obj
                Path
                Args.Recurse = false;       % When true, load also file from sub-folders
            end
            
            Obj.Path = Path;
            io.msgLog(LogLevel.Debug, 'loadFolderInternal: %s', Obj.Path);

            % Scan folder for YML files
            List = dir(fullfile(Path, '*.yml'));
            for i = 1:length(List)
                if ~List(i).isdir
                    FileName = fullfile(List(i).folder, List(i).name);
                    Obj.loadFile(FileName, 'Field', true);
                elseif Args.Recurse
                    Folder = fullfile(List(i).folder, List(i).name);
                    Obj.loadFolder(Folder);
                end
            end
        end


        function reloadFile(Obj, YamlStructOrFileName)
            % Reload specified configuration object (file name)
            % Input: - A Configuration objeect.
            %        - YamlStructOrFileName - if char, it specified the file name
            %          to be loaded, otherwise it is assumed to be struct that contains
            %          also FileName field, and this file is reloaded
            % Output:  Obj.Data... is afected
            % Example: MyConfig.reloadFile(MyConfig.Data.unittest)
            
            if isa(YamlStructOrFileName, 'char')
                Obj.loadFile(YamlStructOrFileName);
            else
                % @Todo: This is probably a bug, we need to update Obj.Data..
                % fix it (22/12/2021)
                Configuration.internal_reloadYaml(Obj.(YamlStructOrFileName));
            end
                
        end


        function reload(Obj)
            % Reload all configuration files from folder used by last call
            % to loadFolder(), if Obj.Path is empty, it reloads existing
            % structs by the stored FileName field
            % Example: MyConfig.reloadFolder()
            
            if ~isempty(Obj.Path)
                loadFolder(Obj, Obj.Path);
            else
                StructList = fieldnames(Obj.Data);
                for i = 1:length(StructList)
                    Configuration.internal_reloadYaml(Obj.Data.(StructList{i}));
                end
            end
        end
        
    end
    
    %----------------------------------------------------------------------
    methods % Utility Functions
        function Result = expandFolder(Obj, Path)
            % Expand Path with macros from Configuration.System.EnvFolders
            % This functions assume that we already loaded a configuration
            % file called System.yml which has EnvFolders section.
            % Input:
            % Output:
            % Example:
            if isfield(Obj.Data, 'System') && isfield(Obj.Data.System, 'EnvFolders')
                Result = Configuration.unmacro(Path, Obj.Data.System.EnvFolders);
            else
                Result = Path;
            end
        end

    end
    
    %======================================================================
    %                          Static Functions
    %======================================================================
    methods (Static)
        function help
            % Show MLX manual
            tools.os.class_mlx(mfilename('fullpath'))
        end
    end
    
    methods(Static) % Static functions to access the Singleton configuration

        function Result = getSingleton()
            % Return the singleton Configuration object, this is the 'Global'
            % configuration object of the system
            % Example: Conf = Configuration.getSingleton()
            Result = Configuration.internal_initSysConfig();
        end

        
        function Result = loadSysConfig()
            % Load entire system configuration, same as getSingleton()
            % Example: Configuration.loadSysConfig()
            Result = Configuration.internal_initSysConfig();
        end

        
        function Result = reloadSysConfig()
            % Reload entire system configuration, Warning: calls 'clear java'
            % Example: Configuration.reloadSysConfig()
            io.msgStyle(LogLevel.Debug, 'red', 'Configuration.reload: Calling "clear java", required until we find better solution');
            clear java;
            Result = Configuration.internal_initSysConfig('clear');
        end
        
                    
        function Result = internal_initSysConfig(varargin)
            % **Internal function**
            % Return singleton Configuration object, clear entire configuration if argument is 'clear'
            % This function DOES NOT load any configuration file, just create/clear the object
            % Output:  New Configuration object
            % Example: Conf = Configuration.internal_initSysConfig('clear')
            persistent Conf

            % Optionally clear configuration
            if numel(varargin) > 0 && strcmp(varargin{1}, 'clear') && ~isempty(Conf)
                io.msgLog(LogLevel.Debug, 'Configuration.init: Clearing Conf');
                Conf.Data = struct();
                %Conf = [];
            end

            % Load/reload entire configuration
            if isempty(Conf)
                io.msgLog(LogLevel.Debug, 'Configuration.init: Creating Conf');
                Conf = Configuration;
            end

            % Load ALL configuration files in Obj.SysConfig/ and Obj.SysConfig/local/
            if isempty(Conf.Data) || numel(fieldnames(Conf.Data)) == 0

                % Get path to config
                Path = Configuration.getSysConfigPath();
                assert(~isempty(Path));
                assert(isfolder(Path));
            
                % Load files in default folder
                Conf.loadFolder(Path);

                % Load local files from local/subfolder
                % These files should be excluded from git (in config/.gitignore)
                Conf.loadFolder(fullfile(Path, 'local'));
                
                % Save the path (it is modified by last call to load 'local')
                Conf.Path = Path;
            end
            Result = Conf;
        end
        
        
        function Result = getSysConfigPath()
            % Get path to system configuration file, from
            % ASTROPACK_CONFIG_PATH or repository
            % This function is called from MsgLogger and MUST NOT use
            % any msgLog() call!
            % Output:  Path of configuration files
            % Example: Path = Configuration.getSysConfigPath()
            
            EnvPath = getenv('ASTROPACK_CONFIG_PATH');
            if ~isempty(EnvPath)
                Path = EnvPath;
                %io.msgLog(LogLevel.Debug, 'Configuration.getSysConfigPath: Using env path: %s', Path);
            else
                % Get full path and name of the file in which the call occurs,
                % not including the filename extension
                MyFileName = mfilename('fullpath');
                [MyPath, ~, ~] = fileparts(MyFileName);
                Path = fullfile(MyPath, '..', '..', '..', 'config');
                %io.msgLog(LogLevel.Debug, 'Configuration.getSysConfigPath: Using git path: %s', Path);
            end
            Result = Path;
        end
    end
    
    %----------------------------------------------------------------------
    % Access=private is experimental (27/10/2021)
    methods(Static,Access=private) % For internal use, calls the yaml package
        
        function YamlStruct = internal_loadYaml(FileName)
            % Read YAML file to struct, add FileName field
            % Input:   FileName - File name of YAML file to be loaded
            % Output:  struct with hierarchical data loaded from YAML file
            % Example: MyStruct = Configuration.internal_loadYaml('conf.yml')
            io.msgLog(LogLevel.Debug, 'loadYaml: Loading file: %s', FileName);
            try
                if ~isfile(FileName)
                    io.msgLog(LogLevel.Error, 'loadYaml: File not found: %s', FileName);
                end
                
                % This do the actual loading
                YamlStruct = yaml.ReadYaml(string(FileName).char);
                
                % Store the file name in FileName property
                YamlStruct.FileName = FileName;
                
                % Convert, recursive function!
                YamlStruct = Configuration.internal_convertStruct(YamlStruct);
            catch
                io.msgStyle(LogLevel.Error, '@error', 'loadYaml: Exception loading file: %s', FileName);
            end
        end

        function NewYamlStruct = internal_reloadYaml(YamlStruct)
            % Reload configuration file, YamlStruct.FileName property must exist
            % FileName is created by Configuration.loadYaml() on loading.
            % Input:   -
            % Output:  -
            % Example: - MyStruct = internal_reloadYaml(MyStruct)
            if isfield(YamlStruct, 'FileName')
                NewYamlStruct = Configuration.internal_loadYaml(YamlStruct.FileName);
            else
                msgLog('loadYaml: reloadYaml: no FileName property');
                NewYamlStruct = YamlStruct;
            end
        end
            
        function Result = internal_convertStruct(Struct)
            % Recursive scan and replace data in struct:
            % '@FuncName' -> Function handle
            % 'eval(...)' -> Call to eval, can be used to create object,
            % run any function, etc.
            %
            % See also:
            %     https://stackoverflow.com/questions/56338151/matlab-recursive-function-to-browse-and-modify-a-structure
            %
            % Input:   Struct - struct returned by internal_loadYaml()
            % Output:  struct with fixed data
            % Example: MyStruct = Configuration.internal_convertStruct(MyStruct)
            fields = fieldnames(Struct);
            for i=1:numel(fields)
                FieldName = fields{i};
                Value = Struct.(FieldName);
                if isstruct(Value)
                    Struct.(FieldName) = Configuration.internal_convertStruct(Value);
                else
                    if ischar(Value)
                                                
                        % Function handle
                        if startsWith(Value, '@')
                            FuncName = Value(2:end);
                            FuncHandle = str2func(FuncName);
                            Struct.(FieldName) = FuncHandle;
                            io.msgLog(LogLevel.Debug, 'Configuration.convert: %s', Value);
                            
                        % Eval (any expression)
                        elseif startsWith(Value, 'eval(')
                            Struct.(FieldName) = eval(Value);
                            io.msgLog(LogLevel.Debug, 'Configuration.convert: %s', Value);
                        end
                    end
                end
            end
            Result = Struct;
        end
        
    end

    %----------------------------------------------------------------------
    methods(Static) % Helper functions

        function Result = unmacro(Str, MacrosStruct)
            % @Todo help
            % Replace macros in string with values from struct
            % Input:   -
            % Output:  -
            % Example: -
            % Example:
            % Str="$Root/abc", MacrosStruct.Root="xyz" -> "xyz/abc"
            % Configuration.unmacro(Component.Config.Data.DarkImage.InputFolder, Component.Config.Data.EnvFolders)

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

    end

    %----------------------------------------------------------------------
    methods(Static) % Unit test

        Result = unitTest()
            % Unit test
    end

end
