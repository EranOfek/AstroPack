# Top-level configuration
# Load multiple configuration files as properties
# Load all YML files in folder
# Access each file as property of the Configuration object.


import yaml

with open("example.yaml", 'r') as stream:
    try:
        print(yaml.safe_load(stream))
    except yaml.YAMLError as exc:
        print(exc)



class Configuration:

    def __init__(self):
        self.ConfigName              # Optional name for the entire configuration
        self.Path                    # Path of configuration files
        self.External                # Path to external packages
        self.Data struct = struct()  # Initialize empty struct, all YML files are added here in tree structure

    
    def load(self):

        # Get full path and name of the file in which the call occurs,
        # not including the filename extension
        MyFileName = mfilename('fullpath');
        [MyPath, ~, ~] = fileparts(MyFileName);

        # Set path to configuration files
        # @FFU: overide with env???
        Obj.Path = fullfile(MyPath, '..', '..', 'config');

        # Set path to yaml external package
        # Replace it with env? move to startup.m?
        Obj.External = fullfile(MyPath, '..', 'external');

        fprintf('Configuration Path: %s\n', Obj.Path);
        fprintf('Configuration External: %s\n', Obj.External);
        fprintf('Master Configuration files are located in AstroPack/config\n');

        # Validate
        assert(~isempty(Obj.Path));
        assert(~isempty(Obj.External));
        assert(isfolder(Obj.Path));
        assert(isfolder(Obj.External));

        addpath(Obj.External);



    # Load entire Configuration: all files in folder
    def loadConfig(self):

        # Load default folder
        self.loadFolder(self.Path);

        # Load local files
        self.loadFolder(fullfile(self.Path, 'local'));



    # Load specified file to property
    def loadFile(Obj, FileName):


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
            io.msgStyle(LogLevel.Error, '@error', 'loadFile: Exception: %s', FileName);


    # Load specified folder to properties
    def loadFolder(Obj, Path):

        #@Todo: fix
        #Obj.Path = Path;
        #Obj.ConfigName = 'Config';
        io.msgLog(LogLevel.Info, 'loadFolder: %s', Obj.Path);

        List = dir(fullfile(Path, '*.yml'));
        for i = 1:length(List)
            if ~List(i).isdir
                FileName = fullfile(List(i).folder, List(i).name);
                Obj.loadFile(FileName);


    # Reload specified configuration file
    def reloadFile(Obj, YamlStruct):

        Configuration.reload(Obj.(YamlStruct));



    # Reload all configuration files from folder
    def reloadFolder(Obj):
        loadFolder(Obj, Obj.Path);


    # Expand Path with macros from Configuration.System.EnvFolders
    def expandFolder(Obj, Path):

        if isprop(Obj, 'System') && isfield(Obj.System, 'EnvFolders')
            Result = Configuration.unmacro(Path, Obj.System.EnvFolders);
        else
            Result = Path;


    # Static functions

    def init()
        # Return singleton Configuration object
        persistent Conf
        if isempty(Conf)
            Conf = Configuration;
        end
        Result = Conf;
    end


    def getSingleton():
       #% Return singleton Configuration object
        Conf = Configuration.init();
        if isempty(Conf.Data) || numel(fieldnames(Conf.Data)) == 0
            Conf.loadConfig();
        end
        Result = Conf;
    end


    def loadYaml(FileName):
        # Read YAML file to struct, add FileName field
        io.msgLog(LogLevel.Info, 'loadYaml: Loading file: %s', FileName);
        try
            if ~isfile(FileName)
                io.msgLog(LogLevel.Error, 'loadYaml: File not found: %s', FileName);

            YamlStruct = yaml.ReadYaml(string(FileName).char);
            YamlStruct.FileName = FileName;
        catch
            io.msgStyle(LogLevel.Error, '@error', 'loadYaml: Exception loading file: %s', FileName);


    def reloadYaml(YamlStruct):
        # Reload configurastion file, 'FileName' field must exist
        if isfield(YamlStruct, 'FileName')
            NewYamlStruct = Configuration.loadYaml(YamlStruct.FileName);
        else
            msgLog('loadYaml: reloadYaml: no FileName property');
            NewYamlStruct = YamlStruct;


    # Helper functions

    def unmacro(Str, MacrosStruct):
        # Replace macros in string with values from struct
        # Str="$Root/abc", MacrosStruct.Root="xyz" -> "xyz/abc"
        # conf.unmacro(conf.Yaml.DarkImage.InputFolder, conf.Yaml.EnvFolders)

        FieldNames = fieldnames(MacrosStruct);
        for i = 1:numel(FieldNames)
            Var = FieldNames{i};
            Macro = "$" + Var;
            Value = MacrosStruct.(Var);
            if contains(Str, Macro)
                NewStr = strrep(Str, Macro, Value);
                Str = NewStr;

        Result = Str;


    def getRange(Cell):
        % Get minimum and maximum values from cell array
        % Example: [min, max] = conf.getRange(conf.Yaml.DarkImage.TemperatureRange)
        Min = Cell{1};
        Max = Cell{2};


    def listLen(List):
        % Return list length
        [~, Len] = size(List);



    def listItem(List, Index):
        Value = List;



    def unitTest():
        io.msgLog(LogLevel.Test, 'Configuration test started');

        Conf = Configuration.init();
        assert(~isempty(Conf.Path));
        assert(~isempty(Conf.External));
        assert(isfolder(Conf.Path));
        assert(isfolder(Conf.External));

        fprintf('Conf.Path: %s\n', Conf.Path);
        fprintf('Conf.External: %s\n', Conf.External);

        ConfigPath = Conf.Path;
        ConfigFileName = fullfile(ConfigPath, 'UnitTest.yml');

        # Test low level loading of Yaml struct
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

        # Test Configuration class
        io. msgLog(LogLevel.Test, 'Testing Configuration object');
        Conf.loadFile(ConfigFileName);

        confUnitTest = Conf.Data.UnitTest;

        #
        io.msgLog(LogLevel.Test, 'FileName: %s', confUnitTest.FileName);
        disp(Conf.Data.UnitTest);

        #
        io.msgLog(LogLevel.Test, 'Key1: %s', confUnitTest.Key1);
        io.msgLog(LogLevel.Test, 'Key2: %s', confUnitTest.Key2);
        io.msgLog(LogLevel.Test, 'Key: %s', confUnitTest.Key0x2D3);
        io.msgLog(LogLevel.Test, 'Key: %s', confUnitTest.x0x2DKeyMinus);

        #disp(conf.listLen(conf.UnitTest.NonUniqueKeys));

        # Load all config files in folder
        io.msgLog(LogLevel.Test, 'Testing folder');
        Conf.loadFolder(ConfigPath);
        disp(Conf.Data.System.EnvFolders);

        io.msgLog(LogLevel.Test, 'Testing utility functions');
        io.msgLog(LogLevel.Test, 'unmacro: %s', Configuration.unmacro("$ROOT/abc", Conf.Data.System.EnvFolders));

        io.msgLog(LogLevel.Test, 'expandFolder: %s', Conf.expandFolder("$ROOT/abc"));

        # Done
        io.msgLog(LogLevel.Test, 'Configuration test passed');
        Result = true;
