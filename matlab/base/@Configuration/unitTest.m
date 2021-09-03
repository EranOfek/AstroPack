function Result = unitTest()
    % Configuration.unitTest
    
    io.msgLog(LogLevel.Test, 'Configuration test started');

    % Clear java to avoid failure of yaml.ReadYaml()
    clear java;

    % Initialize and get a singletone persistant object
    Conf = Configuration.init();
    assert(~isempty(Conf.Path));
    assert(~isempty(Conf.External));
    assert(isfolder(Conf.Path));
    assert(isfolder(Conf.External));            

    fprintf('Conf.Path: %s\n', Conf.Path);
    fprintf('Conf.External: %s\n', Conf.External);

    ConfigPath = Conf.Path;
    ConfigFileName = fullfile(ConfigPath, 'UnitTest.yml');

    % Private configuration file, load directly to Data
    PrivateConf = Configuration;
    % Field = false will load only the ConfigFileName into the Data
    % without the full struct
    PrivateConf.loadFile(ConfigFileName, 'Field', false);
    assert(~isfield(PrivateConf.Data, 'UnitTest'));
    assert(isfield(PrivateConf.Data, 'Key1'));            

    % Private configuration file, load to Data.UnitTest
    PrivateConf2 = Configuration;
    PrivateConf2.loadFile(ConfigFileName);          
    assert(isfield(PrivateConf2.Data, 'UnitTest'));
    assert(isfield(PrivateConf2.Data.UnitTest, 'Key1'));

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
