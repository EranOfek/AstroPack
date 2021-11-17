function Result = unitTest()
    % Configuration.unitTest
    
    io.msgLog(LogLevel.Test, 'Configuration test started');

    % Clear java to avoid failure of yaml.ReadYaml()
    clear java;
    FileName = 'UnitTest';
    
    % Low level test
    TestLowLevel = false;
    if TestLowLevel 
        TestPath = 'D:\Ultrasat\AstroPack.git\config';
        TestFileName = fullfile(TestPath, 'UnitTest.yml');
        YamlStruct = yaml.ReadYaml(string(TestFileName).char);
        YamlStruct = Configuration.internal_convertStruct(YamlStruct);    
        yml = Configuration.internal_loadYaml(TestFileName);
    end
    
    ConfigPath = Configuration.getSysConfigPath();
    assert(~isempty(ConfigPath));
    ConfigFileName = fullfile(ConfigPath, strcat(FileName, '.yml'));
    
    % Test low level loading of Yaml struct
    io.msgLog(LogLevel.Test, 'Testing low level functions');
    yml = Configuration.internal_loadYaml(ConfigFileName);
    uTest = yml;
    io.msgLog(LogLevel.Test, 'Key1: %s', uTest.Key1);
    io.msgLog(LogLevel.Test, 'Key2: %s', uTest.Key2);
    io.msgLog(LogLevel.Test, 'Key: %s', uTest.Key0x2D3);
    io.msgLog(LogLevel.Test, 'Key: %s', uTest.x0x2DKeyMinus);
    yml = Configuration.internal_reloadYaml(yml);
    uTest = yml;
    io.msgLog(LogLevel.Test, 'Key1: %s', uTest.Key1);                  
    
    %----------------------------------------------------------------------
    % Initialize and get a singletone persistant object
    Conf = Configuration.getSingleton();
    assert(~isempty(Conf.Path));
    assert(~isempty(Conf.ExternalPath));
    assert(isfolder(Conf.Path));
    assert(isfolder(Conf.ExternalPath));            

    fprintf('Conf.Path: %s\n', Conf.Path);
    fprintf('Conf.External: %s\n', Conf.ExternalPath);

    % Private configuration file, load directly to Data
    PrivateConf = Configuration;
    % Field = false will load only the ConfigFileName into the Data
    % without the full struct
    PrivateConf.loadFile(ConfigFileName, 'Field', false);
    assert(~isfield(PrivateConf.Data, FileName));
    assert(isfield(PrivateConf.Data, 'Key1'));            

    % Private configuration file, load to Data.UnitTest
    PrivateConf2 = Configuration;
    PrivateConf2.loadFile(ConfigFileName);          
    assert(isfield(PrivateConf2.Data, FileName));
    assert(isfield(getfield(PrivateConf2.Data, FileName), 'Key1'));

    %----------------------------------------------------------------------

    % Test Configuration class
    io. msgLog(LogLevel.Test, 'Testing Configuration object');

    confUnitTest = PrivateConf2.Data.(FileName);

    %
    io.msgLog(LogLevel.Test, 'FileName: %s', confUnitTest.FileName);
    disp(confUnitTest);         

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
    %io.msgLog(LogLevel.Test, 'unmacro: %s', Configuration.unmacro("$Key1/abc", confUnitTest));
    assert(strcmp(Configuration.unmacro("$Key1/abc", confUnitTest),"Value1/abc"))

    %io.msgLog(LogLevel.Test, 'expandFolder: %s', Conf.expandFolder("$ROOT/abc"));
    assert(~strcmp(Conf.expandFolder("$ROOT/abc"),"$ROOT/abc"))

    % Done
    io.msgLog(LogLevel.Test, 'Configuration test passed');
    Result = true;
end
