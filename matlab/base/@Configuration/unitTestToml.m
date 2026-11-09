function Result = unitTestToml()
% Configuration.unitTestToml — TOML loading unit test
% Requires readtoml (io.toml.install). Does not modify or depend on config/*.yml.

if isempty(which('readtoml'))
    error('Configuration:unitTestToml:MissingToolbox', ...
        'readtoml was not found. Run io.toml.install (matlab/util/+io/+toml).');
end

MyFileName = mfilename('fullpath');
[MyPath, ~, ~] = fileparts(MyFileName);
ConfigFileName = fullfile(MyPath, 'UnitTestToml.toml');
assert(isfile(ConfigFileName), 'Fixture missing: %s', ConfigFileName);

FileName = 'UnitTestToml';

toml = Configuration.internal_loadToml(ConfigFileName);
assert(strcmp(toml.Key1, 'Value1'));
assert(strcmp(toml.FileName, ConfigFileName));
assert(toml.Enabled == true);
assert(toml.Retries == 3);
assert(strcmp(string(toml.nested.Item), "nestedValue"));

toml = Configuration.internal_reloadYaml(toml);
assert(strcmp(toml.Key1, 'Value1'));

PrivateConf = Configuration;
PrivateConf.loadFile(ConfigFileName, 'Field', false);
assert(isfield(PrivateConf.Data, 'Key1'));
assert(~isfield(PrivateConf.Data, FileName));

PrivateConf2 = Configuration;
PrivateConf2.loadFile(ConfigFileName);
assert(isfield(PrivateConf2.Data, FileName));
confUnitTest = PrivateConf2.Data.(FileName);
assert(strcmp(confUnitTest.Key1, 'Value1'));
assert(isequal(confUnitTest.List1(:), ["Item1"; "Item2"; "Item3"]) || ...
    iscell(confUnitTest.List1));

Result = true;
end
