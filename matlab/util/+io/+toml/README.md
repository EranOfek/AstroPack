# TOML support in AstroPack (`io.toml`)

This folder provides installation, examples, and smoke tests for reading TOML
configuration files with MathWorks'
[`matlab-toml-yaml`](https://github.com/mathworks/matlab-toml-yaml) toolbox.
AstroPack's `@Configuration` class can load `.toml` files in addition to
existing `.yml` configs; system YAML under `config/` is unchanged.

## Files

- `install.m` — installs the bundled `.mltbx` (`io.toml.install`)
- `demo.m` — reads sample TOML files and runs smoke tests (`io.toml.demo`)
- `app_config.toml` — scalars, arrays, nested tables, datetime, hyphenated keys
- `pipeline.toml` — Windows paths, nested options, array of tables
- `MATLAB_Toolbox_for_TOML_and_YAML.mltbx` — bundled upstream installer
- `INSTALL-TOML-TOOLKIT.md` — version, checksum, verification
- `TEAM_EMAIL_TOML_SUPPORT.md` — draft team announcement

## Install (once per MATLAB profile)

Ensure `matlab/util` is on the MATLAB path, then:

```matlab
io.toml.install
which readtoml
```

See [INSTALL-TOML-TOOLKIT.md](INSTALL-TOML-TOOLKIT.md) for GUI install and checksum.

## Run the demo

```matlab
io.toml.demo
```

Or by path:

```matlab
run("C:\Ultrasat\AstroPack\matlab\util\+io\+toml\demo.m")
```

Successful execution ends with:

```text
PASS: both TOML files were read and all smoke tests passed.
```

## Use in application code

**Direct read** (toolbox API):

```matlab
config = readtoml("my_config.toml");
port = config.server.port;
configStruct = struct(config);
```

**Configuration class** (same pattern as YAML):

```matlab
MyConf = Configuration;
MyConf.loadFile("C:\path\my_settings.toml");
disp(MyConf.Data.my_settings.Key1)
```

## API patterns (toolbox)

```matlab
config = readtoml("app_config.toml");

% Normal keys: dot notation
port = config.server.port;

% Hyphenated keys: dynamic field names
backend = config.("build-system").("build-backend");

% Datetimes are MATLAB datetime by default
created = config.created_at;
configAsText = readtoml("app_config.toml", DatetimeType="string");

configStruct = struct(config);
```

Upstream docs:
[`readtoml`](https://github.com/mathworks/matlab-toml-yaml/blob/main/toolbox/doc/readtoml.md).
