# Install the MATLAB TOML toolkit

The installer is bundled in this folder:

`MATLAB_Toolbox_for_TOML_and_YAML.mltbx`

Source:
[`mathworks/matlab-toml-yaml` latest release](https://github.com/mathworks/matlab-toml-yaml/releases/latest)
(downloaded 2026-11-09).

- Toolbox version: 0.1.5
- Required MATLAB release: R2022b or later
- File size: 1,742,767 bytes
- SHA-256: `8E1D77116A829DF0A24B88D0CF7A6442249EE9A9FDD347AADA43DE090D19D799`

## Preferred: AstroPack installer

With `matlab/util` on the path:

```matlab
io.toml.install
```

## Manual install from MATLAB

```matlab
toolboxFile = fullfile(pwd, "MATLAB_Toolbox_for_TOML_and_YAML.mltbx");
matlab.addons.toolbox.installToolbox(toolboxFile)
```

Or double-click the `.mltbx` in the Current Folder browser.

## Verify

```matlab
which readtoml
io.toml.demo
```

Expected final line from the demo:

```text
PASS: both TOML files were read and all smoke tests passed.
```

List installed toolboxes:

```matlab
matlab.addons.toolbox.installedToolboxes
```
