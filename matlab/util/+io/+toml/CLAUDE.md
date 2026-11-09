# MATLAB TOML (`io.toml`)

## Purpose

Install, demo, and document TOML file support via MathWorks `matlab-toml-yaml`.
Package namespace: `io.toml`. `@Configuration` loads `.toml` via the same
patterns as `.yml` once `readtoml` is installed.

## Contents

- `install.m` — bundled `.mltbx` install + verify `readtoml`
- `demo.m` — smoke test on `app_config.toml` and `pipeline.toml`
- `app_config.toml`, `pipeline.toml` — sample configs
- `MATLAB_Toolbox_for_TOML_and_YAML.mltbx` — upstream installer
- `INSTALL-TOML-TOOLKIT.md`, `README.md`, `TEAM_EMAIL_TOML_SUPPORT.md`

## Setup

Run `io.toml.install` once per MATLAB user profile (or when replacing the `.mltbx`).

## Run

```matlab
io.toml.demo
```

## Maintenance

- Sample paths: resolve relative to `demo.m` via `mfilename("fullpath")`.
- Keep installer version and SHA-256 in `INSTALL-TOML-TOOLKIT.md` when updating `.mltbx`.
- Configuration TOML tests live in `matlab/base/@Configuration/unitTestToml.m`.
- Upstream: <https://github.com/mathworks/matlab-toml-yaml>
