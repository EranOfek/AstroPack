Subject: AstroPack: TOML support added (YAML unchanged)

Hi team,

We added **TOML** as an optional configuration format in AstroPack. All existing
**YAML** files under `config/` stay as they are — nothing to migrate unless you
choose TOML for new or personal settings.

**Why TOML for user config?**

- Straightforward syntax: explicit `key = value`, less indentation sensitivity than YAML
- Nested settings are clear (`[section]` / `[[array_of_tables]]`)
- Native types (bool, numbers, datetimes, arrays) without YAML quoting surprises
- Easy to review in git and edit by hand

**What we added**

- Folder: `matlab/util/+io/+toml` (`io.toml` package)
- Bundled MathWorks **matlab-toml-yaml** toolkit (`.mltbx` in repo)
- `@Configuration` can load `.toml` files the same way as `.yml` (extension decides loader)

**One-time install per MATLAB profile**

```matlab
io.toml.install
which readtoml
```

**Quick check**

```matlab
io.toml.demo
Configuration.unitTestToml
```

You should see: `PASS: both TOML files were read and all smoke tests passed.`

**Using TOML in code**

```matlab
cfg = readtoml("settings.toml");
MyConf = Configuration;
MyConf.loadFile("C:\path\settings.toml");
```

Docs: `matlab/util/+io/+toml/README.md` and `INSTALL-TOML-TOOLKIT.md`.

Questions — ping me.

Thanks,
[Your name]
