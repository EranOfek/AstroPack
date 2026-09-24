# API Core Classes

Base classes for logging and configuration used across `+api`, `+planner/+guiutils`, and services.

Parent: [[../README|+api README]]

---

## `Loggable.m`

Base class providing structured logging to console and log file.

### Key properties

| Property | Role |
|----------|------|
| `LogFilePath` | Full path to log file (resolved at construction) |
| `LogPrefix` | Class name prefix in log lines (subclasses should set) |
| `LogBasePath` | From `SOC_PATH` environment variable |

**Requires `SOC_PATH`** — constructor errors if not set.

### Key methods

| Method | Role |
|--------|------|
| `msglog` / `msgLog` | Log info/warning messages |
| `msgEx` / `msgex` | Log exceptions |
| `logException` | Structured exception logging |
| `checkErrorAndLogExtra` | Check error struct and log details |

---

## `Config.m`

Configuration handle class. Used by `Component` hierarchy for path and settings access.

---

## Inheritance chain

```
Loggable
├── ClientBase              (+clients/)
├── MainModule              (+planner/+guiutils/)
├── PlannerMain*Helper      (+planner/+guiutils/)
├── TooPlannerRunner        (+planner/)
└── VirtualTimeModels       (+models/)
```

`ClientBase` extends `Loggable` and adds `postRequest()` for HTTP.

---

## Usage convention

Subclasses set `LogPrefix` in their constructor:

```matlab
Obj.LogPrefix = 'PlansManagerClient';
```

Log files are written under `$SOC_PATH/log/` (path resolved by `Loggable`).

---

_Last updated: 2026-06_
