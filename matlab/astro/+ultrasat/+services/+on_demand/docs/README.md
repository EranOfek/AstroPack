# On-demand pipeline (MATLAB worker)

**User guide (portal + scientists):** [USER_GUIDE.md](USER_GUIDE.md) — start here for how the shared folder and file-IPC work.

Long-running MATLAB workers for **on-demand scientific processing** over a shared filesystem. Unlike SOC exchange workers under `$SOC_PATH/runtime/exchange/`, this service uses **`ON_DEMAND_PATH`** on a share reachable from DMZ (portal) and internal hosts.

Project-agnostic: clients may be ULTRASAT Science Portal, LAST pipelines, or other tools. Processing kinds are isolated under separate subtrees (`forced_photometry`, `custom_coadd`, …).

**Status (2026-11):** MATLAB IPC loop and validate-only handlers ship in this package; science (`imProc` / coadd) is not wired yet. Set `dry_run: true` in workdir `request.json` for integration tests. See [SPEC.md](SPEC.md) for the normative contract.

Diagram: [../diagrams/on_demand_flow.mmd](../diagrams/on_demand_flow.mmd)

---

## Purpose

- Run **heavy or ad-hoc** jobs (forced photometry, custom coaddition, future types) without coupling to MCS/SOC internal runtime trees.
- **One flat inbox per processing type** — workers poll a single folder; no recursive tree walk.
- **Producer creates the workdir** and places a short trigger in `inbox/` (same pattern as TOO planner / alerts filter: large inputs live on the share, IPC stays small).
- **Multi-worker** support via claim-by-rename on inbox triggers.
- **Per-type TTL** for workdirs and archived triggers (optional “keep forever”).

---

## Layout (under `ON_DEMAND_PATH`)

```text
$ON_DEMAND_PATH/
├── forced_photometry/
│   ├── inbox/
│   ├── processed/yyyy/mm/dd/
│   ├── work/yyyy/mm/dd/<request_id>/
│   └── forced_photometry.watchdog
└── custom_coadd/
    ├── inbox/
    ├── processed/yyyy/mm/dd/
    ├── work/yyyy/mm/dd/<request_id>/
    └── custom_coadd.watchdog
```

Example workdir (compare to GW experiment stages):

```text
//socsrv/rwshare_data/.../2023/05/29/S230529ay/
```

Here the date + `request_id` folder holds all inputs, `request.json`, outputs, and `status.json`.

---

## Actions

| `action` | Handler | Worker inbox |
|----------|---------|----------------|
| `health` | inline in `processRequest` | any configured type |
| `forced_photometry` | `processForcedPhotometry.m` | `forced_photometry/inbox/` |
| `custom_coadd` | `processCustomCoadd.m` | `custom_coadd/inbox/` |
| `custom_coaddition` | alias of `custom_coadd` | same |

Shared dispatcher: `processRequest.m` dispatches by `action` to a type-specific file (see `+slew_calc/processRequest.m`).

---

## Path rule

All paths in JSON (`request.json`, inbox triggers, `status.json`, `result.json`, `.out.json`) use **forward slashes** (`/`). UNC paths use the form `//host/share/...`. Local I/O uses `ultrasat.tools.localPathForIo`; serialization uses `ultrasat.tools.pathForJson`.

---

## Network and deployment

- **Producers** (Euclid, portal VM in WIS DMZ) create workdirs and write triggers to the **DMZ-visible share**.
- **Workers** may run on Euclid, another DMZ host, or inside the WIS internal network, as long as they mount the same `ON_DEMAND_PATH`.
- DMZ hosts **cannot** write to arbitrary WIS-internal disks; the share is the integration boundary. Where the worker reads/writes is defined entirely by paths in the request — the inbox only carries pointers.

No MATLAB database: history and orchestration beyond the folder tree are optional at the Science Portal / client layer.

---

## MATLAB package files

| File | Role |
|------|------|
| `on_demand_service.m` | Entry; `Type` selects inbox; claim + IPC loop |
| `OnDemandFileIpc.m` | `JsonFileIpc` subclass with claim / reclaim / work TTL |
| `processRequest.m` | Dispatch `health`, `forced_photometry`, `custom_coadd` |
| `processForcedPhotometry.m` | Validate + `dry_run`; science TBD |
| `processCustomCoadd.m` | Validate + `dry_run`; science TBD |

---

## Configuration

Per-type JSON on the share as `worker_config.json` or pass `ConfigFile` — see [../config/](../config/) and [SPEC.md](SPEC.md) §7.

---

## Examples

| File | Description |
|------|-------------|
| `../examples/01_health.json` | Health trigger |
| `../examples/02_forced_photometry_trigger.json` | Inbox trigger |
| `../examples/02_forced_photometry_request.json` | Workdir `request.json` (add `"dry_run": true` for tests) |
| `../examples/03_custom_coadd_trigger.json` | Inbox trigger |
| `../examples/03_custom_coadd_request.json` | Workdir `request.json` |

---

## Environment

| Variable | Purpose |
|----------|---------|
| `ON_DEMAND_PATH` | Root of all on-demand subtrees (required for workers) |
| `ASTROPACK_DATA_PATH` | Calibrations / grids as needed by handlers |
| `SOC_PATH` | **Not** used for on-demand exchange (MCS logs may still use SOC elsewhere) |

---

## Python client (Science Portal)

Single-file producer: [../python_client/on_demand_client.py](../python_client/on_demand_client.py). See [../python_client/README.md](../python_client/README.md).

Optional logging: `ON_DEMAND_CHLOG=1` (Python and MATLAB); MATLAB also respects `MsgLogger.UseChLogger`.

## Running

```matlab
ultrasat.services.on_demand.on_demand_service('Type', 'forced_photometry')
ultrasat.services.on_demand.on_demand_service('Type', 'custom_coadd')
```

Debug (no long-running loop):

```matlab
ultrasat.services.on_demand.debug.debug_processRequest()
ultrasat.services.on_demand.debug.debug_onDemandIpcTick()
```

---

## Cross-references

- Normative spec: [SPEC.md](SPEC.md)
- Services index: [../../README.md](../../README.md)
- LAST forced photometry (library, not this IPC): `matlab/image/+imProc/+sources/forcedPhot.m`

_Last updated: 2026-11_