# On-demand pipeline — user guide

**Audience:** Science Portal developers (Python) and MATLAB scientist developers  
**Updated:** November 2026  
**Ownership:** Platform team maintains workers, inbox, and bridge adaptation. Scientist developers implement processing only in the two handler files listed below.

This guide explains how jobs move through a **shared network folder**. It does not cover low-level IPC implementation (claiming, archiving, or internal classes). For exact JSON fields, see `SPEC.md` in this folder.

---

## 1. What this is

The on-demand pipeline runs occasional heavy jobs—**forced photometry** and **custom coadd**—without tying them to the SOC internal exchange tree. Instead, everyone agrees on one share root:

**`ON_DEMAND_PATH`**

The Science Portal (or any Python tool) **writes job files on the share**. One or more **MATLAB worker processes** read a small trigger from an **inbox**, run science, and write results back on the share. Communication is **file-based** (no sockets between portal and MATLAB). The pattern is similar to TOO planner jobs that use a share plus a short JSON trigger.

Job types live in separate subtrees under `ON_DEMAND_PATH` (for example `forced_photometry` and `custom_coadd`).

---

## 2. How it works (high level)

```
  Science Portal (Python)
        |
        |  (1) Create work folder: request.json, inputs, optional status.json
        |  (2) Drop short trigger JSON into type/inbox/
        v
  Shared folder (ON_DEMAND_PATH on Euclid or equivalent mount)
        |
        |  (3) MATLAB worker picks up trigger, runs handler, writes outputs
        v
  Science Portal
        |
        |  (4) Poll inbox/<trigger>.out.json and/or work/.../status.json, result.json
```

**Multiple MATLAB workers** of the same type may run at once (for load or separate hosts). The platform ensures one worker processes each trigger; portal developers do not need to coordinate workers manually.

**Network (typical setup):** The portal VM in the DMZ can write to a share on Euclid. MATLAB workers may run on Euclid or on internal hosts, as long as they mount the **same** `ON_DEMAND_PATH`. The share is the integration boundary—not arbitrary paths on internal disks.

---

## 3. Shared folder layout

Under `ON_DEMAND_PATH`, each processing type has its own subtree:

```text
ON_DEMAND_PATH/
  forced_photometry/
    inbox/                    short trigger *.json files (portal drops here)
    work/yyyy/mm/dd/
      <request_id>/           one folder per job
    processed/                archived triggers (platform)
    forced_photometry.watchdog
  custom_coadd/
    inbox/
    work/yyyy/mm/dd/<request_id>/
    ...
```

Each **request** gets its own folder under `work/yyyy/mm/dd/<request_id>/`. That folder holds inputs, the full job specification, and outputs. Think of it like one alert stage folder on the GW coverage experiment share: everything for that job stays together.

### Files you will see

| File | Who writes it | Purpose |
|------|----------------|---------|
| `work/.../request.json` | Portal (via Python client) | Full job parameters for MATLAB science |
| `inbox/<request_id>.json` | Portal | Short trigger: action, paths to work folder and request file |
| `work/.../status.json` | Portal (optional initial), worker (updates) | Job state: queued, running, ok, error |
| `work/.../result.json` | Worker | Durable result summary and output paths |
| `inbox/<request_id>.out.json` | Worker | Short completion reply for the portal |

**Path rule:** Every path inside JSON must use **forward slashes**, including UNC (`//server/share/path/...`). Do not use backslashes in JSON.

---

## 4. For Science Portal developers

### What you need

One Python module in the AstroPack repo (copy into the portal repo if that is easier):

`matlab/astro/+ultrasat/+services/+on_demand/python_client/on_demand_client.py`

That file contains the class **`OnDemandClient`**. You do not need to implement inbox logic yourself.

### Setup

1. Set environment variable **`ON_DEMAND_PATH`** to the share root (same value workers use).
2. Ensure a MATLAB worker is already running for your job type (see section 6). The portal does **not** start MATLAB.
3. Optional: set `ON_DEMAND_CHLOG=1` and `CHLOG_CONFIG` if your site uses ClickHouse logging.

### Typical usage

```python
from on_demand_client import OnDemandClient

client = OnDemandClient()  # reads ON_DEMAND_PATH

# Smoke test
health = client.health_check("forced_photometry")

# Submit a job
work = {
    "action": "forced_photometry",
    "request_id": "...",  # optional; client can allocate
    "image_file": "//euclid/share/.../reference.fits",
    "sources": [{"ra_deg": 12.3, "dec_deg": -4.5, "name": "s1"}],
}
handle = client.submit_job("forced_photometry", work, timeout_sec=3600)
result = client.wait_for_job(handle, timeout_sec=3600)
client.close()
```

`submit_job` creates the work directory, writes `request.json`, and drops the inbox trigger. `wait_for_job` waits for `.out.json` in the inbox (and can fall back to `status.json` in the work folder).

### Demo script in git

For a command-line smoke test (health and optional dry-run job):

`matlab/astro/+ultrasat/+services/+on_demand/python_client/demo_on_demand_client.py`

Run from the `python_client` directory or add it to `PYTHONPATH`. See `python_client/README.md` for flags.

### Testing without real science

Put **`"dry_run": true`** in the workdir `request.json` body you pass to `submit_job`. The MATLAB handler will validate fields and return a mock success. Use this to test portal integration before scientists finish the real algorithms.

### More detail

- `python_client/README.md` — env vars and examples  
- `docs/SPEC.md` — required JSON fields per job type  
- `examples/` — sample trigger and request files  

The platform team will adapt any **bridge** or service wrappers; your integration point remains **`on_demand_client.py`** and `ON_DEMAND_PATH`.

---

## 5. For MATLAB scientist developers

### What you implement

You only need to maintain **science logic** in these two files:

| File (under `+on_demand/`) | Job type |
|------------------------------|----------|
| `processForcedPhotometry.m` | `forced_photometry` |
| `processCustomCoadd.m` | `custom_coadd` |

Do **not** need to change: `on_demand_service.m`, `OnDemandFileIpc.m`, `processRequest.m`, validators, status/result writers, or the Python client. The platform owns those.

### What the handler shell already does

When the worker runs your handler, the surrounding code:

1. Loads **`request.json`** from the path in the inbox trigger.
2. Sets **`status.json`** to `running`, then `ok` or `error`.
3. Validates required fields (validators next to the handlers).
4. If **`dry_run`** is true in `request.json`: writes a mock **`result.json`** and returns success (for portal tests).
5. If not dry run and science is not implemented yet: returns a clear **error** message until you add real processing.
6. Fills the short **`.out.json`** reply in the inbox with `status`, `message`, and paths to `result.json` / `status.json`.

Your job is to replace the stub block with real calls into AstroPack (for example image loading and `imProc` for forced photometry; coadd pipeline TBD).

### How to implement science

1. After validation, read inputs from the **`WorkReq`** struct (decoded `request.json`).
2. Open files with **`ultrasat.tools.localPathForIo`**; write paths in JSON with **`ultrasat.tools.pathForJson`**.
3. Write products under the **work folder** (or a subfolder named in the request).
4. Build **`result.json`** (the handler helpers already show the pattern).
5. Return **`status: ok`** only when processing succeeded; otherwise **`status: error`** with a helpful `message`.

Keep the core steps in functions that can be tested **without** running the long-lived worker loop (same idea as `TooPlannerRunner` for TOO).

### Local debugging

Run handlers directly:

```matlab
ultrasat.services.on_demand.debug.debug_processRequest()
```

For a full inbox tick with a temp folder:

```matlab
ultrasat.services.on_demand.debug.debug_onDemandIpcTick()
```

Point **`ON_DEMAND_PATH`** at a test share, run **`on_demand_service`** for your type, and submit jobs from the Python demo or client.

### Request shape

See **`docs/SPEC.md`** section 10 and the JSON files in **`examples/`** (for example `02_forced_photometry_request.json`). Do not duplicate those tables here.

---

## 6. Running MATLAB workers (operations)

Someone with access to the processing host must start a worker **per job type** (or as your deployment standard dictates):

```matlab
ultrasat.services.on_demand.on_demand_service('Type', 'forced_photometry')
ultrasat.services.on_demand.on_demand_service('Type', 'custom_coadd')
```

Optional: copy `config/on_demand.*.example.json` to  
`ON_DEMAND_PATH/<type>/worker_config.json` for retention and timeouts.

A **watchdog** file on the share is touched while the worker is alive; operations can use it for monitoring.

---

## 7. Who does what

| Role | Responsibility |
|------|----------------|
| Science Portal (Python) | Submit jobs via `OnDemandClient`; poll results; optional portal DB |
| MATLAB scientists | Implement `processForcedPhotometry.m` and `processCustomCoadd.m` |
| Platform / SOC | Workers, share layout, bridge, deployment, `ON_DEMAND_PATH` |
| Operations | Keep workers running, mount share on DMZ and worker hosts |

---

## 8. Related documentation

| Document | Location in repo |
|----------|------------------|
| This user guide | `matlab/astro/+ultrasat/+services/+on_demand/docs/USER_GUIDE.md` |
| Technical overview | `.../+on_demand/docs/README.md` |
| Normative JSON / IPC spec | `.../+on_demand/docs/SPEC.md` |
| Python client readme | `.../+on_demand/python_client/README.md` |
| Sample JSON | `.../+on_demand/examples/` |

---

_End of user guide._