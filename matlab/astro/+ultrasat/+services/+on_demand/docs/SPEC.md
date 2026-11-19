# On-demand pipeline — specification

Normative contract for file-based on-demand processing. Implementation must follow this document; gaps are filled in versioned revisions of `SPEC.md`.

Package: `ultrasat.services.on_demand`  
Runtime root: environment variable **`ON_DEMAND_PATH`** (not `$SOC_PATH/runtime/exchange`).

Workdir `request.json` may include **`dry_run`: true** for IPC integration tests (mock `result.json`, no science).

---

## 1. Terms

| Term | Meaning |
|------|---------|
| **Processing type** | A named pipeline family (`forced_photometry`, `custom_coadd`, …). Owns `inbox/`, `work/`, `processed/`, watchdog. |
| **Trigger** | Small JSON file dropped in `inbox/`. Drives IPC; references the workdir. |
| **Workdir** | `work/yyyy/mm/dd/<request_id>/` — inputs, canonical `request.json`, outputs, `status.json`. |
| **Worker** | Long-running MATLAB process polling one type’s `inbox/`. |
| **Claim** | Atomic rename of a trigger so only one worker processes it. |

---

## 2. Directory layout

Let `ROOT = ON_DEMAND_PATH` (no trailing slash in documentation; paths in JSON use `/`).

```text
ROOT/
└── <type>/
    ├── inbox/
    ├── processed/
    │   └── yyyy/mm/dd/          # UTC
    ├── work/
    │   └── yyyy/mm/dd/
    │       └── <request_id>/
    └── <type>.watchdog
```

`<type>` is a lowercase identifier matching the primary `action` (`forced_photometry`, `custom_coadd`).

### 2.1 Workdir contents (producer)

Before dropping the trigger, the producer MUST:

1. Create `ROOT/<type>/work/yyyy/mm/dd/<request_id>/` (UTC date).
2. Write **`request.json`** (full job specification).
3. Copy or reference input files under that folder (or use absolute paths on the share in `request.json`).
4. Write initial **`status.json`** with `"state": "queued"` (recommended).

After the worker accepts the job it sets `"state": "running"`; on completion `"ok"` or `"error"`.

### 2.2 Trigger file (inbox)

Filename: unique, stable; recommended `HH-MM-SS-<NNNN>.json` or `<request_id>.json` where `request_id` matches the workdir folder name.

The trigger MUST include at least:

| Field | Required | Description |
|-------|----------|-------------|
| `action` | yes | Processing type (`forced_photometry`, `custom_coadd`, or `health` for local checks) |
| `request_id` | yes | Same as workdir basename |
| `request_file` | yes | Path to workdir `request.json` |
| `work_folder` | yes | Path to workdir |
| `timeout_sec` | no | Client hint; worker may enforce processing limit |

The worker MUST NOT depend on inbox layout beyond the trigger fields and its configured `ROOT/<type>/inbox/`.

### 2.3 Response files

| Artifact | Location | Purpose |
|----------|----------|---------|
| **IPC output** | `inbox/<trigger_basename>.out.json` | Compact API response next to archived trigger |
| **Durable result** | `work_folder/result.json` | Full result envelope for portal replay |
| **Status** | `work_folder/status.json` | Lifecycle for clients polling workdir only |

Write order: update `status.json` → write `result.json` (if any) → write `.out.json.tmp` → rename to `.out.json` (atomic).

---

## 3. Request identifiers

Preferred: **`HH-MM-SS-<NNNN>`** (UTC) with 4-digit daily counter, unique under `work/yyyy/mm/dd/`.

Alternative: UUID v4 as `<request_id>` (allowed; producers must still use one folder per id).

---

## 4. Path serialization

- JSON strings that denote file or directory paths MUST use `/` only.
- UNC: `//server/share/path` (never backslashes).
- Implementation: serialize with `ultrasat.tools.pathForJson`; open files with `ultrasat.tools.localPathForIo`.

---

## 5. Claim and multi-worker semantics

SOC `JsonFileIpc` does not claim; on-demand workers MUST claim before processing.

### 5.1 Eligible files

In `inbox/`, consider only files matching `*.json` EXCEPT:

- `*.out.json`
- `*.tmp`
- `*.claimed.*`
- `*.claiming.*`

### 5.2 Claim sequence

1. Worker `W` selects the oldest eligible trigger (by filename sort or mtime — document choice per deployment; default filename ascending).
2. Two-phase rename (recommended on SMB/NFS):
   - `T.json` → `T.claiming.W.json`
   - `T.claiming.W.json` → `T.claimed.W.json`
3. If any rename fails, another worker owns the file or it disappeared — skip.
4. Set workdir `status.json` to `running` with `worker_id: W` and `claimed_at` ISO-8601 UTC.

`W` is a stable string per process (hostname + pid, or configured `WorkerId`).

### 5.3 Completion

1. Run handler (`processForcedPhotometry` / `processCustomCoadd`).
2. Write `result.json`, final `status.json`.
3. Write `T.out.json` via temp file in `inbox/`.
4. Move `T.claimed.W.json` to `processed/yyyy/mm/dd/` (UTC). If move fails, log and add to skip-list; do not reprocess indefinitely.

### 5.4 Claim timeout and reclaim

Config: `ClaimTimeoutSec` per type (see section 7).

If `now - claimed_at > ClaimTimeoutSec` on a `*.claimed.*` file still in `inbox/`:

1. Another worker MAY rename `T.claimed.W.json` back to `T.json` (or `T.retry.json`).
2. Set workdir `status.json` to `queued` with `reclaim_count` incremented and optional `last_error`.

Reclaim MUST NOT run if workdir `status.json` is `running` and `heartbeat_at` is newer than `ClaimTimeoutSec` (optional future: worker touches heartbeat during long jobs).

### 5.5 Multiple workers per type

Allowed: N processes poll the same `ROOT/<type>/inbox/`. Claim rules prevent duplicate processing.

### 5.6 Multiple types

Separate inboxes — deploy one worker binary/config per type during phased development.

---

## 6. Dispatcher and handlers

Entry: `on_demand_service(Args)` with `Args.Type` determining inbox under `ON_DEMAND_PATH`.

`processRequest(Input)`:

| `action` | Behavior |
|----------|----------|
| `health` | `status: ok`, `message: health: OK` |
| `forced_photometry` | Load `request.json` from paths in trigger; call `processForcedPhotometry` |
| `custom_coadd` | Load workdir spec; call `processCustomCoadd` |
| `custom_coaddition` | Same as `custom_coadd` |

Handlers:

- Validate workdir fields.
- Perform science (callable without IPC for unit tests).
- Return struct converted to `.out.json` and `result.json`.
- On failure: `status: error`, `message` with reason; workdir `status.state: error`.

Injected fields on callback input: `IpcInputJsonFilename`, `IpcOriginalTriggerBasename` (when claimed).

---

## 7. TTL and cleanup

Per-type configuration file (see `../config/on_demand.<type>.example.json`):

| Key | Type | Default | Meaning |
|-----|------|---------|---------|
| `ClaimTimeoutSec` | number | 3600 | Reclaim stuck claimed triggers in inbox |
| `ProcessedTriggerRetentionDays` | number | 7 | Delete archived triggers under `processed/` |
| `WorkRetentionDays` | number or omit | omit = forever | Delete terminal workdirs under `work/` |
| `CleanupIntervalSec` | number | 3600 | How often worker runs maintenance tick |
| `WorkerId` | string | auto | Claim suffix |

Cleanup rules:

- Delete workdirs only if `status.json` state is `ok` or `error` (not `queued` or `running`).
- Age measured from `status.completed_at` or folder mtime if missing.
- Never delete partial inputs while `running`.

Optional per-request override in workdir `request.json`: `retention_days` (number or `null` for forever) wins over type default.

---

## 8. `status.json` schema

```json
{
  "state": "queued",
  "request_id": "14-32-01-0007",
  "action": "forced_photometry",
  "created_at": "2026-11-19T14:32:01Z",
  "claimed_at": null,
  "started_at": null,
  "completed_at": null,
  "worker_id": null,
  "reclaim_count": 0,
  "heartbeat_at": null,
  "message": ""
}
```

States: `queued` → `running` → `ok` | `error`.

---

## 9. Inbox trigger schemas

### 9.1 Health (inbox-only smoke test)

```json
{ "action": "health" }
```

No workdir required. Response: standard health `.out.json`.

### 9.2 Forced photometry trigger

See `../examples/02_forced_photometry_trigger.json`.

### 9.3 Custom coadd trigger

See `../examples/03_custom_coadd_trigger.json`.

---

## 10. Workdir `request.json` schemas (v1)

Fields may evolve; handlers MUST reject unknown required missing fields with clear errors.

### 10.1 `forced_photometry`

| Field | Required | Description |
|-------|----------|-------------|
| `action` | yes | `forced_photometry` |
| `request_id` | yes | Matches folder name |
| `image_file` | yes | Reference image (FITS or `AstroImage` path) |
| `psf_file` | no | PSF model; default from image header/calib |
| `sources` | yes | Array of `{ "ra_deg", "dec_deg", "radius_pix"?, "name"? }` |
| `mask_file` | no | Bad-pixel / source mask |
| `output_folder` | no | Subfolder under workdir; default `.` |
| `retention_days` | no | Override TTL |
| `dry_run` | no | If true, mock success without science |
| `params` | no | Handler-specific struct (gain, aperture, …) |

### 10.2 `custom_coadd`

| Field | Required | Description |
|-------|----------|-------------|
| `action` | yes | `custom_coadd` |
| `request_id` | yes | Matches folder name |
| `image_files` | yes | Array of paths to images to coadd |
| `wcs_reference` | no | Reference image or explicit WCS file |
| `output_basename` | no | Default `coadd` |
| `method` | no | e.g. `median`, `mean`, `sigma_clip` — handler-defined enum |
| `retention_days` | no | Override TTL |
| `dry_run` | no | If true, mock success without science |
| `params` | no | Clip thresholds, sky subtraction, … |

---

## 11. `.out.json` response (common)

Success minimum:

```json
{
  "status": "ok",
  "message": "",
  "action": "forced_photometry",
  "request_id": "14-32-01-0007",
  "work_folder": "//euclid/share/on_demand/forced_photometry/work/2026/11/19/14-32-01-0007",
  "result_file": "//euclid/share/on_demand/forced_photometry/work/2026/11/19/14-32-01-0007/result.json",
  "status_file": "//euclid/share/on_demand/forced_photometry/work/2026/11/19/14-32-01-0007/status.json"
}
```

---

## 12. Security and operations

- Treat `inbox/` as a **multi-writer** drop zone; validate JSON size and schema before heavy I/O.
- Workers run with service account read/write on `ON_DEMAND_PATH` only (principle of least privilege).
- Watchdog: touch `ROOT/<type>/<type>.watchdog` every N seconds while loop alive (same ops pattern as other SOC workers).
- Logging: worker logs local to host; not stored under workdir by default.
- Optional ClickHouse: `ON_DEMAND_CHLOG=1` enables `io.chlog` in MATLAB (`openOnDemandChLogger`) and `chlog` in Python client (`../python_client/`). `CHLOG_CONFIG` for Python.

---

## 13. Network topology (informative)

```text
  [Science Portal / LAST client]     (WIS DMZ — Euclid / portal VM)
            |
            |  read/write ON_DEMAND_PATH on DMZ-visible share
            v
  +------------------+
  |  Euclid NFS/SMB  |
  |  ON_DEMAND_PATH  |
  +------------------+
            ^
            |  mount same share
            |
  [MATLAB worker]  (DMZ or WIS internal)
```

---

## 14. Implementation checklist (non-normative)

- [x] `OnDemandFileIpc` with claim/reclaim/TTL
- [x] `on_demand_service.m`, `processRequest.m`
- [x] `processForcedPhotometry.m`, `processCustomCoadd.m` (validate + `dry_run` only)
- [x] `+on_demand/+debug` scripts
- [ ] Python bridge (optional; clients may write triggers directly)
- [ ] Science: `imProc` forced photometry and custom coadd pipelines

---

## 15. Revision history

| Date | Change |
|------|--------|
| 2026-11-19 | Initial spec: two types, inbox/work split, claim, TTL |
| 2026-11-19 | MATLAB IPC + dry_run handlers |