# On-demand Python client

Single-file producer for [`ON_DEMAND_PATH`](../docs/SPEC.md) file-IPC. Pattern matches GW experiment `dispatch_too_planner.py` (atomic inbox publish + poll `.out.json`), extended with workdir + trigger per the on-demand spec.

## Files

| File | Role |
|------|------|
| **`on_demand_client.py`** | Copy or import this for the Science Portal |
| `demo_on_demand_client.py` | Local smoke test |
| `test_on_demand_client.py` | `pytest` (mock worker, no MATLAB) |

## Environment

| Variable | Purpose |
|----------|---------|
| `ON_DEMAND_PATH` | Share root (required) |
| `ON_DEMAND_CHLOG` | `1` / `true` — enable ClickHouse logging (optional) |
| `CHLOG_CONFIG` | Path to chlog config (optional; astro-clickhouse) |

## Quick start

```python
from on_demand_client import OnDemandClient

client = OnDemandClient()  # ON_DEMAND_PATH

result = client.health_check("forced_photometry")
print(result.status, result.message)

work = {
    "action": "forced_photometry",
    "dry_run": True,
    "image_file": "//euclid/share/on_demand/.../reference.fits",
    "sources": [{"ra_deg": 12.3, "dec_deg": -4.5, "name": "s1"}],
}
handle = client.submit_job("forced_photometry", work, timeout_sec=3600)
outcome = client.wait_for_job(handle, timeout_sec=3600)
client.close()
```

Run MATLAB worker (separate host or terminal):

```matlab
ultrasat.services.on_demand.on_demand_service('Type', 'forced_photometry')
```

## Optional chlog

Add `astro-clickhouse/utils/logging` to `PYTHONPATH`, then:

```bash
set ON_DEMAND_CHLOG=1
set CHLOG_CONFIG=C:\path\to\chlog.json
python demo_on_demand_client.py --chlog
```

## Tests

```bash
pytest matlab/astro/+ultrasat/+services/+on_demand/python_client/test_on_demand_client.py
```