# On-demand pipeline

MATLAB file-IPC workers rooted at **`ON_DEMAND_PATH`** (not `$SOC_PATH/runtime/exchange`).

- **User guide:** [docs/USER_GUIDE.md](docs/USER_GUIDE.md) (import to Google Docs)
- **Documentation:** [docs/README.md](docs/README.md) · [docs/SPEC.md](docs/SPEC.md)
- **Python client:** [python_client/on_demand_client.py](python_client/on_demand_client.py) (single file for portal)
- **Run:**

```matlab
ultrasat.services.on_demand.on_demand_service('Type', 'forced_photometry')
```