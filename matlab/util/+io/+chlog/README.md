# ClickHouse central log (`io.chlog`)

MATLAB client for the shared **chlog** schema (same spool/replay rules as Python/C++ clients in astro-clickhouse). Inserts use **HTTP** `:8123` and `JSONEachRow` — no MEX required.

## Quick start

Ensure `matlab/util` is on the MATLAB path (normal AstroPack startup).

```matlab
L = io.chlog.ChLogger.open('', 'Process', 'my_app');   % ChLogger.yaml (local wins over shared)
L.log('INF', 'hello', 'CorrId', 'cmd-1');
L.close();
```

Configuration: AstroPack [`config/ChLogger.yaml`](../../../../config/ChLogger.yaml) (or `config/local/ChLogger.yaml`), a struct, explicit YAML/JSON path, or env `CHLOG_CONFIG`. Password via env `CHLOG_CH_PASSWORD` when YAML leaves `clickhouse.password` empty.

**Config resolution:** `open('')` or `open([])` loads `config/local/ChLogger.yaml` if present, else `config/ChLogger.yaml`, under `Configuration.getSysConfigPath()` (repo `config/` or env **`ASTROPACK_CONFIG_PATH`**). Offline **`io.chlog.demo.demo`** and **`io.chlog.tests.run_tests`** use built-in defaults plus `Overrides` (no disk YAML required). Direct `loadConfig('')` is the same defaults-only path used by tests.

**`client.node`:** leave empty in the shared YAML to use this machine’s OS hostname in every row and spool file prefix (same as Python/C++ when `client.node` is empty). Set a fixed logical name in `config/local/ChLogger.yaml` when you need a SOC label that differs from the hostname (e.g. `ccs-01`). You can also pass `io.chlog.ChLogger.open(..., 'Node', 'my-node')`.

## Tests and demos

```matlab
nfail = io.chlog.tests.run_tests;   % mock adapter, no server
io.chlog.demo.demo;                 % offline outage demo
io.chlog.demo.demo_real([], 'verify_process', 100, spoolDir);  % default ChLogger.yaml
```

## MsgLogger integration

In [`config/MsgLogger.yml`](../../../../config/MsgLogger.yml): **`UseChLogger: false`** (default). Set to `true` to forward `io.msgLog` / `MsgLogger` to ClickHouse.

All connection and client settings live in **`config/ChLogger.yaml`** (`MsgLoggerLevel`, `Process`, `clickhouse`, `client`, `matlab`). Override with `config/local/ChLogger.yaml`.

## Schema and ops

Table layout, janitor, and server config live in the **astro-clickhouse** repo (`utils/logging/`). Python/C++ still use `chlog.json` there; AstroPack uses YAML only.
