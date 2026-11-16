# ClickHouse central log (`io.chlog`)

MATLAB client for the shared **chlog** schema (same spool/replay rules as Python/C++ clients in astro-clickhouse). Inserts use **HTTP** `:8123` and `JSONEachRow` — no MEX required.

## Quick start

Ensure `matlab/util` is on the MATLAB path (normal AstroPack startup).

```matlab
L = io.chlog.ChLogger.open([], 'Process', 'my_app');   % Configuration.Data.ChLogger
L.log('INF', 'hello', 'CorrId', 'cmd-1');
L.close();
```

Configuration: [`config/ChLogger.yml`](../../../../config/ChLogger.yml) (loaded into **`Configuration.getSingleton().Data.ChLogger`**; `config/local/ChLogger.yml` overrides). You can also pass a struct, an explicit `.yml`/JSON path, or env `CHLOG_CONFIG`. Password via env `CHLOG_CH_PASSWORD` when `clickhouse.password` is empty.

```matlab
c = Configuration.getSingleton();
c.Data.ChLogger.ChLogger.clickhouse
```

**Config resolution:** `open([])` reads `Configuration.Data.ChLogger` first, then `CHLOG_CONFIG`, then `Data.ChLogger.FileName` or `config/ChLogger.yml` under `Configuration.getSysConfigPath()` (repo `config/` or **`ASTROPACK_CONFIG_PATH`**). Offline **`io.chlog.demo.demo`** and **`io.chlog.tests.run_tests`** use built-in defaults plus `Overrides` (`loadConfig('', Overrides)`). 

**`client.node`:** leave empty in shared YML to use this machine’s OS hostname in every row and spool file prefix. Set a fixed logical name in `config/local/ChLogger.yml` when you need a SOC label that differs from the hostname (e.g. `ccs-01`). You can also pass `io.chlog.ChLogger.open(..., 'Node', 'my-node')`.

## Tests and demos

```matlab
nfail = io.chlog.tests.run_tests;   % mock adapter, no server
io.chlog.demo.demo;                 % offline outage demo
io.chlog.demo.demo_real([], 'verify_process', 100, spoolDir);  % Configuration / ChLogger.yml
```

## MsgLogger integration

In [`config/MsgLogger.yml`](../../../../config/MsgLogger.yml): **`UseChLogger: true`** forwards `io.msgLog` / `MsgLogger` to ClickHouse. MsgLogger reads the toggle from **`Configuration.Data.MsgLogger`** when the singleton is loaded.

Connection and client settings live in **`config/ChLogger.yml`** (`MsgLoggerLevel`, `Process`, `clickhouse`, `client`, `matlab`). Override with `config/local/ChLogger.yml`.

## Schema and ops

Table layout, janitor, and server config live in the **astro-clickhouse** repo (`utils/logging/`). Python/C++ still use `chlog.json` there; AstroPack uses YAML only.
