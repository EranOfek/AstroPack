# Local config (not in git)

`ChLogger.yaml` here is picked up automatically by `io.chlog.ChLogger.open('')` before `config/ChLogger.yaml`.

`MsgLogger.yml` in this folder wins over `../MsgLogger.yml` when `Configuration.getSysConfigPath()` points at the config root (repo `config/` or deploy tree such as `C:\AstroPack\Config`).

## Enable MsgLogger → ClickHouse (euclid)

In **`local/MsgLogger.yml`** (gitignored on disk; create on your machine or deploy tree):

```yaml
MsgLogger:
    UseChLogger: true
    FileLevel: LogLevel.All
    DispLevel: LogLevel.Info
    SyslogLevel: LogLevel.Info
    Console: true
    MaxFileSize: 10000000
```

Do not use `LogLevel.ChLog` — that is not a valid `LogLevel` name. Chlog verbosity comes from `ChLogger.yaml` **`MsgLoggerLevel`**.

Shared [`../ChLogger.yaml`](../ChLogger.yaml) already targets `euclid`, database `chlog`, user `chlog`. Set `clickhouse.password` or env `CHLOG_CH_PASSWORD`. Optional `local/ChLogger.yaml` for `client.spool_dir` (absolute path).

After edits: `clear classes` in MATLAB (MsgLogger loads config once per session).

```matlab
L = MsgLogger.getSingleton();
assert(~isempty(L.ChLog));
Tag = sprintf('chlog_smoke_%s', datestr(now, 'yyyymmdd_HHMMSS'));
io.msgLog(LogLevel.Info, '%s', Tag);
L.ChLog.flush();
```

`MsgLogger.unitTest` expects `UseChLogger: false`; rename or disable local `MsgLogger.yml` before running it.

## Quick test (direct client)

```matlab
io.chlog.demo.demo_real([], 'local_test', 5, fullfile(tempdir, 'chlog_spool_test'));
```
