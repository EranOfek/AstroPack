# ClickHouse JDBC for MATLAB

## Server upgrade (2026-07-13)

The ClickHouse server on Euclid was upgraded to version **26.6**. The old JDBC driver **0.7.0** is not compatible with the new server behavior and caused query and insert errors.

The read-only ClickHouse user `last_user` was updated from `readonly = 1` to `readonly = 2`. This still blocks INSERT, ALTER, CREATE, DROP, and other write operations, but allows JDBC to set required session/query options.

### Required driver

Install via Installer (recommended):

```matlab
I = Installer; I.install('ClickHouseJar')
```

This downloads **clickhouse-jdbc-0.9.3-all.jar** from Maven into the `Java/Clickhouse` data directory.

Manual download:

https://repo1.maven.org/maven2/com/clickhouse/clickhouse-jdbc/0.9.3/clickhouse-jdbc-0.9.3-all.jar

This version works with the Java 8 runtime used by MATLAB R2023a and R2025b, so no Java or Windows changes are required.

After installing or replacing the JAR, **fully restart MATLAB** before reconnecting to ClickHouse.

Legacy Installer items (do not use with ClickHouse 26.6+): `ClickHouseJar070`, `ClickHouseJar070Github`, `ClickHouseJar085`.

---

## How `db.Db` connects to ClickHouse

All ClickHouse access from MATLAB goes through [`Db.m`](Db.m). The class wraps MATLAB's Database Toolbox `database()` call over the ClickHouse JDBC driver.

### Driver and JAR discovery

`db.Db.connectCH_Java` (static method) loads the JDBC driver:

1. If `JarFile` is not passed, it uses `Installer` to locate the data directory for `ClickHouseJar`:
   ```matlab
   I = Installer;
   JarDir = I.getDataDir(I.Items.ClickHouseJar);
   ```
2. It scans that directory for `*.jar` files and adds each to the MATLAB Java path via `javaaddpath`.
3. If no JAR is found, it prints:
   ```matlab
   I = Installer; I.install('ClickHouseJar')
   ```

Place **only** `clickhouse-jdbc-0.9.3-all.jar` in that directory and remove older versions (0.7.0, 0.8.x). All ClickHouse Installer items share `Java/Clickhouse`; `connectCH_Java` adds every `*.jar` in that folder to the classpath, so legacy jars must be removed after upgrading.

A copy of the JAR is kept in `@Db/` for reference; the runtime copy used by `db.Db` lives in the Installer data dir.

**Important:** swapping JAR files requires a full MATLAB restart because `javaaddpath` does not reliably unload old driver classes.

### JDBC URL and driver class

| Parameter | Value |
|-----------|-------|
| Driver class | `com.clickhouse.jdbc.ClickHouseDriver` |
| Base URL | `jdbc:clickhouse` |
| Full URL | `jdbc:clickhouse://<Host>:<Port>/<DbName>?protocol=native&use_object_types=true` |

Connection is created with:

```matlab
Conn = database('', User, Password, Driver, JdbcURL);
```

Default host/port/database on a new `db.Db` object:

| Property | Default |
|----------|---------|
| `Host` | `10.150.28.18` (Euclid) |
| `Port` | `8123` |
| `DbName` | (empty; resolved from config or set explicitly) |
| `DbType` | `"Clickhouse"` |
| `ConnType` | `'java'` |

### Connection object flow

```matlab
DB = db.Db;
DB.Host     = '10.150.28.18';
DB.DbName   = 'last';
DB.User     = 'last_user';
DB.Password = 'physics';
DB.Conn;              % lazy connect via connectCH_Java
DB.useDB('last');     % USE last; updates DB.DbName
```

- **`get.Conn`** — creates the connection on first access by calling `connectCH_Java` with current `Host`, `Port`, `DbName`, `User`, `Password`.
- **`get.User` / `get.Password`** — if `User` is a cell `{Project, UserName}`, credentials are resolved through `PasswordsManager`. String shortcuts `euclid/user`, `euclid/root`, `last0/user`, `last0/root` map to the appropriate host and PasswordsManager entries.
- **`disconnect` / `disconnectCH_Java`** — closes the JDBC connection and clears `DB.Conn`.
- **`isConnected`** — returns whether `Conn` is non-empty and whether the session is read-only.

### Query and exec paths

All SQL goes through `DB.query(Query, ...)`:

| Mode | Flag | Underlying call |
|------|------|-----------------|
| Read (SELECT) | `'IsExec', false` (default) | `select(Conn, Query)` or `fetch(Conn, Query, Opts)` |
| Write (DDL/DML) | `'IsExec', true` | `exec(Conn, Query)` |

Low-level path: `query` → `queryCH_Java` → MATLAB Database Toolbox.

Schema helpers:

- `showDB` — `SHOW DATABASES`
- `showCurrentDB` — `SELECT currentDatabase()`
- `useDB(DbName)` — `USE <DbName>`
- `showTables` — `SHOW TABLES`
- `describeTable(TableName)` — `DESCRIBE <TableName>`
- `getColumns(TableName, DbName)` — query `system.columns`
- `fetch(Command)` — direct `fetch(Conn, Command)`

### Writes and user permissions

| User | Password | Access |
|------|----------|--------|
| `last_user` | (LAST read password) | Read-only (`readonly = 2`): SELECT and session options only |
| `default` | (root password) | Full write: CREATE, INSERT, ALTER, DROP |

Write methods on `db.Db`:

- **`createTable(TableName, ColNames, ColTypes, ...)`** — builds and execs `CREATE TABLE`
- **`insertCharDump(TableName, InputTable)`** — small inserts via `INSERT INTO ... VALUES`
- **`insertCsv(TableName, Data)`** — bulk insert via `clickhouse-client` and CSV file

Use `default` / root credentials for any test or script that creates tables or inserts rows.

### Quick usage (read-only LAST)

```matlab
DB = db.Db;
DB.User     = 'last_user';
DB.Password = '<password>';
DB.Conn;
DB.useDB('last');
DB.showCurrentDB
DB.showTables
T = DB.query("SELECT top 10 * FROM last.visit_images;");
T = DB.query("SELECT count(*) FROM last.proc_images;");
DB.disconnect;
```

### Quick usage (write test)

```matlab
DB = db.Db;
DB.User     = 'default';
DB.Password = '<root-password>';
DB.Conn;
DB.query('CREATE DATABASE IF NOT EXISTS test', 'IsExec', true);
DB.createTable('test.matlab_insert', ["id"; "name"; "val"], ["UInt32"; "String"; "Float64"]);
T = table(1, "hello", 3.14, 'VariableNames', {'id','name','val'});
DB.insertCharDump('test.matlab_insert', T);
T = DB.query("SELECT * FROM test.matlab_insert");
DB.disconnect;
```

### Unit test

Run from MATLAB:

```matlab
db.Db.unitTest
```

This runs four inner tests in order:

1. Raw JDBC — `SELECT version()`
2. Raw JDBC — `SELECT count(*) FROM last.proc_images`
3. `db.Db` read — version and count as `last_user`
4. `db.Db` write — create `test.matlab_insert`, insert one row, select back as `default`

Tests require network access to Euclid (`10.150.28.18:8123`) and the 0.9.3 JAR in the Installer data directory.
