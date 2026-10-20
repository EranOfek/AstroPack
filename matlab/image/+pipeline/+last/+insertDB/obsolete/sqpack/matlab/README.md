# StatusDB: folder status table in PostgreSQL, from MATLAB

One row per leaf folder, keyed by its path relative to the data root:

| folder (PK) | status | updated | updated_by |
|---|---|---|---|
| `w2026/06/01/visit_001` | `done` | 2026-10-14 17:02:11 UTC | pipeline |

`StatusDB` is a `handle` class on the **Database Toolbox native PostgreSQL
interface** (`postgresql`, R2020b+; written for R2025b). Status text is free text
(multi-line is fine). Every change is recorded in `visit_status_log` by a trigger.

## Setup (once)

1. Database and schema (as a PostgreSQL admin):
   ```sql
   CREATE ROLE pipeline LOGIN PASSWORD '...';
   CREATE DATABASE obs OWNER pipeline ENCODING 'UTF8' TEMPLATE template0;
   ```
   ```bash
   psql -h euclid -U pipeline -d obs -f ../sql/schema.sql
   ```
   or from MATLAB: `st.createSchema()` (idempotent).
2. MATLAB: `addpath('<this folder>')`, then store the password once (R2024a+):
   `setSecret("pgstatus")`. Or set `PGPASSWORD`.
3. Check: `st = StatusDB('Server','euclid','Database','obs','User','pipeline'); st.check()`
4. Tests: `run_statusdb_tests('Server','euclid','Database','obs','User','pipeline')`.
   They use a scratch table and drop it; your data is never touched.
5. Import the existing status files (see below).

## Everyday use

```matlab
st = StatusDB('Server','euclid','Database','obs','User','pipeline','Root','/data');

st.write('w2026/06/01/visit_001', 'done');                 % set
s = st.read('w2026/06/01/visit_001');                      % get ('' if no row)
st.write(folders, 'queued');                               % many at once (chunked)
st.write(key, 'reprocessed', 'Mode', 'append');            % add a line

T = st.asTable('w2026/06/01');                             % table, RowNames = folder
T{"w2026/06/01/visit_001", "Status"}
T(T.Status == "failed", :)
T.Status(T.Status == "failed") = "queued";
st.writeTable(T);                                          % only changed rows are modified

st.summary('w2026/06')                                     % counts per status
H = st.history('w2026/06/01/visit_001');                   % change log

% parallel-safe work queue (many MATLAB sessions / parfor workers at once)
got = st.claim([], 'queued', 'running', 'Limit', 10, 'Prefix', 'w2026/06');

clear st                                                   % closes the connection
```

Keys can be `w2026/06/01/visit_001`, the autofs key `w2026_06_01_visit_001`,
or an absolute path under `Root`.

## Importing status files from the folder tree

```matlab
rep = st.importStatusFiles('/data', 'w2026', 'DryRun', true);          % look first
rep = st.importStatusFiles('/data', 'w2026', 'Originals', 'rename');   % import, rename originals
```

- **Where it looks.** For each leaf `<night>/<visit>` it finds files matching
  `Pattern` (default `*.status`) in three places:
  - inside the folder: `<visit>/x.status`
  - beside a SquashFS-packed folder: `<visit>.meta/x.status`
  - as a sibling file: `<visit>.status`
  It works night by night, never follows symlinks (so it never mounts packed
  folders), and prints one line per night.
- **Mode.** `insert_only` (default) never overwrites a row already in the
  database. `replace` lets the files win.
- **Originals.** `keep` (default), `rename` (adds `.imported`, so re-runs skip
  them) or `delete`. Only files whose text is **confirmed in the database after
  the write** are renamed or deleted; anything that differs is left in place
  and reported.
- **Several files for one folder.** `OnMultiple` chooses: `newest` (default;
  the others are left alone), `join` (oldest first), or `skip`.
- **Problems are reported, not fatal.** A sibling file with no matching folder,
  an unreadable file or a bad key goes into `rep.errors`.
- **Pilot runs.** `MaxNights` stops after N nights.

## Methods

| method | purpose |
|---|---|
| `StatusDB(...)` / `close` / `clear st` | connect (password from vault/PGPASSWORD), safety checks, auto-close |
| `createSchema` | table, indexes, `updated` trigger, history log (`'Print',true` shows SQL) |
| `read` | one or many folders, input order kept, `Default` for missing |
| `write` | upsert; modes `replace` (default), `append`, `insert_only` |
| `list` / `asTable` | rows under a prefix; filter by `Status`, `Limit` |
| `writeTable` | write an edited table back |
| `claim` | compare-and-set / work queue (`FOR UPDATE SKIP LOCKED`) |
| `remove`, `history`, `summary` | delete rows, change log, counts |
| `importStatusFiles` | scan tree for status files, insert, verify, rename/delete originals |
| `check` | **debug**: server, encodings, schema, collation, triggers, privileges, latency, self test |
| `folderKey` | canonical key for any accepted key form |

File-level tools (no database): `sqstatus_debug` (**debug** one folder on disk:
packed?, symlink target, `.meta`, leftovers, mount state) and
`sqstatus_read/write/list/resolve`.

## Debugging

- `st.check()`: prints a report and lists every problem it finds in `R.warnings`.
- `st.debug = true` or `setenv('PGSTATUS_DEBUG','1')`: every SQL statement is
  printed with its run time and row count.
- `sqstatus_debug('/data', 'w2026/06/01/visit_001', 'Probe', true)`: checks one
  folder, including reading files through the automount.

## Design notes

- **Quoting.** The native interface has no prepared statements (MathWorks
  supports them on JDBC only), so values are quoted by the class. This is safe
  because the constructor refuses to run unless `standard_conforming_strings = on`
  (the PostgreSQL default). The tests round-trip quotes, backslashes, `%`, `_`,
  newlines, `$$` and non-ASCII text.
- **Folder ordering and prefixes.** `folder` is `COLLATE "C"`, so prefix queries
  use the primary-key index and sorting is byte order (`visitX` sorts before
  `visit_`). A prefix stops at a level boundary, and `_`/`%` in it are literal.
- **Unchanged rows are not rewritten**, so `updated` and the history only move
  on real changes.
- **Shared connections.** `StatusDB('Connection', conn, ...)` borrows an open
  connection and does not close it when cleared. A JDBC connection from
  `database(...)` should also work this way (untested).

## Other languages

It is a plain table, so Python and C++ can use it directly:

```python
import psycopg   # pip install "psycopg[binary]"
with psycopg.connect("host=euclid dbname=obs user=pipeline") as c:
    c.execute("""INSERT INTO visit_status(folder,status) VALUES (%s,%s)
                 ON CONFLICT (folder) DO UPDATE SET status = EXCLUDED.status""",
              ("w2026/06/01/visit_001", "done"))
```

## Tested

Tested in GNU Octave 8.4 against PostgreSQL 16, through a psql-based stand-in
for the toolbox (`test_mock/`; Octave only, never put it on the MATLAB path):

- 20 of 21 tests pass; the MATLAB-only table test is skipped.
- Six concurrent workers claimed 200 queued folders with no duplicates.

**Run `run_statusdb_tests` once in MATLAB R2025b on euclid** to confirm the
toolbox side, especially `asTable`/`writeTable`.
