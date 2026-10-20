# Visit status in PostgreSQL (LAST insertDB)

`pipeline.last.insertDB.status.StatusDB` stores per-visit ingest flags on Euclid
when archive folders are squash-backed and `.status` on disk is read-only or absent.

## Setup

1. Apply [schema.sql](schema.sql) on Euclid (`obs` database, `pipeline` user).
2. Add a row to `~/.astropack/Passwords.yml` (chmod 600), e.g. key `euclid_pg_status`.
3. Optional: `setSecret("pgstatus")` or `PGPASSWORD` for non-interactive runs.

```matlab
st = pipeline.last.insertDB.status.openVisitStatusDB(struct());
st.check();
```

Import existing `.status` files before packing visits on Euclid:

```matlab
st.importStatusFiles('/bigdata2/projects/last/data', 'LAST.01.01.01/2026/01', ...
    'Pattern', '*.status', 'Originals', 'rename');
```

Euclid squash/autofs ops: `astro-infra` `servers/euclid/squash_visit_folders/`.

Legacy sqpack prototype remains under `../sqpack/` (generic layout); production LAST
uses squash-store + autofs, not the `/sq` symlink layout.
