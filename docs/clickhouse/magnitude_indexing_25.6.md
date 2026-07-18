# Magnitude Indexing on ClickHouse 25.6+ (LAST / ULTRASAT)

Modern answer: use a **lightweight `_part_offset` projection** on `magnitude`.
It behaves like a secondary index, storing only `magnitude + _part_offset`
(a pointer back to the base row) — a few % of storage instead of a full copy.

## Do this

```sql
ALTER TABLE last.proc_src
  ADD PROJECTION prj_mag_idx ( SELECT _part_offset ORDER BY magnitude );

ALTER TABLE last.proc_src
  MATERIALIZE PROJECTION prj_mag_idx;   -- backfill existing data
```

Keep the base table sorted for cone search:

```sql
ORDER BY (upix_high, jd, id_proc_src)
```

## Know your version

| Version | `magnitude BETWEEN x AND y` |
|---------|------------------------------|
| 25.6 | prunes **whole parts only** |
| 25.11+ | prunes at **granule level** — this is where it gets fast |

Check: `SELECT version();`

- **25.11+** → the lightweight projection is your best option. Done.
- **25.6–25.10** → part-level only. If your range queries aren't selective
  enough, use a **full projection** instead until you upgrade:
  ```sql
  ALTER TABLE last.proc_src
    ADD PROJECTION prj_by_mag ( SELECT * ORDER BY (magnitude, jd, upix_high) );
  ALTER TABLE last.proc_src MATERIALIZE PROJECTION prj_by_mag;
  ```
  (~2× storage, faster reads.)

## Multiple filters (25.6+)

Define one lightweight projection per hot filter column. ClickHouse uses each
projection's index to prune, reading rows from just one source:

```sql
ALTER TABLE last.proc_src ADD PROJECTION prj_mag_idx ( SELECT _part_offset ORDER BY magnitude );
-- add others as needed, e.g. a second column your queries filter on
```

## Verify it's used

```sql
EXPLAIN projections = 1
SELECT count() FROM last.proc_src WHERE magnitude BETWEEN 16 AND 18;
```

Look for `prj_mag_idx` under `Projections:`. Benchmark with caches off:

```sql
... SETTINGS use_query_condition_cache = 0, optimize_use_projections = 0;  -- baseline
... SETTINGS use_query_condition_cache = 0, optimize_use_projections = 1;  -- with projection
```

## Skip these for magnitude ranges

- `minmax` — only helps if magnitude is clustered per granule; useless if
  values are mixed.
- `set` / `bloom_filter` — wrong tool for continuous numeric ranges.

## Cost

Lightweight projection ≈ half the storage of a full projection, ~2× slower
reads than a full one. Projections add insert/merge overhead — measure on your
ingest rate before rolling out at 100B-row scale.

---

**Refs:** [25.6 release](https://clickhouse.com/blog/clickhouse-release-25-06) ·
[25.11 release](https://clickhouse.com/blog/clickhouse-release-25-11) ·
[Projections docs](https://clickhouse.com/docs/data-modeling/projections) ·
[Secondary-index projections](https://clickhouse.com/blog/projections-secondary-indices)
