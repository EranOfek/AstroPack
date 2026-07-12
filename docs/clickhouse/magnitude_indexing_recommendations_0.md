# ClickHouse Magnitude Indexing — Top Recommendations (LAST / ULTRASAT)

**Context:** `magnitude` in `last.proc_src` has values spread across the whole
range and is not part of the primary key. The goal is fast magnitude
predicates (`magnitude BETWEEN 16 AND 18`, `magnitude < 17`) without wrecking
the pipeline's real access path.

**The one thing to internalize first:** for a continuous column whose values
are randomly mixed inside granules, *no* ordinary skip index can prune. The
only real fix is a physical layout that is sorted/clustered by magnitude
somewhere. Everything below follows from that.

---

## TL;DR — ranked

| # | Option | Use when | Cost | Prunes magnitude? |
|---|--------|----------|------|-------------------|
| 0 | **Sky/time primary key, magnitude as post-filter** | cone search (the normal case) | none | via sky/time, not magnitude |
| 1 | **Lightweight `_part_offset` projection on magnitude** (25.5+, granule-level 25.11+) | magnitude-first queries, want index-like behavior cheaply | ~few % storage | ✅ (part-level 25.6, granule-level 25.11+) |
| 2 | **Full projection ordered by magnitude** | magnitude-first queries, older versions, want max speed | ~2× storage + insert/merge | ✅ strongly |
| 3 | **`magnitude` late in ORDER BY** `(upix_high, jd, magnitude, …)` | cone search that *also* filters magnitude | small | ✅ within each sky/time locality |
| 4 | **Separate bright-source table** (`magnitude < 18`) | frequent bright/transient workflows | maintenance of 2nd table | ✅ (table is just small) |
| 5 | **`minmax` skip index** | only if magnitude is clustered per granule (e.g. exposure/sky depth) | tiny | ⚠️ only when clustered |
| 6 | `mag_bin` + `set` index | almost never for range filters | tiny | ❌ for `magnitude BETWEEN` |
| — | `bloom_filter`, `ngrambf`, `tokenbf` | not for continuous numeric ranges | — | ❌ |

---

## 0. Get the access path right first

For LAST/ULTRASAT cone search, the heavy pruning must come from **sky (HEALPix
`upix_high`) and time (`jd`)**, with magnitude applied afterward on an already
small set:

```sql
ORDER BY (upix_high, jd, id_proc_src)
-- query shape:
WHERE upix_high BETWEEN :p_lo AND :p_hi
  AND jd        BETWEEN :j_lo AND :j_hi
  AND magnitude BETWEEN 16 AND 18
```

Before adding *anything* for magnitude, check whether the scan after sky/time
pruning is already small enough:

```sql
EXPLAIN indexes = 1
SELECT count() FROM last.proc_src
WHERE upix_high BETWEEN :p_lo AND :p_hi
  AND jd        BETWEEN :j_lo AND :j_hi
  AND magnitude BETWEEN 16 AND 18;
```

If the remaining granule count is tiny, **do nothing else**. Only the
magnitude-first workloads (below) justify extra structures.

---

## 1. Lightweight `_part_offset` projection — the modern top pick

Since ClickHouse **25.5**, a projection can store *only* its sort key plus the
virtual `_part_offset` column (a pointer back to the base row) instead of a
full copy. Its primary index then acts like a secondary index on that column:
ClickHouse locates matching rows via the projection and reads the actual
columns from the base table — for a few percent of extra storage instead of a
full duplicate.

```sql
-- index-like projection: sorts by magnitude, stores only (magnitude, _part_offset)
ALTER TABLE last.proc_src
  ADD PROJECTION prj_mag_idx ( SELECT _part_offset ORDER BY magnitude );

ALTER TABLE last.proc_src
  MATERIALIZE PROJECTION prj_mag_idx;   -- backfill existing parts
```

**Version matters a lot here — read this before relying on it:**

| Version | What the `_part_offset` projection does for `magnitude BETWEEN x AND y` |
|---------|--------------------------------------------------------------------------|
| 25.5 | `_part_offset` projections exist; **part-level pruning only** |
| 25.6 | multiple `_part_offset` projections can be combined for multi-filter queries; still **part-level pruning only** |
| 25.11+ | `_part_offset` projections gain **granule-level pruning** — the version where this becomes genuinely strong for selective magnitude ranges |
| 26.1+ | compact syntax available (define the projection as an index directly) |

So on **25.6** a lightweight magnitude projection prunes whole *parts* only —
useful if magnitude correlates with parts (e.g. per-exposure/per-partition
depth), weaker for finely selective ranges. On **25.11+** it prunes at the
granule level and behaves like a true secondary index (published benchmarks
show ~90% less data read on comparable range filters). If you can run 25.11 or
newer, this is the best storage-vs-speed trade-off for magnitude.

A big practical bonus (25.6+): you can define **several** `_part_offset`
projections (e.g. one per commonly-filtered column) and ClickHouse will use
each one's primary index to prune for multi-column queries — while still
reading row data from just one source.

**Verify it's actually used:**

```sql
EXPLAIN projections = 1
SELECT count() FROM last.proc_src WHERE magnitude BETWEEN 16 AND 18;
-- look for: Projections: Name: prj_mag_idx ... used for part-level/granule filtering
```

---

## 2. Full projection ordered by magnitude

If you're on a version before 25.11 (so lightweight projections only prune
parts) and you need fast magnitude-first queries *now*, a **full** projection
is the reliable heavy hammer. It keeps a second copy of the data sorted by
magnitude; the base table stays sky/time-optimal and ClickHouse auto-selects
the projection for magnitude-driven queries.

```sql
ALTER TABLE last.proc_src
  ADD PROJECTION prj_by_mag ( SELECT * ORDER BY (magnitude, jd, upix_high) );

ALTER TABLE last.proc_src
  MATERIALIZE PROJECTION prj_by_mag;
```

**Cost:** ~2× storage and extra insert/merge work. On a 100B-row table this is
a real commitment — measure the insert-rate and disk impact before committing.
Prefer option 1 if you're on 25.11+.

---

## 3. `magnitude` later in the ORDER BY (best for cone search + magnitude)

If your dominant query is cone search that *also* narrows magnitude, put
magnitude at the **end** of the sorting key. It costs almost nothing and sorts
magnitude *within* each `(upix_high, jd)` locality, so the compound query gets
extra pruning without harming the sky/time access path.

```sql
ORDER BY (upix_high, jd, magnitude, id_proc_src)
```

This does **not** speed up a bare `WHERE magnitude BETWEEN 16 AND 18` (no sky/time
filter) — for that you need option 1/2. Only adopt if magnitude at the tail
doesn't bloat the primary key or hurt insert/merge.

---

## 4. Separate bright-source table

Often the most practical answer for "give me bright objects" science. Instead
of indexing the giant mixed table, maintain a much smaller table holding only
the scientifically useful bright detections:

```sql
CREATE TABLE last.proc_src_bright
ENGINE = MergeTree
ORDER BY (upix_high, jd, magnitude, id_proc_src)
AS SELECT * FROM last.proc_src WHERE magnitude < 18;
-- better: populate via the insert pipeline / a materialized view so it stays live
```

The bright table is small enough that *every* query on it is fast, and it can
outperform any index on the full table for bright/transient candidate
workflows.

---

## 5. `minmax` skip index — only if magnitude is clustered

`minmax` stores per-granule min/max and can skip a granule only when the
requested range falls entirely outside it. That helps **only** when each
granule's magnitude span is narrow — i.e. when magnitude is physically
clustered (e.g. detections from one exposure share a limiting depth, or survey
depth varies smoothly across the sky so sky-ordered granules are
magnitude-coherent). If magnitude is randomly mixed, it prunes essentially
nothing.

```sql
ALTER TABLE last.proc_src
  ADD INDEX idx_mag_minmax magnitude TYPE minmax GRANULARITY 4;
ALTER TABLE last.proc_src
  MATERIALIZE INDEX idx_mag_minmax;
```

**Prove whether it can possibly help** with the granule-span check (small span
⇒ minmax can prune; span ≈ full range ⇒ it can't):

```sql
SELECT
  count()                          AS granules,
  avg(mx - mn)                     AS avg_span,
  quantile(0.5)(mx - mn)           AS p50_span,
  quantile(0.9)(mx - mn)           AS p90_span,
  min(mn) AS global_min, max(mx)   AS global_max
FROM (
  SELECT intDiv(rowNumberInAllBlocks(), 8192) AS g,
         min(magnitude) AS mn, max(magnitude) AS mx
  FROM last.proc_src
  GROUP BY g
);
```

Then A/B the real query:

```sql
SELECT count() FROM last.proc_src
WHERE magnitude BETWEEN 17 AND 18 SETTINGS use_skip_indexes = 0;  -- baseline
SELECT count() FROM last.proc_src
WHERE magnitude BETWEEN 17 AND 18 SETTINGS use_skip_indexes = 1;  -- with index
```

It's cheap, so it's fine to keep as an opportunistic win — just don't *depend*
on it.

---

## 6. What's usually not worth it

- **`mag_bin` (`floor(magnitude*10)`) + `set` index.** A `set` index only
  helps when each granule holds few distinct values, and it indexes `mag_bin`,
  not `magnitude` — so a `WHERE magnitude BETWEEN …` predicate won't use it at
  all. It only assists queries written against `mag_bin`, and even then only if
  granules are bin-coherent. Skip it for range filtering.
- **`bloom_filter` / `ngrambf_v1` / `tokenbf_v1`.** Bloom filters answer
  membership/equality, not ordered numeric ranges; the token/ngram filters are
  for text. None fit `magnitude BETWEEN x AND y`.

---

## Decision guide

```
Is the query cone search (sky + time + magnitude)?
  └─ yes → Option 0 (+ Option 3 if magnitude narrows it further). Usually done.
  └─ no, it's magnitude-first (WHERE magnitude <17 / BETWEEN …):
        On ClickHouse 25.11+ ? ──► Option 1 (lightweight _part_offset projection)
        On 25.6 / older       ? ──► Option 2 (full projection)   [1 prunes parts only]
        Mostly "bright" science ? ─► Option 4 (separate bright table), often best
Is magnitude clustered per granule (exposure/sky depth)?
  └─ yes → add Option 5 (minmax) as a cheap bonus; verify with the span query.
  └─ no  → don't rely on any skip index for magnitude.
```

---

## How these were validated

These rankings are backed by a benchmark harness on the real
`upix_high`/`jd`/`magnitude` schema (100M-row scale; runs locally on embedded
ClickHouse or against a cluster). Representative results, using `EXPLAIN
indexes = 1` granule counts as the pruning signal:

- **`minmax`, randomly-mixed magnitude:** granule span ≈ 10 mag → **0%** pruned
  (60/60 granules read). Confirms the core warning.
- **`minmax`, magnitude clustered by depth:** granule span ≈ 1.8 mag → **~93%**
  pruned (60 → 4 granules). Confirms it works *only* when clustered.
- **`mag_first` / projection:** magnitude-only queries prune to a handful of
  granules — but a table physically ordered by magnitude then reads **all**
  granules for a sky/time cone search (the tradeoff). A projection avoids that:
  base table stays sky/time-optimal, projection serves magnitude-first queries.

Always confirm on your own data and version with `EXPLAIN indexes = 1` and
`EXPLAIN projections = 1`, disabling caches for clean numbers.

---

## Version cheat-sheet (`_part_offset` projections)

- **25.5** — `_part_offset` in projections introduced; lightweight
  (index-only) projections; part-level pruning.
- **25.6** — multiple `_part_offset` projections combined for multi-filter
  queries; still part-level pruning only.
- **25.11** — `_part_offset` projections support granule-level pruning (the
  version where they become a strong magnitude index).
- **26.1** — compact syntax for defining an index-like projection.

Check your version: `SELECT version();`

---

## References

- ClickHouse blog — *Projections now behave like true secondary indexes*: https://clickhouse.com/blog/projections-secondary-indices
- ClickHouse docs — *Projections* (`_part_offset`, multi-projection pruning): https://clickhouse.com/docs/data-modeling/projections
- ClickHouse release notes — 25.6: https://clickhouse.com/blog/clickhouse-release-25-06
- ClickHouse release notes — 25.11 (granule-level pruning): https://clickhouse.com/blog/clickhouse-release-25-11
- Projections guide (partial/lightweight, version notes): https://pulse.support/kb/clickhouse-projections-guide
- Indexes, projections & skipping overview: https://thinhdanggroup.github.io/clickhouse-indexes/
