# LAST pipeline — draft ClickHouse DDL (AstroPack)

These four `.sql` files are **Sasha’s original table drafts** (`last_test.*`):

- `image_visit`, `image_proc`, `src_visit`, `src_proc`
- Column list, `COMMENT` clauses, `PARTITION BY` / `ORDER BY` only
- **No** skip indexes, projections, or production codecs

## Schema ownership

MATLAB ingest in the parent [`+insertDB`](../README.md) folder is a **producer**
(insert scripts). Do not treat this `sql/` folder as the production schema master.

**Canonical masters** (indexed `create_*.sql` + verbatim as-is copies) live in the
astro-clickhouse repository at `sql/pipeline1/`.

When column order or names change here, update the matching pair in astro-clickhouse
before applying DDL in ClickHouse.
