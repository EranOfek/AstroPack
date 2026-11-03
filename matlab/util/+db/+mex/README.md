# +db/+mex

The files in this folder are **modified** copies of the ClickHouse MATLAB client from Mark's repo.
The unmodified originals are also stored in:

    repo: astro-clickhouse
    path: matlab/clickhouse-matlab/src/

## Changes vs the originals (per file)

### ClickHouseClient.m
- Package-qualified references, so the class works as `db.mex.ClickHouseClient`:
  - `Compression.*` -> `db.mex.Compression.*` (default LZ4 option, the `isa` check, and the error message text).
  - every `clickhouse_mex(...)` call -> `db.mex.clickhouse_mex(...)` (connect, ping, query x2, insert x2, delete, version).
- `insert`: when the table schema is known, sets `data.ch_strict_nullable = true` so that NaN in a
  non-Nullable float column is stored as a value, not as NULL (issue #1396).

### clickhouse_mex.cpp
- Reads the new optional field `ch_strict_nullable` from the insert struct (new `strict_nullable` flag)
  and skips it when enumerating columns (like `ch_decimal_hint`).
- Float32 and Float64 column creation: `has_nan` alone makes the column Nullable only when
  `strict_nullable` is off (`(has_nan && !strict_nullable) || force_nullable`).
  With the schema hint, only a Nullable column in the schema is Nullable.

### Compression.m
- No changes (identical to the original).

### Not in the original src folder
`block2struct.cpp`, `insert2.cpp`, `query.cpp` and their `.mexa64` files are AstroPack-only and have no counterpart there.
