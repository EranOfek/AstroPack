# HEALPix Cone Search — SQL Generator

> **Goal:** Given a sky position `(RA, Dec)` and a search radius, generate an efficient  
> ClickHouse `SELECT` statement using the minimum possible number of `BETWEEN … OR …` ranges  
> over a HEALPix **level 16** (NSide = 65 536) NESTED pixel column.

---

## Table of Contents

1. [Background](#1-background)  
2. [Algorithm Overview](#2-algorithm-overview)  
3. [NSide Selection — The Key Formula](#3-nside-selection--the-key-formula)  
4. [Two Search Algorithms](#4-two-search-algorithms)  
5. [Range Expansion & Merging](#5-range-expansion--merging)  
6. [SQL Output](#6-sql-output)  
7. [Post-Filter Options](#7-post-filter-options)  
8. [Cross-Platform Backend](#8-cross-platform-backend)  
9. [Pixel ID Bounds — Why UInt64](#9-pixel-id-bounds--why-uint64)  
10. [API Reference](#10-api-reference)  
11. [ClickHouse Schema](#11-clickhouse-schema)  
12. [Running & Testing](#12-running--testing)  
13. [Known Limitations](#13-known-limitations)

---

## 1. Background

HEALPix (Hierarchical Equal Area isoLatitude Pixelisation) divides the sphere into  
`12 × NSide²` equal-area pixels.  At **level 16**, NSide = 2¹⁶ = 65 536:

| Property | Value |
|---|---|
| Level | 16 |
| NSide | 65 536 |
| Total pixels | 12 × 65 536² = **51 539 607 552** |
| Pixel size (approx.) | **≈ 3.2 arcseconds** |
| Max pixel id | **51 539 607 551** (requires `UInt64`) |

**NESTED ordering** is essential here.  In NESTED, every pixel at a coarse level maps  
to a *contiguous block* of IDs at a finer level.  That is:

```
parent pixel P at NSide_low  →  IDs [ P·k², P·k² + k² − 1 ]  at NSide_high
where  k = NSide_high / NSide_low
```

This property lets us represent a set of pixels as a small list of integer ranges —  
perfect for ClickHouse `BETWEEN` queries on an indexed column.

---

## 2. Algorithm Overview

```
Input: RA, Dec (degrees), Radius (degrees)
       table name, column name

Step 1  Choose NSideSearch ≈ pixel_size ≈ Radius
        (largest power-of-2 NSide whose pixel size covers the search radius)

Step 2  Find low-resolution pixels overlapping the cone
        (NEIGHBOR: centre + 8 neighbours  |  CONE: query_disc)

Step 3  Expand each low-res pixel to a range of level-16 pixel ids

Step 4  Sort ranges, merge adjacent/overlapping ones

Step 5  Emit ClickHouse SQL  WHERE (col BETWEEN lo AND hi) OR …

Output: SQL string + optional post-filter fragment
```

---

## 3. NSide Selection — The Key Formula

The angular "radius" of a HEALPix pixel (centre to vertex) is approximately:

```
pixel_radius ≈ sqrt(3) / NSide   [radians]
```

We want `pixel_radius ≥ search_radius`, so:

```
NSide ≤ sqrt(3) / search_radius_rad

NSideSearch = largest power-of-2 satisfying this, capped at NSide_CAT
```

### Implementation

```python
ideal = math.sqrt(3.0) / math.radians(radius_deg)
level = int(math.floor(math.log2(ideal)))
nside_search = min(2 ** level, NSIDE_CAT)
```

### Why is the `sqrt(3)` factor critical?

Without it (the bug in the original MATLAB code):

```
1 / radius_rad  → underestimates by factor 1.73 → NSideSearch too coarse
```

For `radius = 1°`:

| Formula | ideal | NSideSearch |
|---|---|---|
| `1 / radius_rad` (wrong) | 57.3 | 32 |
| `sqrt(3) / radius_rad` (correct) | 99.3 | **64** |

Getting NSideSearch wrong by one level means each range is **4× wider**, requiring  
4× more candidate rows in ClickHouse.

---

## 4. Two Search Algorithms

### NEIGHBOR (conservative, always ≤ 9 ranges)

1. Find the pixel containing `(RA, Dec)` at `NSideSearch`  
2. Retrieve its 8 HEALPix neighbours (handles boundary/pole cases, removes -1 entries)  
3. Keep centre + unique neighbours → typically 9 pixels  
4. Expand to level-16 ranges

**Pros:** Deterministic, always exactly ≤ 9 ranges, never misses the centre.  
**Cons:** Over-inclusive — the 3×3 box may reach 2–3× the search radius at its corners.

### CONE (tighter, fewer ranges)

1. Use `healpy.query_disc` / `astropy_healpix.cone_search_skycoord` at `NSideSearch`  
   with `inclusive=False` (only pixels whose *centres* are inside the cone)  
2. Expand results to level-16 ranges  
3. Merge contiguous ranges

**Pros:** Fewer, tighter ranges — typically 4–6 instead of 9.  
**Cons:** Slightly more computation at query-plan time. Sub-pixel radius falls back to single pixel.

### Comparison for RA=254°, Dec=64°, R=1°

| | Ranges | Total pixels at NSide=2^16 |
|---|---|---|
| NEIGHBOR | 9 | ~15.7 M |
| CONE | 4 | ~11.8 M |

CONE produces **~25% fewer candidate rows** for ClickHouse to post-filter.

> **Recommendation:** Use `Algo.CONE` for production. Use `Algo.NEIGHBOR` as a  
> fast sanity-check or when the HEALPix library is unavailable.

---

## 5. Range Expansion & Merging

Each low-NSide pixel `P` expands to:

```python
nchild = (NSIDE_CAT // nside_search) ** 2
lo = P * nchild
hi = lo + nchild - 1
```

After computing all `(lo, hi)` pairs, adjacent/overlapping ranges are merged:

```
[0, 3], [4, 7]  →  [0, 7]       # merge (adjacent)
[0, 5], [3, 9]  →  [0, 9]       # merge (overlapping)
[0, 3], [8, 11] →  [0,3],[8,11] # keep separate
```

This reduces the SQL `OR` clause count.

---

## 6. SQL Output

### Ranges-only query

```sql
SELECT *
FROM proc_src
WHERE (
   (upix_high BETWEEN 12149850112 AND 12151947263)
   OR (upix_high BETWEEN 12152995840 AND 12160335871)
   OR (upix_high BETWEEN 12161384448 AND 12162433023)
   OR (upix_high BETWEEN 12180258816 AND 12185501695)
)
```

ClickHouse evaluates this using its sparse index on the sort key — each `BETWEEN`  
hits at most 1–2 granules (default granule = 8192 rows).  
**Total I/O: O(ranges × granule_size)**, independent of table size.

---

## 7. Post-Filter Options

The healpix ranges are a **pre-filter** — they return a superset of the true cone.  
A post-filter is always needed for exact results.

### Option A: Direction-cosine dot product (recommended)

Store three extra columns `cx, cy, cz` (unit vector of each source):

```sql
-- Pre-computed at ingest time:
cx = cos(dec_rad) * cos(ra_rad)
cy = cos(dec_rad) * sin(ra_rad)
cz = sin(dec_rad)
```

Post-filter (no trig at query time):

```sql
AND (cx * 0.12345 + cy * 0.67890 + cz * 0.73456 >= 0.99985)
--         ↑ cx0          ↑ cy0          ↑ cz0      ↑ cos(radius)
```

This is pure multiply-and-add — very fast in ClickHouse vectorised execution.

### Option B: ClickHouse `greatCircleAngle`

```sql
AND (greatCircleAngle(ra, dec, 254.0, 64.0) <= 1.0)
```

Requires trig per row but needs no extra stored columns.

---

## 8. Cross-Platform Backend

| Platform | Library | Notes |
|---|---|---|
| Linux | `healpy` | C extension, fast |
| Windows | `astropy_healpix` | Pure Python, no C compiler needed |

The module auto-detects the OS and loads the appropriate backend.  
Both backends expose the same internal API:

```python
ang2pix_nested(nside, ra_deg, dec_deg) → int
query_disc_nested(nside, ra_deg, dec_deg, radius_deg) → np.ndarray
neighbours_nested(nside, pix) → np.ndarray
pix2ang_nested(nside, pix) → (ra_deg, dec_deg)
```

---

## 9. Pixel ID Bounds — Why UInt64

```
Max pixel id = 12 × 65536² − 1 = 51 539 607 551

UInt32 max  =              4 294 967 295   ← OVERFLOW!
UInt64 max  = 18 446 744 073 709 551 615   ← OK
```

**Always use `UInt64` for the healpix column in ClickHouse.**

---

## 10. API Reference

### `cone_to_pixel_ranges(ra_deg, dec_deg, radius_deg, algo) → PixelRanges`

Returns a `PixelRanges` object with `.ranges` list of `(lo, hi)` tuples.

| Parameter | Type | Description |
|---|---|---|
| `ra_deg` | float | Right ascension [0, 360) degrees |
| `dec_deg` | float | Declination [−90, 90] degrees |
| `radius_deg` | float | Search radius > 0 degrees |
| `algo` | Algo | `Algo.CONE` or `Algo.NEIGHBOR` |

---

### `cone_search_sql(ra_deg, dec_deg, radius_deg, table, column, ...) → (sql, post_filter)`

| Parameter | Default | Description |
|---|---|---|
| `algo` | `Algo.CONE` | Search algorithm |
| `extra_columns` | `"*"` | SELECT column list |
| `post_filter` | `True` | Return post-filter fragment |
| `post_filter_mode` | `"cosine"` | `"cosine"` or `"greatcircle"` |
| `cx_col,cy_col,cz_col` | `"cx","cy","cz"` | Direction-cosine column names |
| `ra_col,dec_col` | `"ra","dec"` | RA/Dec column names for greatcircle mode |

Returns `(sql_string, post_filter_string_or_None)`.

---

### `cone_search_sql_full(...)  → str`

Returns a single SQL string with both healpix ranges AND the post-filter `AND` clause embedded.

---

## 11. ClickHouse Schema

```sql
CREATE TABLE proc_src
(
    source_id  UInt64,
    ra         Float64,
    dec        Float64,
    upix_high  UInt64,        -- HEALPix NESTED level 16, MUST be UInt64
    -- optional: for fast cosine post-filter
    cx         Float64,       -- cos(dec)*cos(ra)
    cy         Float64,       -- cos(dec)*sin(ra)
    cz         Float64,       -- sin(dec)
    -- ... other columns ...
)
ENGINE = MergeTree()
ORDER BY upix_high             -- sort key enables fast range scans
;
```

**Populating direction cosines at ingest:**

```sql
INSERT INTO proc_src SELECT
    source_id,
    ra, dec,
    healpixNested(16, ra, dec) AS upix_high,
    cos(radians(dec)) * cos(radians(ra)) AS cx,
    cos(radians(dec)) * sin(radians(ra)) AS cy,
    sin(radians(dec))                    AS cz
FROM raw_sources;
```

---

## 12. Running & Testing

### Install dependencies

```bash
# Linux
pip install healpy numpy pytest

# Windows
pip install astropy-healpix astropy numpy pytest
```

### Run debug output

```bash
python healpix_cone_search.py
```

### Run tests

```bash
pytest test_healpix_cone_search.py -v
```

### Quick usage example

```python
from healpix_cone_search import cone_search_sql, Algo

sql, post = cone_search_sql(
    ra_deg=254.0,
    dec_deg=64.0,
    radius_deg=1.0,
    table="proc_src",
    column="upix_high",
    algo=Algo.CONE,
    post_filter=True,
    post_filter_mode="cosine",
)

print(sql)
# SELECT *
# FROM proc_src
# WHERE (
#    (upix_high BETWEEN 12149850112 AND 12151947263)
#    OR ...
# )

print(post)
# AND (cx * 0.12... + cy * -0.34... + cz * 0.89... >= 0.99985)
```

---

## 13. Known Limitations

| Issue | Detail |
|---|---|
| CONE may miss edge pixels | `inclusive=False` excludes pixels whose centre is outside the cone but overlap it. Always add the post-filter. |
| Sub-pixel radius | Radii smaller than the level-16 pixel size (~3.2") fall back to a single-pixel lookup. Correct, but may miss very nearby sources in adjacent pixels. |
| Large radii (> ~5°) | CONE may return many ranges. Consider tiling the search or using a coarser pre-filter first. |
| No ClickHouse connection | This module only generates SQL strings. Execution is the caller's responsibility. |
