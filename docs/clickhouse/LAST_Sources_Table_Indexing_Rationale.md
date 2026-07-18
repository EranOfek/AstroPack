# LAST / ULTRASAT — Sources (catalog) table: index strategy rationale, per column

This document explains **why** each column of the LAST **source catalog table** (one row per source detection per image) received its ClickHouse index strategy and priority, with per-column reasoning plus insights and things to consider. A companion document covers the image/header table.

---

## How each decision was made

Every column is put through the same three-question test:

1. **Will queries filter on it?** — from expected science query patterns (cone search / crossmatch, magnitude and S/N cuts, quality/flag cuts, forced-photometry selection).
2. **If filtered, is the filter selective?** — does it prune many granules? Depends on cardinality, value distribution, and whether the on-disk order correlates with the column.
3. **Does the pruning beat the cost?** — this table has billions of rows, so every projection is paid enormously; the write/storage cost of an index dominates the decision far more than it does on the image table.

**Strategy** matches the column's behavior and filter type:

- Constant / near-constant → **none**.
- Value correlated with on-disk order → **minmax** (only helps if the sort order tracks the column).
- Semi-random float with **range** filters (magnitude, S/N, quality) → **PROJ**, a lightweight `_part_offset` projection ordered by that column. Skip indices fail on unordered values; projections re-sort by the column so ranges prune, and multiple projections combine on a multi-filter query.
- High-cardinality **exact-match** (epoch/join ids) → **bloom**.
- Bitmask → **bit** (materialize hot bits as typed columns).
- Low-cardinality equality → **set**.
- HEALPix spatial → **PK**.

**Priority** = `f(query frequency, selectivity, whether it is also a key/join column)`:

- **H** — frequently filtered and strongly selective, or a sort key. Build now.
- **M** — realistically filtered, decent selectivity. Build when query logs justify it.
- **L / —** — rare or constant / low-selectivity. Defer or skip.

**The "index most columns" caveat is sharper here.** On a billion-row table, write amplification and merge cost from many projections is the dominant risk. Treat priority as a strict **build order**: ship the H set, add M only when real queries prove it out, leave L. The behavioral class tells you the mechanism; the priority tells you whether it's worth paying for at this scale.

**Recommended sort key:** `ORDER BY (UPIX_PAR, UPIX_LOW, UPIX_HIG)`. The HEALPix spatial PK handles position for free (cone search → HEALPix ranges + a fine distance filter on a few granules), so positional columns need no separate index. The only projections worth building are on the handful of columns science queries actually cut on (magnitude, S/N, quality); bitmasks and skewed booleans get special handling.

---

## Per-column rationale

| Column | Strategy | Prio | Why & considerations |
|---|---|---|---|
| `XPEAK`,`YPEAK`,`X1`,`Y1` | none | L | Detection/first-moment pixel positions; almost never a query predicate. Position filtering is done in sky coordinates via the spatial PK, not pixels. |
| `X2`,`Y2` | PROJ | L | Second moments enable a star/galaxy (extendedness) cut; project only if you do morphology selection. |
| `XY` | none | L | Cross-moment; rarely filtered alone. |
| `SN_1`,`SN_2` | PROJ | M | Matched-filter S/N for delta and PSF hypotheses; detection-quality cuts. **Consider:** `SN_2 − SN_1` is the pipeline's hot-pixel discriminator, and `SN_3/SN_2` encodes extendedness — richer than any single value. |
| `SN_3` | PROJ | L | Slightly-extended hypothesis; secondary. |
| `BACK_IM`,`VAR_IM` | PROJ | L | Local background/variance; quality cuts. `VAR_IM` also appears in the bright-star artifact test. |
| `BACK_ANNULUS` | none | L | Annulus background; derived, rarely the filter. |
| `STD_ANNULUS` | PROJ | L | Pairs with `VAR_IM` in the bogus-source test (`STD_ANNULUS²/VAR_IM`). |
| `FLUX_APER_1/2/3`,`FLUXERR_APER_1/2/3`,`FLUX_XYPEAK`,`FLUX_PSF` | none | L | Fluxes; users filter on **magnitudes**, not fluxes, so index those instead and leave fluxes unindexed to save write cost. |
| `MAG_APER_1`,`MAG_APER_2` | PROJ | M | Aperture-magnitude cuts. |
| `MAG_APER_3` | PROJ | H | The main aperture magnitude — a hot brightness cut; semi-random float where skip indices fail, so a projection is the only thing that prunes. |
| `MAGERR_APER_1/2` | none | L | Error columns; low query value on their own. |
| `MAGERR_APER_3` | PROJ | L | Effectively an S/N cut for the main aperture. |
| `MAG_PSF` | PROJ | H | The primary magnitude and the single hottest float cut in the table; **the canonical "magnitude is semi-random → projection, never skip index" case.** Order the projection so the common cut direction (faint limit) prunes. |
| `MAGERR_PSF` | PROJ | M | Pairs with magnitude/S/N cuts for quality selection. |
| `PSF_CHI2DOF` | PROJ | M | PSF-fit reduced χ² — a strong star/artifact discriminator; a real quality filter. |
| `SN` | PROJ | H | PSF-fit S/N — extremely common cut (`SN > threshold`). **Consider** a single combined "detection-quality" projection over `(SN, MAG_PSF)` rather than one per column, to cut write cost while still pruning the usual joint filter. |
| `MITER` | set | L | Small-int iteration index; low-card equality. |
| `RA`,`Dec` | PK-served / PROJ | H | Covered by the HEALPix PK: a cone search becomes HEALPix ranges + a fine distance filter on a few granules, so RA/Dec need **no** separate index. Add a `Dec`-only projection just for wide declination-band scans. |
| `FLAGS` | **bit** | H | 32-bit mask: no index prunes `FLAGS & mask`. Decode the hot bits (saturation, edge, CR/bad-pixel) into separate `UInt8`/`Bool` columns and index those; otherwise rely on `PREWHERE`, cheap on a `UInt32`. |
| `X`,`Y`,`XFULL`,`YFULL` | none | L | PSF-fit pixel positions; not predicates (sky coords + PK cover position). |
| `MergedCatMask` | **bit** | M | Same bitmask logic as `FLAGS`. Materialize the useful membership bits ("has GAIA/QSO/CV match") as booleans and index those. |
| `DistMP` | PROJ | M | Mostly NaN (populated only within 10″ of a minor planet), so `DistMP < x` selects a tiny subset — **high selectivity**, which makes a projection genuinely effective. **Consider** a boolean `near_mp` bit instead, since the exact distance is rarely the cut. Keep it a flag, not a quality penalty. |
| `AIRMASS` | minmax / PROJ | L | Constant within an image but **scattered once the table is sorted by HEALPix**, so `minmax` won't prune here; PROJ only if queried. **Consider** not storing it per source at all — derivable by joining to the header on the epoch id, saving space over billions of rows. |
| `UPIX_PAR` | **PK** | H | Coarse HEALPix — the leading spatial sort key; drives cone search and crossmatch locality. |
| `UPIX_LOW` | **PK** | H | Mid HEALPix — second PK level; **consider** leading with this resolution so granules aren't over-fragmented while still giving cone-search selectivity. |
| `UPIX_HIG` | **PK** | H | Fine HEALPix — finest PK level for tight positional pruning. |
| `AB_ZP` | none | L | Per-image zero point; constant within an image, scattered here. Like `AIRMASS`, prefer joining from the header over storing per source. |
| `FORCED` | set / PROJ | M | Boolean but very skewed (forced points are a minority). A `set(2)` skip index isolates the forced subset; **consider** a dedicated projection or even a separate table/partition for forced photometry, since forced points have different provenance and are often queried on their own. |

---

## Things to consider (sources table)

- **This is where index cost bites.** Every projection is materialized over billions of rows and re-merged forever. Ship only the H set first: the spatial PK (`UPIX_*`), `MAG_PSF` and `SN` projections, and decoded `FLAGS` bits. Promote M columns (`MAG_APER_3` is already H; `SN_1/2`, `MAGERR_PSF`, `PSF_CHI2DOF`, `DistMP`, `FORCED`, `MergedCatMask` bits) only when query logs justify them.
- **Combine correlated projections.** `SN`, `MAG_PSF`, `MAGERR_PSF`, `PSF_CHI2DOF` are usually filtered together as one "clean detection" predicate. A single projection ordered by the primary cut (or a small composite) can serve the joint filter for a fraction of the write cost of four separate ones.
- **Don't store per-source what you can join.** `AIRMASS`, `AB_ZP` (and arguably parent-image `FWHM`, `LIMMAG`, `PT_SUCC`) are per-image constants; joining them from the header keeps the row narrow and sidesteps indexing them here entirely.
- **Bitmasks are a schema decision, not a query-layer one.** The value of `FLAGS`/`MergedCatMask` comes from decoding the few hot bits into typed columns up front — made once, in the schema, not re-derived per query.
- **HEALPix NSide is the highest-leverage knob.** Too coarse and cone searches scan whole regions; too fine and granules fragment and compression suffers. Tune the leading `UPIX` resolution to your typical search radius.
- **Positions belong to the PK, not to indexes.** RA/Dec, pixel coordinates, and the corner columns are all subsumed by the HEALPix sort order; resist the urge to add positional indexes — they'd duplicate what the PK already gives.
- **Forced photometry may warrant its own physical split.** Because forced points are a skewed minority with distinct provenance and query patterns, a separate table or partition can be cleaner and cheaper than indexing `FORCED` on the combined table.
