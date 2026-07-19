# LAST / ULTRASAT — `proc_src` index & projection recommendations (ClickHouse 26.6)

**Scope:** review of the two rationale docs against the live `proc_src.sql` / `proc_images.sql`, plus concrete recommendations for a sources table growing to ~100 B rows. Logic transfers to `visit` / `diff` / `ref` (same column families).
**Date:** 2026-07-19 • **Target engine:** ClickHouse 26.6

---

## 0. TL;DR — the four things that matter most

1. **Your live `proc_src` over-uses `minmax`.** Almost every skip index in the deployed DDL is `minmax`, and on this table `minmax` prunes *almost nothing*, because the table is sorted by HEALPix (`upix_high`) and the indexed columns (`id_*`, `sn`, `mag_*`, `jd`, `is_forced`) are **not** correlated with that order. `minmax` only works when the column tracks the on-disk sort. Replace them (details in §4).

2. **Your assumption about magnitudes is correct.** For semi-random continuous floats filtered by *range* (`mag_psf`, `mag_aper_3`, `sn`, …), a **lightweight `_part_offset` projection** is the only ClickHouse mechanism that actually prunes. `minmax`/`set`/`bloom_filter` all fail on range-over-unordered-float. See §2 for the full argument.

3. **Exact-match ID columns want `bloom_filter`, not `minmax`.** `id_uniq_src`, `id_proc_im`, `id_cat_src`, `id_proc_src` are queried by `=`/`IN` (light-curve retrieval, "all sources in image X", joins). `bloom_filter` is the right tool; `minmax` is dead weight here.

4. **Several columns should be dropped and joined from `proc_images`** — most clearly `id_raw_im`, `nodenumb`, `mountnum`, `camnum`, `cropid` (per-image / per-crop constants). Over 100 B rows this is real money. Keep `jd` (hot for time queries) even though it is technically per-image. See §3.

The current schema already does one thing right: it does **not** store per-image quality columns (`airmass`, `fwhm`, `limmag`, `ph_zp`) per source. Keep it that way.

---

## 1. What actually prunes in ClickHouse 26.6 (the mechanics you're deciding between)

Your version sits right after the big projection overhaul, so the rules are different from what most older material describes:

| Mechanism | Prunes on | Works when… | Fails when… |
|---|---|---|---|
| **Primary key (sparse index)** | leading sort cols, ranges + equality | column is (a prefix of) `ORDER BY` | column not in sort key |
| **`minmax` skip index** | ranges & equality | column **correlates with on-disk order** | column is scattered (min/max of each granule spans the whole range → no skip) |
| **`set(N)` skip index** | equality / `IN` | low cardinality **and** values clustered into few granules | every granule contains most values (scattered low-card) |
| **`bloom_filter`** | equality / `IN` / `has` | high-cardinality **exact match** | range filters (`<`, `BETWEEN`) — bloom can't do ranges |
| **Lightweight `_part_offset` projection** | ranges & equality on the projection's sort key | you need to prune a scattered column by range | you filter on something not in any projection's order |

Key 26.x facts (they landed in the 25.5 → 26.1 window, so 26.6 has all of them):

- **Lightweight projections** store only their sort key + a `_part_offset` pointer back to the base part — a few percent of storage instead of a full data copy. They act as a true secondary index.
- **Granule-level pruning** for these projections arrived in **25.11** — before that they only pruned whole parts. On 26.6 they prune down to individual 8192-row granules.
- **Multiple lightweight projections cooperate in one query** (since 25.6): each filter can be served by its own projection's primary index to prune granules, and if one filter also matches the base PK, that participates too. **Caveat:** ClickHouse still *reads row data from only one source* (one projection, or the base table) — the others only prune. So building several single-column lightweight projections is now viable; you don't have to hand-craft one composite.
- **26.1 added a friendlier syntax** for defining index-like projections (the explicit `SELECT _part_offset ORDER BY col` form below still works and is unambiguous).
- Two settings gate the optimizer: `max_projection_rows_to_use_projection_index` and `min_table_rows_to_use_projection_index`. Tune these if EXPLAIN shows a projection *not* being used when it should.

> Always verify with `EXPLAIN projections = 1` / `EXPLAIN indexes = 1` and disable caches while benchmarking. Don't trust that the optimizer picked what you expect.

**Trade-off to remember:** a lightweight projection is ~2× slower than a *full* projection but ~½ the storage, because it pays an extra base-table read per matched granule. At 100 B rows you want lightweight almost everywhere; reserve full projections for cases where the projection can answer the whole query (rare here, since you fetch wide rows).

---

## 2. "Do magnitudes just want projections and nothing else?" — Yes, with one caveat

Walk a `WHERE mag_psf < 20.5` filter through every mechanism:

- **`minmax`** — `mag_psf` has no relationship to HEALPix order. Each 8192-row granule (and each `GRANULARITY 16` block = 131 072 rows) contains bright *and* faint sources, so its min/max spans nearly the full magnitude range. Nothing is skipped. **Fails.**
- **`set`** — high cardinality (continuous float) and it's a range, not equality. **Fails.**
- **`bloom_filter`** — membership only; can't answer `<`/`BETWEEN`. **Fails.**
- **Lightweight `_part_offset` projection ordered by `mag_psf`** — re-sorts by magnitude, so its own primary index turns `mag_psf < 20.5` into a contiguous range and prunes granules. **Works — and it's the only thing that does.**

So your intuition is right: **magnitudes, magnitude errors, S/N, and other semi-random continuous floats are projection-only.** Same for `sn`, `sn_1…sn_5`, `mag_aper_*`, `magerr_*`, `back_im`, `var_im`, `std_annulus`, `nepoch`.

**The one caveat.** A projection on `mag_psf` earns its keep only for queries where the magnitude cut is *not* already wrapped inside a tight cone search. If essentially every science query is "cone search + magnitude cut", the HEALPix PK has already reduced you to a handful of granules and a plain scan of `mag_psf` over those is basically free — the projection buys little. The magnitude projection matters most for **magnitude-driven, spatially-unconstrained** queries ("all sources fainter than X across a big region / all-sky"). Decide per your real query mix — see the questions in §8.

Ordering tip: order the projection by the dominant cut direction. Faint-limit cuts (`mag < limit`) and bright cuts both prune with a single ascending order, so `ORDER BY mag_psf` is fine. If a magnitude cut is *always* paired with an S/N cut, a composite `ORDER BY (sn, mag_psf)` (most selective first) lets one projection serve the joint filter and read directly — but with 26.6's multi-projection cooperation you can also just build the two separately.

---

## 3. Columns to remove from `proc_src` and join from `proc_images`

The join key is `id_proc_im` → `proc_images` (add a `bloom_filter` on `proc_images.id_proc` if not already fast). Candidates, ranked by confidence:

| Column(s) | Per-image constant? | Recommendation | Notes |
|---|---|---|---|
| `id_raw_im` | derivable | **Remove, join** | `proc_images` has both `id_raw` and `id_proc`; the raw↔proc mapping lives there. Storing `id_raw_im` per source duplicates it 100 B times (~0.8 TB). Keep only if raw-image lookups are hot *and* latency-critical. |
| `nodenumb`, `mountnum`, `camnum` | yes | **Remove, join** (or keep as projection — see caveat) | Physical telescope identity; one value per image/crop. All present in `proc_images`. |
| `cropid` | yes | **Remove, join** | If `proc_images` is one row per *(image, crop)*, `cropid` is already implied by `id_proc_im`. Confirm `proc_images` granularity (§8 Q6). |
| `id` (UInt128 MATERIALIZED) | derived | **Make it `ALIAS`, not `MATERIALIZED`** | It's just `bitOr(id_proc_im<<64, id_proc_src)`. `MATERIALIZED` **stores** 16 B/row (~1.6 TB at 100 B); `ALIAS` computes on read and still works in `WHERE`. Only keep it materialized if you index it or sort by it. |
| `jd` | yes (per-image) | **Keep** (don't join) | Technically joinable from `proc_images.midjd`, but time-range is a hot predicate; a join-then-filter over 100 B rows is far worse than keeping + projecting it. See §5 for putting `jd` in the sort key. |

**Not removable** (genuinely per-source, direction-dependent): `bjd` (barycentric JD depends on RA/Dec), `baryvel`, `back_im`, `var_im`, `back_annulus`, `std_annulus`, all `mag_*`/`sn_*`/moment columns.

> **Correlation caveat for the categoricals.** If LAST assigns each sky field to a fixed `(mountnum, camnum)`, then those columns *correlate with position* → they correlate with the HEALPix sort → a cheap `minmax`/`set` would actually prune, and they'd also compress extremely well inline. If field→telescope is **not** fixed, they're scattered under the spatial sort and neither `set` nor `minmax` prunes them — in that case removing + joining is strictly better than indexing them. This is the single biggest "it depends" in the schema (§8 Q4).

---

## 4. Per-column strategy for `proc_src` (live DDL)

`cur` = index in the deployed `proc_src.sql`. **Bold** = change from current. Priority = build order (H now, M when logs justify, L defer).

### Identity / join keys
| Column | cur | Recommend | Prio | Why |
|---|---|---|---|---|
| `id_proc_src` | minmax | **bloom_filter** (or drop) | M | Local detection id; exact-match/provenance only. `minmax` scattered → useless. |
| `id_uniq_src` | minmax | **bloom_filter** | H | Cross-epoch / light-curve retrieval `WHERE id_uniq_src = …`. Scattered id values → `minmax` dead; bloom finds the few granules. |
| `id_cat_src` | minmax | **bloom_filter** | M | Link to reference/catalog source; `=`/`IN`/join. |
| `id_raw_im` | minmax | **remove + join** (else bloom) | L | Derivable from `id_proc_im`; see §3. |
| `id_proc_im` | minmax | **bloom_filter** | H | Hottest join key: "all sources in image X" + join to header. |
| `id` (UInt128) | — | **ALIAS**; bloom only if queried directly | M | Derived; save ~1.6 TB (§3). |

### Spatial (HEALPix) — the PK
| Column | cur | Recommend | Prio | Why |
|---|---|---|---|---|
| `upix_partition` | partition key | **keep as PARTITION** (verify count, §8 Q1) | H | Coarse HEALPix partition. |
| `upix_low` | minmax | **keep minmax**, or promote into `ORDER BY` | H | Coarser HEALPix, correlated with sort → `minmax` genuinely prunes. Cheap. |
| `upix_high` | minmax | **drop the minmax** (it's the sort key) | H | The sparse PK already brackets it per granule; a `minmax` on the sort key is redundant. |

### Positions (pixel & sky)
| Column | cur | Recommend | Prio | Why |
|---|---|---|---|---|
| `ra` | minmax | **keep minmax** (PK-served for cone) | M | HEALPix sort makes each granule a compact patch → `ra` range is narrow *except near the 0/360 wrap*, where min/max blows up. Cheap helper for box cuts; PK covers cone search. |
| `dec` | minmax | **keep minmax** (add PROJ only if needed) | M | No wrap → `dec` `minmax` is reliable under spatial sort; good for declination-band scans. A `_part_offset` projection on `dec` only if `minmax` proves too coarse. |
| `xpeak`,`ypeak`,`x`,`y`,`x1`,`y1` | none | **none** | L | Pixel/first-moment positions; never predicates (position filtering is in sky coords via PK). |
| `x2`,`y2` | none | PROJ | L | Second moments → star/galaxy (extendedness) cut. Project only if you do morphology selection. |
| `xy` | none | none | L | Cross-moment; rarely filtered alone. |

### Brightness / S-N (semi-random floats → projections)
| Column | cur | Recommend | Prio | Why |
|---|---|---|---|---|
| `mag_psf` | minmax | **lightweight PROJ `ORDER BY mag_psf`** | H | Primary magnitude, hottest float cut. §2. |
| `mag_aper_3` | minmax | **lightweight PROJ** | H | Main aperture magnitude. |
| `mag_aper_1`,`mag_aper_2` | none | PROJ | M | Secondary aperture cuts. |
| `magerr_psf` | none | PROJ | M | Quality; pairs with mag/S-N. |
| `magerr_aper_3` | none | PROJ | L | Effectively an S/N cut for the main aperture. |
| `magerr_aper_1/2` | none | none | L | Low standalone value. |
| `sn` | minmax | **lightweight PROJ `ORDER BY sn`** | H | PSF-fit S/N; `SN > t` is the most common quality cut. |
| `sn_delta` | none | PROJ | M | Hot-pixel / CR discriminator (precomputed `SN_2−SN_1`); a real veto filter. |
| `sn_1`,`sn_2` | none | PROJ | M | Matched-filter S/N (delta & PSF hypotheses). Consider one composite projection over `(sn, sn_1, sn_2)` if always filtered together. |
| `sn_3`,`sn_4`,`sn_5` | none | PROJ | L | Extended-source hypotheses; secondary. |
| `sn_ext1`,`sn_ext2` | none | PROJ | L | Extended S/N; Nullable, niche. |
| `flux_aper_*`,`fluxerr_aper_*`,`flux_psf`,`fluxerr_psf` | none | **none** | L | Users cut on **magnitudes**, not fluxes. Index the mags, leave fluxes unindexed to save write cost. |

### Background / variance (quality projections)
| Column | cur | Recommend | Prio | Why |
|---|---|---|---|---|
| `back_im`,`var_im` | none | PROJ | L | Local background/variance quality cuts; `var_im` also feeds the bright-star artifact test. Nullable. |
| `std_annulus` | none | PROJ | L | Pairs with `var_im` in the bogus-source test (`std_annulus²/var_im`). |
| `back_annulus` | none | none | L | Derived; rarely the filter. |

### Time
| Column | cur | Recommend | Prio | Why |
|---|---|---|---|---|
| `jd` | minmax | **lightweight PROJ `ORDER BY jd`**, and/or add to sort key (§5) | H | Scattered under HEALPix sort → `minmax` weak. Time-range is hot. Best option is `ORDER BY (upix_high, jd)` so it's PK-served; projection otherwise. |
| `bjd` | none | PROJ (low) | L | Per-source barycentric time; only if you cut on barycentric epoch specifically. `jd` usually suffices. |
| `ingestion_time`,`ingestion_time_jd` | none | minmax or small PROJ | L | Ops "what landed recently". Insertion order ≠ storage order after the spatial sort, so `minmax` is weak; a projection is reliable but low value. |

### Flags / booleans / counts / categoricals
| Column | cur | Recommend | Prio | Why |
|---|---|---|---|---|
| `flags` (UInt32) | none | **decode hot bits → typed cols, index those** | H | No index prunes `flags & mask`. Materialize the hot bits (saturation, edge, CR/bad-pixel) as `Bool`/`UInt8` and put a projection/`set` on those; `PREWHERE flags` is cheap on a `UInt32` as a fallback. This is a *schema* decision, made once. |
| `is_forced` (Bool) | minmax | **lightweight PROJ `ORDER BY is_forced`**, or physical split | M | Skewed boolean scattered across granules → **`set(2)` and `minmax` both fail** (most granules hold both values). A projection groups the minority so it prunes; or split forced photometry into its own table/partition (distinct provenance & query patterns). |
| `nepoch` | minmax | **lightweight PROJ** (or `set` if effectively discrete/small-range) | M | "Well-observed" range cut (`nepoch > N`). Scattered → `minmax` weak. |
| `nodenumb`,`mountnum`,`camnum`,`cropid` | none | **remove + join** (§3); else PROJ (not `set`, unless field↔telescope is fixed) | L/M | Per-image constants. Under spatial sort they're scattered → `set` won't prune. If field→telescope is fixed they correlate with position → cheap `set`/`minmax` works *and* they compress well. Depends on §8 Q4. |
| `baryvel` | none | none | L | Per-source velocity; essentially never a filter. |

---

## 5. Sort key & partitioning review

**Current:**
- `proc_src`: `PARTITION BY upix_partition`, `ORDER BY upix_high`
- `proc_images`: `PARTITION BY upix_partition`, `ORDER BY upix_low`

**Assessment.** The *different* ORDER BY between the two tables is defensible, not a bug: images cover large footprints so a coarse HEALPix (`upix_low`) sort is right; sources are point-like so the finest HEALPix (`upix_high`) gives the tightest cone-search pruning. The sources rationale doc's suggestion to use the same `(upix_par, upix_low, upix_high)` tuple for both is slightly off — your split is reasonable.

**Two things to consider:**

1. **Add `jd` as a secondary sort key on `proc_src`:** `ORDER BY (upix_high, jd)` (or `(upix_low, upix_high, jd)`). Because all detections of one physical source share a position, they already sit in the same HEALPix granule; ordering by `jd` within that makes **light curves time-ordered and contiguous**, and makes cone-search + time-window queries PK-served — removing the need for a separate `jd` projection. `jd` compresses to near-nothing with `Delta`/`DoubleDelta`. This is high-leverage for a variability survey.

2. **Check the partition count.** `PARTITION BY upix_partition` is only healthy if it yields, roughly, hundreds-to-a-few-thousand partitions — not hundreds of thousands. That depends entirely on the NSide of `upix_partition` (§8 Q1). At 100 B rows, too many partitions means too many parts, slow merges, and a bloated parts list. If `upix_partition` is fine, partition by something coarser (a low-NSide HEALPix, or `toYYYYMM(jd)`) instead.

**Nesting check.** If `upix_partition ⊃ upix_low ⊃ upix_high` are nested (each a coarser HEALPix of the same point), then `ORDER BY upix_high` already implies ordering by `upix_low` and `upix_partition`, and the `minmax` on `upix_low` is a cheap correlated helper (keep) while `minmax` on `upix_high` is redundant (drop). If they are **not** nested (different NSide not power-of-2 related, or ring vs nested numbering), the analysis changes — confirm in §8 Q1.

---

## 6. Recommended build order at 100 B rows

This is where index cost bites: every projection is materialized over 100 B rows and re-merged forever. Ship the **H** set only; promote **M** when query logs prove the need.

**Ship now (H):**
- Sort key: `ORDER BY (upix_high, jd)` (fold `jd` in) + keep `upix_partition` partition (after the count check).
- `bloom_filter` on `id_uniq_src`, `id_proc_im` (and `id_cat_src`, `id_proc_src` if queried).
- Lightweight `_part_offset` projections on `mag_psf`, `sn`, `mag_aper_3`.
- Decode the hot `flags` bits into typed columns; index those.
- Keep `minmax` on `upix_low` and `dec`; drop `minmax` on `upix_high` and all the ID columns; convert the rest of the ineffective `minmax` per the table.
- Schema slimming: remove `id_raw_im`, `nodenumb`, `mountnum`, `camnum`, `cropid` (join from header); make `id` an `ALIAS`.

**Add when logs justify (M):** projections on `mag_aper_1/2`, `magerr_psf`, `sn_1/sn_2/sn_delta`, `nepoch`; `is_forced` projection or physical split; `bloom` on `id_cat_src`/`id_proc_src`.

**Leave (L):** flux columns, second moments, annulus/background, `bjd`, `baryvel`, ingestion columns — build only for a specific proven query.

**Syntax reminders:**
```sql
-- lightweight (index-like) projection: tiny storage, granule-level pruning on 26.6
ALTER TABLE last.proc_src
  ADD PROJECTION proj_mag_psf (SELECT _part_offset ORDER BY mag_psf);
ALTER TABLE last.proc_src MATERIALIZE PROJECTION proj_mag_psf;  -- backfill = a mutation; watch system.mutations

-- exact-match id
ALTER TABLE last.proc_src
  ADD INDEX idx_id_uniq_src id_uniq_src TYPE bloom_filter(0.01) GRANULARITY 1;

-- decoded flag bit, then index the typed column (bit position per your FLAGS definition)
ALTER TABLE last.proc_src
  ADD COLUMN flag_saturated UInt8 MATERIALIZED bitAnd(bitShiftRight(flags, /*bit*/ 0), 1);
```
> Materializing projections/indexes on existing 100 B-row partitions is a background mutation — plan it, and verify effect with `EXPLAIN projections = 1`.

---

## 7. Corrections to the two rationale docs

**Sources doc — mostly right, three fixes:**
- ✅ "Semi-random float ranges → projection; skip indices fail" — correct, and now granule-level effective on 26.6.
- ✅ "Multiple projections combine on a multi-filter query" — correct on 26.6 (was false before 25.6). Nuance to add: only **one** projection is *read from*; the rest only prune. So build several single-column lightweight projections freely.
- ⚠️ It describes an **older column set** (`MAG_PSF`, `PSF_CHI2DOF`, `MergedCatMask`, `DistMP`, `AIRMASS`, `AB_ZP`, `MITER`, `XFULL/YFULL`, `FLUX_XYPEAK`, `FORCED`) that doesn't match the live `proc_src`. Re-map to live names (`is_forced`, `sn_delta`, `sn_ext*`, etc.) before using it as a checklist.
- ⚠️ `FORCED`/`is_forced` → it suggests `set(2)`. Under the HEALPix sort, forced points are scattered, so `set(2)` won't prune. Use a projection or a physical split instead.
- ⚠️ RA/Dec → it proposes a `Dec`-only projection. A cheap `minmax` on `dec` usually suffices thanks to the spatial sort; reach for a projection only if `minmax` under-prunes.

**Image doc — solid; the one live-schema mismatch:**
- The image rationale recommends `ORDER BY (MOUNTNUM, CAMNUM, JD)`, but the deployed `proc_images` uses `ORDER BY upix_low` (spatial). These encode opposite priorities (ops/telescope-time vs spatial). That's the real "PK tension" the doc flags — just note the deployed table already chose spatial. Decide deliberately; a second alternate-ordered table (or projection) is the escape hatch if both ops and spatial access are heavy.
- Its closing point — *image-level quality (`FWHM`, `LIMMAG`, `AIRMASS`, `PH_ZP`) lives in the header and is joined into sources, not duplicated* — is exactly what the live `proc_src` already does. Good; preserve it.

---

## 8. Questions that would change these recommendations

These are the genuine unknowns where my advice flips depending on the answer:

1. **HEALPix scheme.** What NSide does each of `upix_partition` / `upix_low` / `upix_high` use, nested or ring, and are they hierarchically nested? → decides the sort key, whether `minmax` on `upix_low` is redundant, and (critically) **how many partitions** `PARTITION BY upix_partition` produces at 100 B rows.
2. **Query mix — spatial vs not.** Do `mag_psf`/`sn` cuts almost always come *with* a cone search, or are there heavy magnitude-driven all-sky/large-region queries? → decides whether the `mag_psf`/`sn` projections are H or optional.
3. **Light-curve access.** Is `WHERE id_uniq_src = …` (per-source light curve) a dominant pattern? → confirms the `bloom` priority and strongly favors folding `jd` into the sort key.
4. **Field → telescope mapping.** Is each sky field always observed by a fixed `(mountnum, camnum)`? → decides whether `mountnum`/`camnum`/`cropid` are cheap `set`/`minmax` (correlated) vs must-project vs best-removed.
5. **Forced photometry.** What fraction of rows have `is_forced = true`, and are forced points usually queried on their own? → set vs projection vs separate table.
6. **`proc_images` granularity.** Is `proc_images` one row per full image, or one row per *(image, crop)*? → confirms `cropid` (and the mount/cam columns) are truly derivable from `id_proc_im`, and whether the `id_raw_im` join is 1:1.
7. **`id_cat_src` semantics.** Does this point at an external reference catalog (GAIA/etc.) or an internal unique-source catalog? → confirms `bloom` and whether a crossmatch-membership bit (like the doc's `MergedCatMask`) is worth materializing.
