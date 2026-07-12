# LAST / ULTRASAT — Image (header) table: index strategy rationale, per column

This document explains **why** each column of the LAST **image / header table** (one row per image) received its ClickHouse index strategy and priority, with per-column reasoning plus insights and things to consider. A companion document covers the sources/catalog table.

---

## How each decision was made

Every column is put through the same three-question test:

1. **Will queries filter on it?** — from expected science and operations query patterns (time ranges, per-telescope ops, per-field selection, image-quality cuts, calibration/provenance lookups).
2. **If filtered, is the filter selective?** — does it prune many granules? Depends on cardinality, value distribution, and whether the on-disk order correlates with the column.
3. **Does the pruning beat the cost?** — every projection roughly re-sorts and stores its key per part and adds merge cost; every skip index adds per-granule metadata and write work.

**Strategy** matches the column's behavior and filter type:

- Constant / near-constant → **none** (an index over a value that never varies stores min=max and prunes nothing).
- Low-cardinality categorical, equality/`IN` → **set**.
- Value correlated with on-disk order (time, counters, date parts) → **minmax** (cheap; granule min/max bracket the range).
- Semi-random float with **range** filters (seeing, depth, background) → **PROJ**, a lightweight `_part_offset` projection ordered by that column (skip indices fail on unordered values; projections prune ranges and combine across filters).
- High-cardinality **exact-match** (IDs, filenames, hashes) → **bloom**.
- Bitmask → **bit** (materialize hot bits as typed columns).
- HEALPix spatial → **PK** / projection for locality.

**Priority** = `f(query frequency, selectivity, whether it is also a key/join column)`:

- **H** — frequently filtered and strongly selective, or a sort/join key. Build now.
- **M** — realistically filtered, decent selectivity. Build when query logs justify it.
- **L / —** — rare or constant / low-selectivity. Defer or skip.

**The "index most columns" caveat.** Indexing everything is an anti-pattern: write amplification and merge cost scale with the number of indexes. Treat the priority column as a **build order** — implement H now, add M as workloads prove out, leave L. The behavioral class tells you the *right* mechanism; the priority tells you whether it's worth paying for yet.

**Recommended sort key:** `ORDER BY (MOUNTNUM, CAMNUM, JD)`. Because rows are ordered by telescope then time, any time-tracking column is monotonic within a block (so `minmax` works), while pointing/quality columns are scattered (so they need projections if filtered). The table is small relative to the catalog, so the main concern is schema honesty — most FITS keywords are constants that should not be indexed at all.

---

## FITS structure & checksum

Structural keywords are identical on every row; indexing them is pure overhead with zero pruning.

| Column | Strategy | Prio | Why / consider |
|---|---|---|---|
| `SIMPLE`,`BITPIX`,`NAXIS`,`NAXIS1`,`NAXIS2`,`EXTEND`,`LONGSTRN` | none | — | Constant across all images. Consider not materializing these in the DB at all — FITS bookkeeping, not queryable science metadata. |
| `CHECKSUM`,`DATASUM` | bloom | L | High-cardinality hashes queried only by exact value (integrity checks); no order so `minmax` is useless and `set` too large. Skip unless you verify integrity in-DB. |

## File & project info

| Column | Strategy | Prio | Why / consider |
|---|---|---|---|
| `FILENAME` | bloom | M | High-card, looked up by exact name or prefix. The LAST filename encodes field/time/crop, so a `tokenbf_v1` variant also enables prefix/substring filters. |
| `CRDATE` | minmax | M | Creation time correlates with `JD`, so granule min/max bracket it cheaply. Redundant with `JD` for most queries. |
| `PROJNAME`,`NODENUMB` | set | M | Low-card categorical, equality filters. |
| `FULLPROJ` | set / PROJ | M | Encodes node/mount/camera; per-system slices. `set` for equality, PROJ to combine with other pruned filters. |
| `TIMEZONE` | none | L | Near-constant for a given site. |
| `MOUNTNUM` | **PK** | H | Sort-key head. "This telescope over time" is a core ops query; leading the PK with it makes that a contiguous scan. Low cardinality. |

**Insight:** `MOUNTNUM` + `CAMNUM` together identify the physical telescope. Leading the PK with them, then `JD`, means both "telescope X, this night" and "telescope X, this month" prune to a tight range with no secondary index.

## Software versions

| Column | Strategy | Prio | Why / consider |
|---|---|---|---|
| `GITASTRP`,`GIT_CAMR`,`GITFOCUS` | set | L | Low-card version strings; filtered only when hunting a regression tied to a build. |
| `PIPEVER` | set | M | Higher priority than the git strings because reprocessing campaigns are routinely filtered by pipeline version. |

## Observatory & environment

| Column | Strategy | Prio | Why / consider |
|---|---|---|---|
| `OBSLON`,`OBSLAT`,`OBSALT` | none | — | Site constants. |
| `MNTTEMP`,`FOCUS`,`PRVFOCUS` | none / PROJ | L | Engineering/mechanical; rarely a science filter. PROJ only for focus/thermal studies. |

## Time

All time columns except `LST` are monotonic within a `(MOUNTNUM,CAMNUM)` block, so `minmax` is the cheap natural choice; only `JD` is promoted to the PK.

| Column | Strategy | Prio | Why / consider |
|---|---|---|---|
| `JD_START` | minmax | M | Time-correlated. |
| `JD` | **PK** | H | Sort-key tail; every time-range query rides the PK. `Delta`/`DoubleDelta` codec compresses it to near-nothing. |
| `MIDJD` | minmax | M | Time-correlated; mid-exposure range queries. |
| `MINJD`,`MAXJD` | minmax | L | Visit-coadd only; time-correlated but narrow use. |
| `DATE-OBS` | minmax | M | Redundant with `JD`; index only if users query the ISO string. |
| `TIME` | none | L | String form of the time, already covered by `JD`. |
| `LST` | PROJ | L | Wraps 0–360 and interleaves across nights, so it is **not** monotonic on disk — `minmax` barely prunes; a projection helps only if you cut on sidereal time. |

## Pointing

Pointing changes all over the sky each night, so these are scattered relative to the time-ordered layout — `minmax` is useless and only projections (or a HEALPix-of-center column) help. Most mount-frame variants are engineering diagnostics that are essentially never filtered.

| Column | Strategy | Prio | Why / consider |
|---|---|---|---|
| `RA`,`DEC` | PROJ | H | Field-center cone/box search is a primary access pattern; semi-random over a fixed range → projection. **Consider** a HEALPix-of-center column (see `UPIX_*`) as the real spatial handle, with RA/DEC projections for box cuts. |
| `EQUINOX` | none | — | Constant. |
| `M_RA`,`M_HA`,`M_DEC`,`M_AZ`,`M_JRA`,`M_JDEC`,`M_ARA`,`M_AHA`,`M_ADEC`,`M_ADRA`,`M_ADHA`,`M_ADDEC` | none | L | Mount-frame engineering values; pointing-model diagnostics, not science queries. |
| `M_ALT` | PROJ | L | The one mount value worth a projection, if you cut samples by elevation. |
| `AIRMASS` | PROJ | M | Airmass-limited samples are a common quality selection; scattered vs time order so a projection, not `minmax`. |
| `TRK_RA`,`TRK_DEC` | none | — | Tracking rates; engineering. |

## Image info & camera

Most camera keywords are fixed by hardware/config (near-constant); the useful filters are the categorical descriptors of *what kind of image* this is.

| Column | Strategy | Prio | Why / consider |
|---|---|---|---|
| `IMTYPE` | set | H | science/dark/flat is filtered on almost every query. **Consider** whether this table should hold non-science rows at all; if it's science-only, `IMTYPE` collapses to a constant and the filter disappears. |
| `OBJECT` | PROJ / bloom | H | Per-target queries; medium-high cardinality → projection for scans, bloom for exact target names. |
| `FILTER` | set | M | Low card. **Insight:** if LAST effectively operates in a single passband, `FILTER` is near-constant → drop to `none`. Verify against your data. |
| `COUNTER` | minmax | L | Semi-monotonic frame counter. |
| `EXPTIME`,`MEXPTIME` | set | M/L | Discrete exposure times; `set` membership. |
| `BZERO`,`BSCALE` | none | — | FITS scaling constants. |
| `EXPMODE`,`CAMMODE`,`CAMGAIN`,`CAMOFFS`,`CAMNAME` | set | L | Low-card camera config; rarely a filter. `CAMNAME` redundant with `CAMNUM`. |
| `GAIN`,`READNOI`,`DARKCUR`,`SATURVAL`,`NONLIN`,`ORIGGAIN` | none | —/L | Detector constants/near-constants per camera. |
| `BINX`,`BINY` | none | — | Fixed binning. |
| `CAMNUM` | **PK** | H | Sort-key component (physical telescope identity with `MOUNTNUM`). |
| `CAMTEMP` | PROJ | L | Thermal studies only. |
| `CAMCOOL` | none | L | Cooling power; engineering. |

## Pipeline & identification

The `ID_*` columns are the join keys between products; high-cardinality, queried by exact value or `IN` → bloom. Directory/date parts are low-card and time-correlated.

| Column | Strategy | Prio | Why / consider |
|---|---|---|---|
| `LEVEL`,`SUBLEVEL`,`VERSION` | set | M | Data-product level/version — routine filters when selecting a product tier. |
| `ID_RAW`,`ID_PROC` | bloom | H | Primary join/lookup keys (raw ↔ processed). **Consider** `UInt64` hashes over strings for smaller, faster blooms. |
| `ID_DARK`,`ID_FLAT`,`ID_COADD` | bloom | M | Calibration/coadd join keys. |
| `ID_PROCF`,`ID_PROCL` | bloom | L | First/last-in-visit join keys; narrow use. |
| `CCDID` | set | L | Low card. |
| `SUBDIR` | bloom / PROJ | M | Per-night grouping key. |
| `FIELDID` | **PK-candidate** / PROJ | H | Per-field light curves are extremely hot; competes with `MOUNTNUM/JD` for the sort key — see insight below. |
| `DIRDAY`,`DIRMON`,`DIRYEAR` | set | M | Low-card date parts, time-correlated; `set` for equality, cheap. |
| `CROPID` | set | H | Per-crop (subimage) filter is very common since each image is cropped into subimages. |
| `LIGHTSEC`,`OVERSCAN`,`CCDSEC`,`ORIGSEC`,`ORIGUSEC`,`UNIQSEC` | none | L | Low-card geometry strings; not selective science filters. |

**Insight (PK tension):** `MOUNTNUM/CAMNUM/JD` and `FIELDID`/`UPIX` both want to lead the sort key, but there's only one. Default: time/telescope as PK (ops-oriented), field/spatial as projections. If "all images of field X across time" dominates, flip to `ORDER BY (FIELDID, JD)` and project the telescope columns — or maintain a second physical table with the alternate order.

## Calibration references, background & variance

Per-image quality metrics are semi-random floats; they help only when someone applies a quality cut, so they're projections at low–medium priority.

| Column | Strategy | Prio | Why / consider |
|---|---|---|---|
| `BIAS_IM`,`FLAT_IM` | bloom | M | "Which calibration was used" — exact-match provenance lookups. |
| `MEANBCK`,`MEDBCK`,`STDBCK` | PROJ | L | Background-level quality cuts. |
| `MINBCK`,`MAXBCK`,`MEANVAR`,`MEDVAR` | none | L | Rarely the filter; derived from the above. |
| `N_STARS` | minmax / PROJ | M | Empty/low-density image rejection. `minmax` is cheap to add but weak (counts scattered vs time order); PROJ is the reliable option. |
| `M_CHI2D`,`RP_MRMS`,`RP_MMRMS` | PROJ / none | L | Photometric/astrometric quality; PROJ only if you cut on them. |
| `AST_NSRC` | minmax | L | Count. |
| `AST_ARMS` | PROJ | M | Astrometric-quality cut — a real selection users make. |
| `AST_ERRM` | PROJ | L | Secondary astrometric metric. |

## WCS

WCS keywords are either constants (projection type, units, poles) or per-image transform values nobody filters directly; only the reference values (= field center) have query value.

| Column | Strategy | Prio | Why / consider |
|---|---|---|---|
| `CTYPE1`,`CTYPE2`,`WCSAXES`,`RADESYS`,`LONPOLE`,`LATPOLE`,`CUNIT1`,`CUNIT2` | none | — | Constant WCS conventions. |
| `CRPIX1`,`CRPIX2` | none | — | Near-constant reference pixel. |
| `CRVAL1`,`CRVAL2` | PROJ | M | These are the center RA/Dec; same spatial role as `RA`/`DEC`. |
| `CD%d_%d`,`PV%d_%d` | none | — | Transform/distortion coefficients; never filtered. |
| `PIXSCALE` | none | L | Near-constant. |
| `ROTAT` | PROJ | L | Only if you cut by field orientation. |
| `RA%d`,`DEC%d`,`RAU%d`,`DECU%d` | none | L | Corner coordinates; coverage already captured by center + HEALPix handle. |

## PSF & HEALPix

| Column | Strategy | Prio | Why / consider |
|---|---|---|---|
| `FWHE`,`FWHM` | PROJ | H | Seeing is one of the most common image-quality cuts; semi-random float → projection. |
| `PSF_FITA`,`PSF_FITB`,`PSF_ERR`,`PSF_PKR` | PROJ | L | PSF-quality diagnostics; PROJ if filtered (some NaN-heavy). |
| `PSF_FITN`,`PSF_FITT`,`PSF_DPK`,`PSF_S2`,`PSF_AF_%d` | none | L | Rarely filtered. |
| `PSF_NST` | minmax | L | Count. |
| `PSF_NPK` | set | L | Small int. |
| `UPIX_PAR` | **PK-candidate** / PROJ | H | HEALPix of image center — the clean spatial handle for field-center cone search; lead a spatial PK with it or project it. |
| `UPIX_LOW`,`UPIX_HIG` | PROJ | M | Finer spatial resolutions for tighter cone searches. |

## Photometric calibration, aperture correction, coaddition

| Column | Strategy | Prio | Why / consider |
|---|---|---|---|
| `MED_X2`,`MED_Y2`,`MED_XY` | none | L | Median shape moments; rarely filtered at image level. |
| `MAG_95Q` | PROJ | L | Depth-ish proxy. |
| `PH_ZP` | PROJ | L | Zero point; calibration monitoring. |
| `PH_COL1`,`PH_MEDC` | none | L | Color terms; not filters. |
| `PH_RMS` | PROJ | M | Photometric-quality cut. |
| `PH_NSRC` | minmax | L | Count. |
| `PH_MAGSY`,`PH_MAGT`,`PH_MAGTE` | set / none | L | Low-card descriptors of the calibration. |
| `LIMMAG` | PROJ | H | Depth cut — a very common "images deeper than X" filter; semi-random float → projection. |
| `BACKMAG` | PROJ | M | Sky-brightness / moon-condition cut. |
| `APCOR_A%d`,`APCOR_PS` | none | L | Aperture-correction coefficients; not filters. |
| `APCOR_N` | none | L | Count. |
| `NCOADD` | minmax / set | M | Coadd-depth filter; small integer, effectively discrete. |
| `COADDOP` | set | L | Low-card method. |
| `AVNCOADD`,`MINCOADD` | none | L | Coverage stats; rarely filtered. |

## Moving object & photometric transmission

| Column | Strategy | Prio | Why / consider |
|---|---|---|---|
| `GM_RATEX`,`GM_STDX`,`GM_RATEY`,`GM_STDY` | none | L | Global-motion fit diagnostics; niche. |
| `PT_RMS` | PROJ | M | Transmission-fit quality — a meaningful calibration-quality cut. |
| `PT_ARMS`,`PT_CHI2` | PROJ | L | Secondary fit-quality metrics. |
| `PT_DOF`,`PT_NCALI` | none / minmax | L | Counts. |
| `PT_SUCC` | set | M | Boolean "did the photometric solution succeed" — a natural veto filter; low cardinality but very useful. |
| `PT_AREF`,`PT_SPEC`,`PT_%d_N` | none | L | Low-card model descriptors. |
| `PT_%d_V%d`,`PT_%d_F%d` | none | L | Fit parameter values/flags; not filtered. |

---

## Things to consider (image table)

- **Build in priority order.** Ship the H set first — `MOUNTNUM/CAMNUM/JD` PK plus projections/sets on `FWHM`, `LIMMAG`, `IMTYPE`, `CROPID`, `FIELDID`, `RA/DEC`, and blooms on `ID_RAW`/`ID_PROC` — then let query logs promote M columns. Don't pre-build L.
- **Resolve the PK tension deliberately.** Time/telescope vs field/spatial is the one real design decision here; pick based on whether ops-style or field-style queries dominate, and consider a second table (or an alternate-ordered projection) if both are heavy.
- **Verify survey-specific constant collapses.** If LAST runs effectively single-filter, `FILTER` becomes constant (drop its index); if this table is science-only, `IMTYPE` does the same. These checks remove indexes that would never prune.
- **This is the small table.** Storage/write cost is not the constraint — over-indexing here is cheap relative to the catalog — but indexing constants still buys nothing, so the "none" calls stand regardless of table size.
- **The header is the natural home for image-level quality.** `FWHM`, `LIMMAG`, `PH_RMS`, `PT_SUCC`, `BACKMAG`, `AIRMASS` here can be joined into the (much larger) source table instead of being duplicated per source — index them once here.
