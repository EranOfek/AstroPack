# LAST / ULTRASAT — index strategy rationale, per column

Companion to the column-types & indexing reference. This document explains **why** each column got its strategy and priority, for the **Header (images)** table and the **Catalog (sources)** table, with per-column reasoning plus insights and things to consider.

---

## How each decision was made

Every column is put through the same three-question test:

1. **Will queries filter on it?** — from the expected science and operations query patterns (cone search, magnitude/quality cuts, per-field light curves, per-telescope ops, calibration lookups).
2. **If filtered, is the filter selective?** — does it prune many granules? This depends on cardinality, value distribution, and whether the on-disk order correlates with the column.
3. **Does the pruning benefit beat the cost?** — every projection roughly re-sorts and stores the key per part and adds merge cost; every skip index adds per-granule metadata and write work.

**Strategy** is the mechanism that matches the column's behavioral class and filter type:

- Constant / near-constant → **none** (an index over a value that never varies stores min=max and prunes nothing).
- Low-cardinality categorical, equality/`IN` → **set** (small membership set per granule).
- Value correlated with on-disk order (time, counters, date parts) → **minmax** (cheap; granule min/max actually bracket the range).
- Semi-random float with **range** filters (magnitude, S/N, seeing, background) → **PROJ** (skip indices fail here because the values aren't ordered on disk; a lightweight `_part_offset` projection re-sorts by that column so ranges prune, and projections combine across filters).
- High-cardinality **exact-match** (IDs, filenames, hashes) → **bloom** (membership test; no order for minmax, too many values for set).
- Bitmask → **bit** (standard indexes can't prune `col & mask`; materialize hot bits).
- HEALPix spatial → **PK** (locality for cone search / crossmatch).

**Priority** is `f(query frequency, selectivity, whether it is also a key/join column)`:

- **H** — frequently filtered and strongly selective, or it is a sort/join key. Build now.
- **M** — realistic to filter, decent selectivity. Build when query logs justify it.
- **L / —** — rarely filtered, or constant / low-selectivity. Defer or skip.

**The "index most columns" caveat.** Indexing everything is an anti-pattern: write amplification and merge cost scale with the number of projections/skip indices. So treat the priority column as a **build order** — implement H now, add M as real workloads prove out, and leave L unless a specific query demands it. The behavioral class tells you the *right* mechanism for a column; the priority tells you whether it's worth paying for yet.

---

# Header (images) table

Sorted by `ORDER BY (MOUNTNUM, CAMNUM, JD)`. Because rows are ordered by telescope then time, any column that tracks time is monotonic within a block (so `minmax` works), while pointing- and quality-type columns are scattered (so they need projections if filtered). The table is small relative to the catalog, so the dominant cost concern is not storage but keeping the schema honest — most FITS keywords are constants that should not be indexed at all.

### FITS structure & checksum

Structural keywords are identical on every row; indexing them is pure overhead with zero pruning.

| Column | Strategy | Prio | Why / consider |
|---|---|---|---|
| `SIMPLE`,`BITPIX`,`NAXIS`,`NAXIS1`,`NAXIS2`,`EXTEND`,`LONGSTRN` | none | — | Constant across all images. Consider not materializing these in the DB at all — they're FITS bookkeeping, not queryable science metadata. |
| `CHECKSUM`,`DATASUM` | bloom | L | High-cardinality hashes only ever queried by exact value (integrity checks); no order so `minmax` is useless and `set` too large. Consider skipping unless you actually verify integrity in-DB. |

### File & project info

| Column | Strategy | Prio | Why / consider |
|---|---|---|---|
| `FILENAME` | bloom | M | High-card, looked up by exact name or prefix. The LAST filename encodes field/time/crop, so a `tokenbf_v1` variant additionally enables prefix/substring filters. |
| `CRDATE` | minmax | M | File-creation time correlates with `JD`, so granule min/max bracket it cheaply. Redundant with `JD` for most queries. |
| `PROJNAME`,`NODENUMB` | set | M | Low-card categorical, equality filters. |
| `FULLPROJ` | set / PROJ | M | Encodes node/mount/camera; per-system slices. `set` if used for equality, PROJ if you want it combinable with other pruned filters. |
| `TIMEZONE` | none | L | Near-constant for a given site. |
| `MOUNTNUM` | **PK** | H | Sort-key head. "This telescope over time" is a core ops query; leading the PK with it makes that a contiguous scan. Low cardinality. |

**Insight:** `MOUNTNUM` + `CAMNUM` together identify the physical telescope. Leading the PK with them, then `JD`, means both "telescope X, this night" and "telescope X, this month" prune to a tight range with no secondary index.

### Software versions

| Column | Strategy | Prio | Why / consider |
|---|---|---|---|
| `GITASTRP`,`GIT_CAMR`,`GITFOCUS` | set | L | Low-card version strings; you only filter these when hunting a regression tied to a build. |
| `PIPEVER` | set | M | Higher priority than the git strings because reprocessing campaigns are routinely filtered by pipeline version. |

### Observatory & environment

| Column | Strategy | Prio | Why / consider |
|---|---|---|---|
| `OBSLON`,`OBSLAT`,`OBSALT` | none | — | Site constants. |
| `MNTTEMP`,`FOCUS`,`PRVFOCUS` | none / PROJ | L | Engineering/mechanical; rarely a science filter. PROJ only if you do focus/thermal studies. |

### Time

All time columns except `LST` are monotonic within a `(MOUNTNUM,CAMNUM)` block, so `minmax` is the cheap, natural choice; only `JD` is promoted to the PK.

| Column | Strategy | Prio | Why / consider |
|---|---|---|---|
| `JD_START` | minmax | M | Time-correlated. |
| `JD` | **PK** | H | Sort-key tail; every time-range query rides the PK. `Delta`/`DoubleDelta` codec compresses it to near-nothing. |
| `MIDJD` | minmax | M | Time-correlated; use for mid-exposure range queries. |
| `MINJD`,`MAXJD` | minmax | L | Visit-coadd only; time-correlated but narrow use. |
| `DATE-OBS` | minmax | M | Redundant with `JD`; index only if users query the ISO string. |
| `TIME` | none | L | String form of the time already covered by `JD`. |
| `LST` | PROJ | L | Wraps 0–360 and interleaves across nights, so it is **not** monotonic on disk — `minmax` would barely prune; a projection is the only thing that helps, and only if you cut on sidereal time. |

### Pointing

Pointing is where the telescope looked, which changes all over the sky each night, so these are scattered relative to the time-ordered layout — `minmax` is useless and only projections (or a HEALPix-of-center column) help. Most of the mount-frame variants are engineering diagnostics that are essentially never filtered.

| Column | Strategy | Prio | Why / consider |
|---|---|---|---|
| `RA`,`DEC` | PROJ | H | Field-center cone/box search is a primary access pattern; semi-random over a fixed range → projection. **Consider** a HEALPix-of-center column instead (see `UPIX_*` below) as the real spatial handle, with RA/DEC projections for box cuts. |
| `EQUINOX` | none | — | Constant. |
| `M_RA`,`M_HA`,`M_DEC`,`M_AZ`,`M_JRA`,`M_JDEC`,`M_ARA`,`M_AHA`,`M_ADEC`,`M_ADRA`,`M_ADHA`,`M_ADDEC` | none | L | Mount-frame engineering values; used for pointing-model diagnostics, not science queries. |
| `M_ALT` | PROJ | L | The one mount value worth a projection, if you cut samples by elevation. |
| `AIRMASS` | PROJ | M | Airmass-limited samples are a common quality selection; scattered vs the time order so a projection, not `minmax`. |
| `TRK_RA`,`TRK_DEC` | none | — | Tracking rates; engineering. |

### Image info & camera

Most camera keywords are fixed by hardware/config (near-constant); the genuinely useful filters are the categorical descriptors of *what kind of image* this is.

| Column | Strategy | Prio | Why / consider |
|---|---|---|---|
| `IMTYPE` | set | H | science/dark/flat is filtered on almost every query. **Consider** whether this table should even hold non-science rows; if it's science-only, `IMTYPE` collapses to a constant and the filter disappears. |
| `OBJECT` | PROJ / bloom | H | Per-target queries; medium-high cardinality → projection for range/scan, bloom for exact target names. |
| `FILTER` | set | M | Low card. **Insight:** if LAST effectively operates in a single passband, `FILTER` is near-constant → drop to `none`. Verify against your data before building. |
| `COUNTER` | minmax | L | Semi-monotonic frame counter. |
| `EXPTIME`,`MEXPTIME` | set | M/L | Discrete exposure times; `set` membership. |
| `BZERO`,`BSCALE` | none | — | FITS scaling constants. |
| `EXPMODE`,`CAMMODE`,`CAMGAIN`,`CAMOFFS`,`CAMNAME` | set | L | Low-card camera config; rarely a filter. `CAMNAME` is redundant with `CAMNUM`. |
| `GAIN`,`READNOI`,`DARKCUR`,`SATURVAL`,`NONLIN`,`ORIGGAIN` | none | —/L | Detector constants/near-constants per camera. |
| `BINX`,`BINY` | none | — | Fixed binning. |
| `CAMNUM` | **PK** | H | Sort-key component (physical telescope identity with `MOUNTNUM`). |
| `CAMTEMP` | PROJ | L | Thermal studies only. |
| `CAMCOOL` | none | L | Cooling power; engineering. |

### Pipeline & identification

The `ID_*` columns are the join keys between products; they're high-cardinality and queried by exact value or `IN`, which is the bloom sweet spot. Directory/date parts are low-card and time-correlated.

| Column | Strategy | Prio | Why / consider |
|---|---|---|---|
| `LEVEL`,`SUBLEVEL`,`VERSION` | set | M | Data-product level/version — routine filters when selecting a product tier. |
| `ID_RAW`,`ID_PROC` | bloom | H | Primary join/lookup keys between raw ↔ processed. **Consider** storing as `UInt64` hashes rather than strings for smaller, faster blooms. |
| `ID_DARK`,`ID_FLAT`,`ID_COADD` | bloom | M | Calibration/coadd join keys. |
| `ID_PROCF`,`ID_PROCL` | bloom | L | First/last-in-visit join keys; narrow use. |
| `CCDID` | set | L | Low card. |
| `SUBDIR` | bloom / PROJ | M | Per-night grouping key. |
| `FIELDID` | **PK-candidate** / PROJ | H | Per-field light curves are extremely hot. It competes with `MOUNTNUM/JD` for the sort key — see insight below. |
| `DIRDAY`,`DIRMON`,`DIRYEAR` | set | M | Low-card date parts, time-correlated; `set` for equality, cheap. |
| `CROPID` | set | H | Per-crop (subimage) filter is very common since each image is cropped into subimages. |
| `LIGHTSEC`,`OVERSCAN`,`CCDSEC`,`ORIGSEC`,`ORIGUSEC`,`UNIQSEC` | none | L | Low-card section strings that describe geometry; not selective science filters. |

**Insight (PK tension):** `MOUNTNUM/CAMNUM/JD` and `FIELDID`/`UPIX` both want to lead the sort key, but there's only one. Default: time/telescope as PK (ops-oriented), field/spatial as projections. If "all images of field X across time" is your dominant query, flip to `ORDER BY (FIELDID, JD)` and project the telescope columns — or maintain a second physical table with the alternate order.

### Calibration references, background & variance

Per-image quality metrics are semi-random floats; they help only when someone applies a quality cut, so they're projections at low–medium priority.

| Column | Strategy | Prio | Why / consider |
|---|---|---|---|
| `BIAS_IM`,`FLAT_IM` | bloom | M | "Which calibration was used" — exact-match provenance lookups. |
| `MEANBCK`,`MEDBCK`,`STDBCK` | PROJ | L | Background-level quality cuts. |
| `MINBCK`,`MAXBCK`,`MEANVAR`,`MEDVAR` | none | L | Rarely the filter; derived from the above. |
| `N_STARS` | minmax / PROJ | M | Empty/low-density image rejection. `minmax` is cheap to add but weak (counts are scattered vs time order); PROJ is the reliable option. |
| `M_CHI2D`,`RP_MRMS`,`RP_MMRMS` | PROJ / none | L | Photometric/astrometric quality; PROJ only if you cut on them. |
| `AST_NSRC` | minmax | L | Count. |
| `AST_ARMS` | PROJ | M | Astrometric-quality cut — a real selection users make. |
| `AST_ERRM` | PROJ | L | Secondary astrometric metric. |

### WCS

The WCS keywords are either constants (projection type, units, poles) or per-image transform values that nobody filters on directly; the only ones with query value are the reference values, which equal the field center.

| Column | Strategy | Prio | Why / consider |
|---|---|---|---|
| `CTYPE1`,`CTYPE2`,`WCSAXES`,`RADESYS`,`LONPOLE`,`LATPOLE`,`CUNIT1`,`CUNIT2` | none | — | Constant WCS conventions. |
| `CRPIX1`,`CRPIX2` | none | — | Near-constant reference pixel. |
| `CRVAL1`,`CRVAL2` | PROJ | M | These are the center RA/Dec; same spatial role as `RA`/`DEC`. |
| `CD%d_%d`,`PV%d_%d` | none | — | Transform/distortion coefficients; never filtered. |
| `PIXSCALE` | none | L | Near-constant. |
| `ROTAT` | PROJ | L | Only if you cut by field orientation. |
| `RA%d`,`DEC%d`,`RAU%d`,`DECU%d` | none | L | Corner coordinates; spatial coverage is already captured by the center + a HEALPix handle. |

### PSF & HEALPix

| Column | Strategy | Prio | Why / consider |
|---|---|---|---|
| `FWHE`,`FWHM` | PROJ | H | Seeing is one of the most common image-quality cuts; semi-random float → projection. |
| `PSF_FITA`,`PSF_FITB`,`PSF_ERR`,`PSF_PKR` | PROJ | L | PSF-quality diagnostics; PROJ if you filter on them (some are NaN-heavy). |
| `PSF_FITN`,`PSF_FITT`,`PSF_DPK`,`PSF_S2`,`PSF_AF_%d` | none | L | Rarely filtered. |
| `PSF_NST` | minmax | L | Count. |
| `PSF_NPK` | set | L | Small int. |
| `UPIX_PAR` | **PK-candidate** / PROJ | H | HEALPix of the image center — the clean spatial handle for field-center cone search; either lead a spatial PK with it or project it. |
| `UPIX_LOW`,`UPIX_HIG` | PROJ | M | Finer spatial resolutions for tighter cone searches. |

### Photometric calibration, aperture correction, coaddition

| Column | Strategy | Prio | Why / consider |
|---|---|---|---|
| `MED_X2`,`MED_Y2`,`MED_XY` | none | L | Median shape moments; rarely filtered at image level. |
| `MAG_95Q` | PROJ | L | Depth-ish proxy. |
| `PH_ZP` | PROJ | L | Zero point; calibration monitoring. |
| `PH_COL1`,`PH_MEDC` | none | L | Color terms; not filters. |
| `PH_RMS` | PROJ | M | Photometric-quality cut. |
| `PH_NSRC` | minmax | L | Count. |
| `PH_MAGSY`,`PH_MAGT`,`PH_MAGTE` | set / none | L | Low-card descriptors of the calibration. |
| `LIMMAG` | PROJ | H | Depth cut — a very common "give me images deeper than X" filter; semi-random float → projection. |
| `BACKMAG` | PROJ | M | Sky-brightness/moon-condition cut. |
| `APCOR_A%d`,`APCOR_PS` | none | L | Aperture-correction coefficients; not filters. |
| `APCOR_N` | none | L | Count. |
| `NCOADD` | minmax / set | M | Coadd-depth filter; small integer, effectively discrete. |
| `COADDOP` | set | L | Low-card method. |
| `AVNCOADD`,`MINCOADD` | none | L | Coverage stats; rarely filtered. |

### Moving object & photometric transmission

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

# Catalog (sources / detections) table

Sorted by `ORDER BY (UPIX_PAR, UPIX_LOW, UPIX_HIG)`. This is the huge table, so unlike the header, storage and write cost dominate — every projection here is paid billions of times. The spatial PK handles position for free; the only projections worth building are on the handful of columns science queries actually cut on (magnitude, S/N, quality), and bitmasks/booleans get special handling.

| Column | Strategy | Prio | Why & considerations |
|---|---|---|---|
| `XPEAK`,`YPEAK`,`X1`,`Y1` | none | L | Detection/first-moment pixel positions; almost never a query predicate. Position filtering is done in sky coordinates via the spatial PK, not pixels. |
| `X2`,`Y2` | PROJ | L | Second moments enable a star/galaxy (extendedness) cut; project only if you do morphology selection. |
| `XY` | none | L | Cross-moment; rarely filtered alone. |
| `SN_1`,`SN_2` | PROJ | M | Matched-filter S/N for delta and PSF hypotheses; used in detection-quality cuts. **Consider:** `SN_2 − SN_1` is the pipeline's hot-pixel discriminator, and `SN_3/SN_2` encodes extendedness — richer than any single value. |
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
| `FLAGS` | **bit** | H | 32-bit mask: no index prunes `FLAGS & mask`. Decode the hot bits (saturation, edge, CR/bad-pixel) into separate `UInt8`/`Bool` columns and index those; otherwise rely on `PREWHERE`, which is cheap on a `UInt32`. |
| `X`,`Y`,`XFULL`,`YFULL` | none | L | PSF-fit pixel positions; not predicates (sky coords + PK cover position). |
| `MergedCatMask` | **bit** | M | Same bitmask logic as `FLAGS`. Materialize the useful membership bits ("has GAIA/QSO/CV match") as booleans and index those. |
| `DistMP` | PROJ | M | Mostly NaN (only populated within 10″ of a minor planet), so `DistMP < x` selects a tiny subset — **high selectivity**, which makes a projection genuinely effective. **Consider** a boolean `near_mp` bit instead, since the exact distance is rarely the cut. Keep it a flag, not a quality penalty. |
| `AIRMASS` | minmax / PROJ | L | Constant within an image but **scattered once the table is sorted by HEALPix**, so `minmax` won't prune here; PROJ only if queried. **Consider** not storing it per source at all — it's derivable by joining to the header on the epoch id, saving space over billions of rows. |
| `UPIX_PAR` | **PK** | H | Coarse HEALPix — the leading spatial sort key; drives cone search and crossmatch locality. |
| `UPIX_LOW` | **PK** | H | Mid HEALPix — second PK level; **consider** leading with this resolution so granules aren't over-fragmented while still giving cone-search selectivity. |
| `UPIX_HIG` | **PK** | H | Fine HEALPix — finest PK level for tight positional pruning. |
| `AB_ZP` | none | L | Per-image zero point; constant within an image, scattered here. Like `AIRMASS`, prefer joining from the header over storing per source. |
| `FORCED` | set / PROJ | M | Boolean but very skewed (forced points are a minority). A `set(2)` skip index isolates the forced subset; **consider** a dedicated projection or even a separate table/partition for forced photometry, since forced points have different provenance and are often queried on their own. |

---

## Cross-cutting things to consider

- **Build in priority order, measure, then extend.** Ship the H set (spatial PK, `MAG_PSF`/`SN` projections, decoded `FLAGS` bits on the catalog; `MOUNTNUM/CAMNUM/JD` PK, `FWHM`/`LIMMAG`/`IMTYPE`/`CROPID`/`FIELDID` on the header), then let real query logs promote M columns. Don't pre-build L.
- **Combine correlated projections.** On the catalog, `SN`, `MAG_PSF`, `MAGERR_PSF`, `PSF_CHI2DOF` are usually filtered together as one "clean detection" predicate. A single projection ordered by the primary cut (or a small composite) can serve the joint filter for a fraction of the write cost of four separate ones.
- **Don't store per-source what you can join.** `AIRMASS`, `AB_ZP` (and arguably parent-image `FWHM`, `LIMMAG`, `PT_SUCC`) are per-image constants; joining them from the header keeps the billion-row table narrower and sidesteps indexing them at all.
- **Bitmasks are a modelling choice, not an index choice.** The value of `FLAGS`/`MergedCatMask` comes from decoding the few hot bits into typed columns up front; that decision belongs in the schema, made once, not in the query layer.
- **HEALPix NSide selection is the highest-leverage knob on the catalog.** Too coarse and cone searches scan whole regions; too fine and granules fragment and compression suffers. Tune the leading `UPIX` resolution to your typical search radius.
- **Verify survey-specific collapses.** If LAST runs effectively single-filter, `FILTER` becomes a constant (drop its index); if this table is science-only, `IMTYPE` does the same. These checks remove indexes that would never prune.
