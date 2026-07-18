# LAST / ULTRASAT — ClickHouse column behavioral types & index strategy

Scope: how the columns of the LAST **image/header table** and the **source catalog / light-curve tables** behave statistically, and — given "I want to index most columns to help queries" — what index strategy each column should get in ClickHouse.

Two separate concerns are covered:

1. **Column behavioral type** — how the values are distributed and how they change row-to-row (e.g. "magnitude is semi-random", "mount/telescope is low cardinality"). This drives the ClickHouse data type + compression codec.
2. **Index strategy** — what to do when a query filters (`WHERE`) on that column: primary sort key, lightweight projection, skip index, or nothing.

Ordering of the per-column tables follows the original wiki column order.

---

## Part A — Behavioral column classes

Each column is assigned to one of these classes. The class implies a type + codec, and biases the index choice.

| # | Class | Description | ClickHouse type + codec |
|---|-------|-------------|--------------------------|
| 1 | Categorical – low cardinality | Few distinct values, heavily repeated (mount, camera, filter, level) | `LowCardinality(String)` / `Enum8` / small `UInt` |
| 2 | Instrument constant / near-constant | Same value across ~all rows in a table (structural FITS keywords, site constants) | plain type, `ZSTD`; compresses to almost nothing |
| 3 | Time / monotonic | Sorted or slowly increasing (JD, dates, counters) | `DateTime64` / `Float64` + `Delta` / `DoubleDelta` |
| 4 | Bounded semi-random (angular / positional) | Random within a fixed range (RA 0–360, Dec ±90, pixel coords) | `Float32/64` + `Gorilla` |
| 5 | Unbounded semi-random measurement floats | High entropy, poor compression (magnitudes, fluxes, background, RMS, FWHM) | `Float32/64` + `ZSTD` |
| 6 | Counts / small non-negative integers | Source counts, coadd counts | `UInt8/16/32` |
| 7 | Boolean flags | true/false | `UInt8` / `Bool` |
| 8 | Bitmasks | Integer, bit-packed; never LowCardinality | `UInt32` / `UInt64` |
| 9 | High-cardinality identifiers / hashes / filenames | Near-unique, random | `String` or `UInt64`, **no** LowCardinality |
| 10 | Spatial index (HEALPix) | Integer, ideal sort/partition key | `UInt64` |

---

## Part B — Header (images) table

One row per image. Small table relative to the catalog; index cost is low, but most columns are constant so indexing them is pointless. Queries typically filter by time, pointing/field, mount/camera, filter, data level/crop, and image-quality metrics.

### B.1 Behavioral classification of columns

**Class 2 — Instrument constant / near-constant:** `SIMPLE`, `BITPIX`, `NAXIS`, `NAXIS1`, `NAXIS2`, `EXTEND`, `LONGSTRN`, `OBSLON`, `OBSLAT`, `OBSALT`, `EQUINOX`, `GAIN`, `READNOI`, `DARKCUR`, `SATURVAL`, `NONLIN`, `BINX`, `BINY`, `BZERO`, `BSCALE`, `WCSAXES`, `LONPOLE`, `LATPOLE`, `RADESYS`, `CTYPE1`, `CTYPE2`, `CUNIT1`, `CUNIT2`

**Class 1 — Categorical / low cardinality:** `PROJNAME`, `FULLPROJ`, `NODENUMB`, `TIMEZONE`, `MOUNTNUM`, `CAMNUM`, `CAMNAME`, `CAMMODE`, `CAMGAIN`, `CAMOFFS`, `EXPMODE`, `FILTER`, `IMTYPE`, `EXPTIME`, `MEXPTIME`, `COADDOP`, `LEVEL`, `SUBLEVEL`, `VERSION`, `PIPEVER`, `GITASTRP`, `GIT_CAMR`, `GITFOCUS`, `CCDID`, `CROPID`, `FIELDID`, `SUBDIR`, `DIRDAY`, `DIRMON`, `DIRYEAR`, `PH_MAGSY`, `PH_MAGT`, `PH_MAGTE`, `PT_AREF`, `PT_SPEC`, `PT_%d_N`, `LIGHTSEC`, `OVERSCAN`, `CCDSEC`, `ORIGSEC`, `ORIGUSEC`, `UNIQSEC`

**Class 3 — Time / monotonic:** `CRDATE`, `JD_START`, `JD`, `MIDJD`, `MINJD`, `MAXJD`, `DATE-OBS`, `TIME`, `LST`, `COUNTER`

**Class 4 — Bounded semi-random (angular / positional):** `RA`, `DEC`, `M_RA`, `M_HA`, `M_DEC`, `M_AZ`, `M_ALT`, `M_JRA`, `M_JDEC`, `M_ARA`, `M_AHA`, `M_ADEC`, `M_ADRA`, `M_ADHA`, `M_ADDEC`, `AIRMASS`, `TRK_RA`, `TRK_DEC`, `CRPIX1`, `CRPIX2`, `CRVAL1`, `CRVAL2`, `CD%d_%d`, `PV%d_%d`, `PIXSCALE`, `ROTAT`, `RA%d`, `DEC%d`, `RAU%d`, `DECU%d`

**Class 5 — Unbounded semi-random measurement floats:** `MNTTEMP`, `CAMTEMP`, `CAMCOOL`, `FOCUS`, `PRVFOCUS`, `ORIGGAIN`, `MEANBCK`, `MEDBCK`, `STDBCK`, `MINBCK`, `MAXBCK`, `MEANVAR`, `MEDVAR`, `M_CHI2D`, `RP_MRMS`, `RP_MMRMS`, `AST_ARMS`, `AST_ERRM`, `FWHE`, `FWHM`, `PSF_FITN`, `PSF_FITA`, `PSF_FITB`, `PSF_FITT`, `PSF_PKR`, `PSF_DPK`, `PSF_ERR`, `PSF_S2`, `PSF_AF_%d`, `MED_X2`, `MED_Y2`, `MED_XY`, `MAG_95Q`, `PH_ZP`, `PH_COL1`, `PH_MEDC`, `PH_RMS`, `LIMMAG`, `BACKMAG`, `APCOR_A%d`, `APCOR_PS`, `GM_RATEX`, `GM_STDX`, `GM_RATEY`, `GM_STDY`, `PT_RMS`, `PT_ARMS`, `PT_CHI2`, `PT_%d_V%d`

**Class 6 — Counts / small integers:** `N_STARS`, `AST_NSRC`, `PSF_NST`, `PSF_NPK`, `PH_NSRC`, `APCOR_N`, `NCOADD`, `AVNCOADD`, `MINCOADD`, `PT_DOF`, `PT_NCALI`

**Class 7 — Booleans:** `PT_SUCC`, `PT_%d_F%d` (and `SIMPLE` / `EXTEND` if typed bool rather than constant)

**Class 9 — High-cardinality identifiers / hashes:** `FILENAME`, `CHECKSUM`, `DATASUM`, `ID_RAW`, `ID_DARK`, `ID_FLAT`, `ID_PROC`, `ID_COADD`, `ID_PROCF`, `ID_PROCL`, `BIAS_IM`, `FLAT_IM`, `OBJECT`

**Class 10 — Spatial index (HEALPix):** `UPIX_PAR`, `UPIX_LOW`, `UPIX_HIG`

### B.2 Recommended base sort key

```
ORDER BY (MOUNTNUM, CAMNUM, JD)
```

Per-telescope time scans are the common access pattern; spatial and field access are handled by projections. If instead the dominant query is "all images of field X", flip to `ORDER BY (FIELDID, JD)` and project the time/telescope columns.

### B.3 Per-column index strategy (wiki order)

Strategy codes and priority are defined in Part F.

| Column | Strategy | Prio | Note |
|---|---|---|---|
| `SIMPLE` | none | — | constant |
| `BITPIX` | none | — | constant |
| `NAXIS` | none | — | constant |
| `NAXIS1` | none | — | constant |
| `NAXIS2` | none | — | constant |
| `EXTEND` | none | — | constant |
| `LONGSTRN` | none | — | constant |
| `CHECKSUM` | bloom | L | only if you look up by checksum |
| `DATASUM` | bloom | L | as above |
| `FILENAME` | bloom | M | exact lookup; `tokenbf` for substring |
| `CRDATE` | minmax | M | time-correlated |
| `PROJNAME` | set | M | low card |
| `FULLPROJ` | set / PROJ | M | per-system filter |
| `NODENUMB` | set | M | low card |
| `TIMEZONE` | none | L | near-constant |
| `MOUNTNUM` | PK | H | sort-key component |
| `GITASTRP` | set | L | low-card version |
| `GIT_CAMR` | set | L | low-card version |
| `GITFOCUS` | set | L | low-card version |
| `PIPEVER` | set | M | filter by pipeline version |
| `OBSLON` | none | — | constant |
| `OBSLAT` | none | — | constant |
| `OBSALT` | none | — | constant |
| `MNTTEMP` | PROJ | L | env metric, rarely filtered |
| `JD_START` | minmax | M | time-correlated |
| `JD` | PK | H | sort-key component (time) |
| `MIDJD` | minmax | M | time-correlated |
| `MINJD` | minmax | L | visit coadd only |
| `MAXJD` | minmax | L | visit coadd only |
| `DATE-OBS` | minmax | M | redundant with JD |
| `TIME` | none | L | string form of time |
| `LST` | PROJ | L | 0–360, weakly time-correlated |
| `RA` | PROJ | H | field-center cone search (spatial) |
| `DEC` | PROJ | H | field-center cone search |
| `EQUINOX` | none | — | constant |
| `M_RA` | none | L | engineering |
| `M_HA` | none | L | engineering |
| `M_DEC` | none | L | engineering |
| `M_AZ` | none | L | engineering |
| `M_ALT` | PROJ | L | if you cut on elevation |
| `M_JRA` | none | L | engineering |
| `M_JDEC` | none | L | engineering |
| `M_ARA` | none | L | engineering |
| `M_AHA` | none | L | engineering |
| `M_ADEC` | none | L | engineering |
| `M_ADRA` | none | L | engineering |
| `M_ADHA` | none | L | engineering |
| `M_ADDEC` | none | L | engineering |
| `AIRMASS` | PROJ | M | airmass-limited samples |
| `TRK_RA` | none | — | engineering |
| `TRK_DEC` | none | — | engineering |
| `FOCUS` | none | L | mechanical |
| `PRVFOCUS` | none | L | mechanical |
| `IMTYPE` | set | H | science/dark/flat — hot filter |
| `OBJECT` | PROJ / bloom | H | per-target queries |
| `FILTER` | set | M | low card |
| `COUNTER` | minmax | L | semi-monotonic |
| `EXPTIME` | set | M | discrete values |
| `MEXPTIME` | set | L | discrete |
| `BZERO` | none | — | constant |
| `BSCALE` | none | — | constant |
| `EXPMODE` | set | L | low card |
| `GAIN` | none | — | near-constant |
| `ORIGGAIN` | none | L | near-constant |
| `READNOI` | none | — | near-constant |
| `DARKCUR` | none | L | near-constant |
| `SATURVAL` | none | — | near-constant |
| `NONLIN` | none | — | near-constant |
| `BINX` | none | — | constant |
| `BINY` | none | — | constant |
| `CAMNUM` | PK | H | sort-key component |
| `CAMNAME` | set | L | redundant w/ CAMNUM |
| `CAMTEMP` | PROJ | L | env metric |
| `CAMCOOL` | none | L | env metric |
| `CAMMODE` | set | L | low card |
| `CAMGAIN` | set | L | low card |
| `CAMOFFS` | set | L | low card |
| `LEVEL` | set | M | data-product level filter |
| `SUBLEVEL` | set | M | sub-level filter |
| `VERSION` | set | M | product version |
| `ID_RAW` | bloom | H | exact lookup / join key |
| `ID_DARK` | bloom | M | join key |
| `ID_FLAT` | bloom | M | join key |
| `ID_PROC` | bloom | H | join key |
| `ID_COADD` | bloom | M | join key (visit) |
| `ID_PROCF` | bloom | L | join key (visit) |
| `ID_PROCL` | bloom | L | join key (visit) |
| `CCDID` | set | L | low card |
| `SUBDIR` | bloom / PROJ | M | per-night grouping |
| `FIELDID` | PK-candidate / PROJ | H | per-field light curves — very hot |
| `DIRDAY` | set | M | date part |
| `DIRMON` | set | M | date part |
| `DIRYEAR` | set | M | date part |
| `CROPID` | set | H | per-crop (subimage) — hot filter |
| `LIGHTSEC` | none | L | low-card string |
| `OVERSCAN` | none | L | low-card string |
| `CCDSEC` | none | L | low-card string |
| `ORIGSEC` | none | L | low-card string |
| `ORIGUSEC` | none | L | low-card string |
| `UNIQSEC` | none | L | low-card string |
| `BIAS_IM` | bloom | M | which calib was used |
| `FLAT_IM` | bloom | M | which calib was used |
| `MEANBCK` | PROJ | L | quality cut |
| `MEDBCK` | PROJ | L | quality cut |
| `STDBCK` | PROJ | L | quality cut |
| `MINBCK` | none | L | |
| `MAXBCK` | none | L | |
| `MEANVAR` | none | L | |
| `MEDVAR` | none | L | |
| `N_STARS` | minmax / PROJ | M | density / empty-image cut |
| `M_CHI2D` | PROJ | L | quality cut |
| `RP_MRMS` | PROJ | L | photometric quality |
| `RP_MMRMS` | none | L | |
| `AST_NSRC` | minmax | L | count |
| `AST_ARMS` | PROJ | M | astrometric-quality cut |
| `AST_ERRM` | PROJ | L | |
| `CTYPE1` | none | — | constant |
| `CTYPE2` | none | — | constant |
| `WCSAXES` | none | — | constant |
| `RADESYS` | none | — | constant |
| `LONPOLE` | none | — | constant |
| `LATPOLE` | none | — | constant |
| `CUNIT1` | none | — | constant |
| `CUNIT2` | none | — | constant |
| `CRPIX1` | none | — | near-constant |
| `CRPIX2` | none | — | near-constant |
| `CRVAL1` | PROJ | M | = center RA (spatial) |
| `CRVAL2` | PROJ | M | = center Dec (spatial) |
| `CD%d_%d` | none | — | WCS matrix |
| `PIXSCALE` | none | L | near-constant |
| `ROTAT` | PROJ | L | if you cut on orientation |
| `PV%d_%d` | none | — | distortion coeffs |
| `RA%d` | none | L | corners; covered by center |
| `DEC%d` | none | L | corners |
| `RAU%d` | none | L | corners |
| `DECU%d` | none | L | corners |
| `FWHE` | PROJ | H | seeing / image-quality cut |
| `FWHM` | PROJ | H | seeing / image-quality cut |
| `PSF_FITN` | none | L | |
| `PSF_FITA` | PROJ | L | PSF-quality |
| `PSF_FITB` | PROJ | L | PSF-quality |
| `PSF_FITT` | none | L | |
| `PSF_NST` | minmax | L | count |
| `PSF_NPK` | set | L | small int |
| `PSF_PKR` | PROJ | L | quality (NaN-heavy) |
| `PSF_DPK` | none | L | NaN-heavy |
| `PSF_ERR` | PROJ | L | quality |
| `PSF_S2` | none | L | |
| `PSF_AF_%d` | none | L | |
| `UPIX_PAR` | PK-candidate / PROJ | H | spatial index for field center |
| `UPIX_LOW` | PROJ | M | spatial (finer) |
| `UPIX_HIG` | PROJ | M | spatial (finest) |
| `MED_X2` | none | L | |
| `MED_Y2` | none | L | |
| `MED_XY` | none | L | |
| `MAG_95Q` | PROJ | L | depth-ish |
| `PH_ZP` | PROJ | L | calibration |
| `PH_COL1` | none | L | |
| `PH_MEDC` | none | L | |
| `PH_RMS` | PROJ | M | photometric-quality cut |
| `PH_NSRC` | minmax | L | count |
| `PH_MAGSY` | none | L | low card |
| `LIMMAG` | PROJ | H | depth cut — hot filter |
| `BACKMAG` | PROJ | M | sky-brightness cut |
| `PH_MAGT` | set | L | low card |
| `PH_MAGTE` | set | L | low card |
| `APCOR_A%d` | none | L | |
| `APCOR_PS` | none | L | |
| `APCOR_N` | none | L | count |
| `NCOADD` | minmax / set | M | coadd-depth filter |
| `COADDOP` | set | L | low card |
| `AVNCOADD` | none | L | |
| `MINCOADD` | none | L | |
| `GM_RATEX` | none | L | |
| `GM_STDX` | none | L | |
| `GM_RATEY` | none | L | |
| `GM_STDY` | none | L | |
| `PT_RMS` | PROJ | M | transmission-fit quality |
| `PT_ARMS` | PROJ | L | |
| `PT_CHI2` | PROJ | L | |
| `PT_DOF` | none | L | |
| `PT_NCALI` | minmax | L | count |
| `PT_SUCC` | set | M | boolean — filter good fits |
| `PT_AREF` | none | L | low card |
| `PT_SPEC` | none | L | low card |
| `PT_%d_N` | none | L | low card |
| `PT_%d_V%d` | none | L | fit params |
| `PT_%d_F%d` | none | L | fixed/fitted flags |

---

## Part C — Catalog (sources / detections) table

One row per source detection per image. This is the huge table, so index cost is real and the payoff concentrates in **spatial primary key + a few projections** on the columns you actually filter (magnitude, S/N, flags, forced).

### C.1 Behavioral classification of columns

**Class 4 — Bounded semi-random (pixel / angular positions):** `XPEAK`, `YPEAK`, `X1`, `Y1`, `X`, `Y`, `XFULL`, `YFULL`, `RA`, `Dec` (RA/Dec want `Float64` for astrometric precision; pixel coords fit `Float32`/`UInt16`)

**Class 5 — Unbounded semi-random measurement floats (the bulk):** `X2`, `Y2`, `XY`, `SN_1`, `SN_2`, `SN_3`, `SN`, `BACK_IM`, `VAR_IM`, `BACK_ANNULUS`, `STD_ANNULUS`, `FLUX_APER_1/2/3`, `FLUXERR_APER_1/2/3`, `MAG_APER_1/2/3`, `MAGERR_APER_1/2/3`, `FLUX_XYPEAK`, `FLUX_PSF`, `MAG_PSF`, `MAGERR_PSF`, `PSF_CHI2DOF`, `DistMP` — this is the "magnitude is semi-random" bucket; `Float32` + `ZSTD`; `DistMP` is NaN-heavy / `Nullable`

**Near-constant within an image (varies image-to-image):** `AIRMASS`, `AB_ZP` — behaviorally special: sorted/grouped by image they compress like constants, but scattered once the table is sorted by HEALPix

**Class 6 — Counts / small integers:** `MITER`

**Class 7 — Booleans:** `FORCED`

**Class 8 — Bitmasks:** `FLAGS` (32-bit → `UInt32`), `MergedCatMask` (→ `UInt32`/`UInt64`)

**Class 10 — Spatial index (HEALPix):** `UPIX_PAR`, `UPIX_LOW`, `UPIX_HIG`

### C.2 Recommended base sort key

```
ORDER BY (UPIX_PAR, UPIX_LOW, UPIX_HIG)
```

Spatial locality drives cone-search / crossmatch, which is the dominant access pattern; everything else is a projection or skip index.

### C.3 Per-column index strategy (wiki order)

| Column | Strategy | Prio | Note |
|---|---|---|---|
| `XPEAK` | none | L | pixel pos, rarely a predicate |
| `YPEAK` | none | L | |
| `X1` | none | L | |
| `Y1` | none | L | |
| `X2` | PROJ | L | shape cut (star/galaxy) |
| `Y2` | PROJ | L | shape cut |
| `XY` | none | L | |
| `SN_1` | PROJ | M | S/N cut |
| `SN_2` | PROJ | M | S/N cut |
| `SN_3` | PROJ | L | |
| `BACK_IM` | PROJ | L | quality cut |
| `VAR_IM` | PROJ | L | quality cut (bogus-source test) |
| `BACK_ANNULUS` | none | L | |
| `STD_ANNULUS` | PROJ | L | pairs with VAR_IM in artifact test |
| `FLUX_APER_1` | none | L | prefer magnitudes |
| `FLUX_APER_2` | none | L | |
| `FLUX_APER_3` | none | L | |
| `FLUXERR_APER_1` | none | L | |
| `FLUXERR_APER_2` | none | L | |
| `FLUXERR_APER_3` | none | L | |
| `MAG_APER_1` | PROJ | M | magnitude cut |
| `MAG_APER_2` | PROJ | M | magnitude cut |
| `MAG_APER_3` | PROJ | H | main aperture mag — hot cut |
| `MAGERR_APER_1` | none | L | |
| `MAGERR_APER_2` | none | L | |
| `MAGERR_APER_3` | PROJ | L | S/N-equivalent cut |
| `FLUX_XYPEAK` | none | L | |
| `FLAGS` | bit | H | materialize hot bits as bool cols + index those |
| `X` | none | L | PSF pos, rarely a predicate |
| `Y` | none | L | |
| `XFULL` | none | L | |
| `YFULL` | none | L | |
| `FLUX_PSF` | none | L | prefer MAG_PSF |
| `MAG_PSF` | PROJ | H | primary magnitude — hottest float cut |
| `MAGERR_PSF` | PROJ | M | pairs with S/N cuts |
| `PSF_CHI2DOF` | PROJ | M | star/artifact quality cut |
| `SN` | PROJ | H | PSF S/N — very hot cut |
| `MITER` | set | L | small int |
| `RA` | PK-served / PROJ | H | covered by UPIX PK; PROJ on Dec-band if needed |
| `Dec` | PK-served / PROJ | H | as above |
| `MergedCatMask` | bit | M | materialize "has match" / per-catalog bools |
| `DistMP` | PROJ | M | minor-planet association (NaN-heavy) |
| `AIRMASS` | minmax / PROJ | L | per-image constant; minmax only if sorted by image |
| `UPIX_PAR` | PK | H | spatial sort key (coarse) |
| `UPIX_LOW` | PK | H | spatial sort key (mid) |
| `UPIX_HIG` | PK | H | spatial sort key (fine) |
| `AB_ZP` | none | L | per-image constant |
| `FORCED` | set / PROJ | M | boolean, very skewed — isolate forced sources |

---

## Part D — Light-curve / sources (measurements) table

"Light-curve per source" is stored differently on two timescales:

- **Within-visit (~20 points):** materialized as the `MatchedSources` product (epochs × sources matrix, HDF5 per field/sub-image/visit). One source's short light curve is one column of that matrix.
- **Long-term (many nights):** generally **not** stored per source; assembled on demand as a positional (cone-search) query across epoch catalogs. LAST source identity is fundamentally positional (HEALPix), not a persistent per-detection `source_id`, unless you manufacture one from a merged/coadd catalog and backfill it.

For a ClickHouse light-curve store, use a **long / narrow measurements table** (one row per source × epoch) — best for append-heavy ingestion and cross-epoch filtering. Consider array-per-source (`Array(Float32)` mag, `Array(Float64)` jd) only for a periodically rebuilt object product.

### D.1 Recommended sort key

```
ORDER BY (UPIX_LOW, source_id, MIDJD)
```

A source's position is fixed, so its HEALPix is stable: this clusters all rows of a source contiguously and time-ordered, turning "light curve per source" into a single contiguous range scan. If the dominant query is region+time rather than single-source, use `(UPIX_LOW, MIDJD, source_id)`.

### D.2 Per-column-group index strategy

| Column group | Example cols | Strategy | Prio | Note |
|---|---|---|---|---|
| Persistent source ID | `source_id` / merged-cat id | PK + bloom | H | PK gives locality; bloom for `IN (…)` lookups |
| Spatial index | `UPIX_PAR/LOW/HIG` | PK (`LOW`) / minmax | H | leading PK; others minmax are cheap |
| Per-detection position | `RA`, `Dec`, `X`, `Y` | PK-served / PROJ | M | covered by spatial PK; PROJ on `Dec` for dec-bands |
| Time | `MIDJD` (+`JD`,`MINJD`,`MAXJD`) | PK tail + minmax | H | `DoubleDelta` codec; minmax for time-range |
| Magnitudes | `MAG_PSF`, `MAG_APER_3` | PROJ | H | variability/threshold cuts (semi-random) |
| Fluxes | `FLUX_PSF`, `FLUX_APER_*` | none / PROJ | L | prefer magnitudes |
| Mag errors | `MAGERR_PSF`, `MAGERR_APER_*` | PROJ | M | weighting / quality cuts |
| S/N | `SN`, `SN_1..3` | PROJ | M | detection-quality cut |
| Quality | `PSF_CHI2DOF`, `BACK_IM`, `VAR_IM`, `STD_ANNULUS` | PROJ | M | artifact rejection |
| Bitmask flags | `FLAGS` | bit | H | materialize hot bits as bool cols + index those |
| Catalog-match mask | `MergedCatMask` | bit | M | materialize "has GAIA/QSO/…" booleans |
| Forced flag | `FORCED` | set(2) / PROJ | M | very skewed |
| Minor-planet dist | `DistMP` | PROJ | L | NaN-heavy |
| Per-image constants | `AIRMASS`, `AB_ZP` | none / PROJ | L | constant within an epoch, scattered here |
| Epoch link | `ID_PROC` / epoch key | bloom | M | join back to header/epoch table |

### D.3 Variability queries → separate summary table

Queries like "sources with RMS > x" or "χ² > y" should hit a **`sources` (objects) summary table**, one row per source, maintained incrementally with an `AggregatingMergeTree` + materialized view over the measurements table (running mean mag, `stddevPop`, χ² vs constant, N epochs, min/max JD, proper motion). Index its summary columns with projections and keep the spatial PK. This turns a variability search from a full light-curve scan into a single-row-per-source lookup.

---

## Part E — Cross-cutting rules that matter most

- **Projections vs skip indices (ClickHouse ≥ 25.6, cleaner syntax in 26.1).** Lightweight `_part_offset` projections act as true secondary indexes with granule-level pruning, and multiple of them combine on a multi-filter query, without duplicating row data. For "index most columns", the strategy is: one good sort key + a spread of lightweight projection-indexes on the value columns, and reserve `minmax`/`set`/`bloom_filter` for their narrow sweet spots.
- **"Magnitude is semi-random" → projection, never skip index.** `minmax`/`bloom` are useless on `MAG_PSF < 18` because values aren't ordered on disk and the range is wide. A lightweight projection ordered by the magnitude gives real granule pruning and combines with the spatial PK on the same query.
- **Bitmasks (`FLAGS`, `MergedCatMask`).** No index prunes `col & mask <> 0`. Materialize the handful of bits you filter on as separate `UInt8`/`Bool` columns (e.g. `flag_saturated`, `has_gaia_match`) and index those with `set(2)` or a projection — or lean on `PREWHERE`, which is cheap on a `UInt32`.
- **Per-image-constant catalog columns (`AIRMASS`, `AB_ZP`).** Constant within an image but scattered once the table is sorted by HEALPix, so `minmax` won't help; index via projection only, and only if queried.
- **Templated keys** (`CD%d_%d`, `PV%d_%d`, `PT_%d_V%d`, `PSF_AF_%d`, `RA%d`, …) expand into many concrete columns of the same behavioral class; they inherit that class's type and index recommendation.
- **PK-candidate tension (header table).** `MOUNTNUM/CAMNUM/JD` vs `FIELDID`/`UPIX` all want the sort key but you get one tuple. Default: time/telescope tuple as PK, spatial/field as projections; flip if "all images of field X" dominates.

---

## Part F — Legend: index strategy codes & priority

| Code | Meaning |
|---|---|
| **PK** | Part of the base table's `ORDER BY` (primary sort key). One tuple per table; strongest and free. |
| **PROJ** | Lightweight `_part_offset` projection index (`PROJECTION p_col INDEX col TYPE basic`, i.e. `SELECT _part_offset ORDER BY col`). For frequently-filtered value columns not covered by the PK — especially range filters on semi-random values. Combinable across filters. |
| **minmax** | `INDEX … TYPE minmax` skip index. Helps only when values correlate with on-disk order (time, counters, per-part-constant columns). Almost free. |
| **set** | `INDEX … TYPE set(N)` skip index. Low-cardinality equality / `IN`. |
| **bloom** | `INDEX … TYPE bloom_filter` (or `tokenbf_v1` / `ngrambf_v1` for substrings). High-cardinality exact-match / `IN` (IDs, filenames, hashes). |
| **bit** | Bitmask: standard indexes can't prune bit-tests. Materialize hot bits as boolean columns and index/PK those; otherwise rely on `PREWHERE`. |
| **none** | Don't index (constant, or semi-random and rarely a predicate). Adds write cost + storage for ~zero pruning. |

**Priority** — how worthwhile: **H** = index it; **M** = index if it appears in your WHERE workload; **L / —** = skip unless a specific query needs it.
