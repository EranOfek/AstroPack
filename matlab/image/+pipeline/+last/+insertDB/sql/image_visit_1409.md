We want a new ClickHouse table, `image_visit`, that keeps the header of every LAST v1 coadd image (one row per coadd sub-image, i.e. 24 rows per visit), so image quality, calibration and pointing can be queried without opening FITS files. Below is a first proposal: for every header keyword (except HISTORY and COMMENT), whether to keep it as a column, its type, default value, the storage resolution it needs, and how many distinct values to expect. Please comment on the ✅/❌ choices.

**Related:** #1411 proposes the source tables `src_visit` and `src_proc`, and #1413 the epoch image table `image_proc`, in the same format. Once this proposal is settled, it will be used to create the actual ClickHouse table `image_visit`.

### What the proposal is based on

- **Example file:** `LAST.01.02.01_20260406.004254.291_clear_1214_000_001_005_sci_coadd_Image_1.fits` (last02e, pipeline `lv1.0.4`).
- **Measured on:** all 1224 coadd headers of the night 2026-04-05 on cameras LAST.01.02.01 and LAST.01.03.01 (51 visits). Two west-side coadds (cameras 03 and 04) and a named-target coadd (`21109WD`) have the same keyword set.
- **Added on 2026-10-19:** `MAXBCKD`, the largest difference of the background level (`MEDBCK`) between the images of the visit (#1355). It was added to the coadd header after the sample was taken, so older coadds do not have it and get the default `nan`.
- **Added on 2026-10-21:** four keywords that are in the headers since `lv1.1.2` (checked on the products of the night 2026-10-20 on euclid). `SCHEDULR` (the scheduler that gave the target, e.g. `last.survey`, LAST_OCS #132) and `QUICFWHM` (the camera software's quick FWHM estimate in the centre of the raw image, in pixels) come from the raw image header. `PT_FNORM` and `PT_NORMU` come from the absolute calibration (#1431): `PT_NORMU = 'm2'` says that the normalisation `PT_1_V1` is now an effective collecting area in m² (about 0.019), and `PT_FNORM` is that area divided by the telescope area π·0.1397² m² (about 0.31). In older headers (no `PT_NORMU`) `PT_1_V1` is already relative to the telescope area (about 0.35), so it is comparable to `PT_FNORM`, not to the new `PT_1_V1`. Older products do not have the four keywords and get the defaults.
- **Columns of the tables below:**
  - **Include:** ✅ keep as a column, ❌ leave out.
  - **Data type:** the proposed ClickHouse type.
  - **Default:** the value written when the keyword is missing or blank. NaN for real numbers, so that it means the same as NaN in the header; 0 for counts and IDs; an empty string for text.
  - **Resolution:** the smallest step in the value worth keeping (1 for whole numbers). It is not the ClickHouse index granularity. It decides between 4-byte (`Float32`, about 7 significant digits) and 8-byte (`Float64`) numbers.
  - **Cardinality:** how many different values to expect across the whole table. The number in brackets is the count of distinct values in the 1224-header sample.
  - **In 2024 sheet:** whether the existing design sheet (`Design-Database-Pipeline-ClickHouse.xlsx`, tab *Images*, `visit_images` column, updated 18/11/2024) already has this column, and with which name and type.
- **Rule used for ❌:** a keyword is left out when it is fixed by the FITS format or the WCS convention (`SIMPLE`, `NAXIS*`, `CTYPE*`, `CUNIT*`, …), fixed by definition for a coadd (`LEVEL`, `IMTYPE`, `SUBLEVEL`), constant for the site (`TIMEZONE`, `EQUINOX`, `SPATNBIN`), or an exact copy or simple function of another kept keyword: `FULLPROJ` repeats `PROJNAME`, `ID_PROC` repeats `ID_PROCF`, `CCDID` is always 1, and `PH_MAGTE` follows from `PH_MAGT`. Keywords that are constant in the sample but can change between pipeline versions or hardware (catalogue names, methods, `NODENUMB`) are kept: a constant column costs almost nothing to store.
- **Column names:** the keyword in lower case, with `-` replaced by `_` (`DATE-OBS` becomes `date_obs`). This matches the style of the existing `last.diff_src` table. IDs and the `UPIX_*` values become `UInt64`. In `diff_src` the IDs are `Int64`, so a join between the two tables on an ID needs a cast. Each image-section keyword (`*SEC` and `OVERSCAN`, header text `'[xmin xmax ymin ymax]'`) becomes four `Int16` columns, `<keyword>_xmin`, `_xmax`, `_ymin` and `_ymax`, as in the existing `last.visit_images` table on euclid.
- **Size:** about 25 visits per camera per night × 24 sub-images × 44 cameras gives roughly 25,000 rows per clear night, or about 8 million rows per year. With about 350 columns (325 keywords, the eight image-section keywords taking four columns each) that is a few GB per year after compression, which is small for ClickHouse.

### Sorting and indexes

The table is sorted by `id_coadd` (`ORDER BY id_coadd`). The observation time sits in the highest bits of the ID, so this order is close to time order. Additional indexes will be defined by @ChenTishler.

### Keyword table

Click a group to open it.

<details><summary><b>FITS structure</b> (0 of 9 kept)</summary>

| Keyword | Include | Data type | Default | Resolution | Cardinality | In 2024 sheet |
|---|:---:|---|---|---|---|---|
| `SIMPLE` | ❌ | `UInt8` | 0 | — | constant [1] | ❌ |
| `BITPIX` | ❌ | `Int8` | 0 | — | constant [1] | ❌ |
| `NAXIS` | ❌ | `UInt8` | 0 | — | constant [1] | ❌ |
| `NAXIS1` | ❌ | `UInt16` | 0 | 1 pix | constant [1] | ✅ `int16` |
| `NAXIS2` | ❌ | `UInt16` | 0 | 1 pix | constant [1] | ✅ `int16` |
| `EXTEND` | ❌ | `UInt8` | 0 | — | constant [1] | ❌ |
| `BZERO` | ❌ | `Float32` | nan | — | constant [1] | ❌ |
| `BSCALE` | ❌ | `Float32` | nan | — | constant [1] | ❌ |
| `LONGSTRN` | ❌ | `LowCardinality(String)` | '' | — | constant [1] | ❌ |

</details>

<details><summary><b>Identification, files and versions</b> (26 of 31 kept)</summary>

| Keyword | Include | Data type | Default | Resolution | Cardinality | In 2024 sheet |
|---|:---:|---|---|---|---|---|
| `FILENAME` | ✅ | `String` | '' | full name | per row (sub-image) [1224] | ❌ |
| `PROJNAME` | ✅ | `LowCardinality(String)` | '' | — | per camera (≤44) [2] | ❌ |
| `FULLPROJ` | ❌ | `LowCardinality(String)` | '' | — | per camera (≤44) [2] | ❌ |
| `NODENUMB` | ✅ | `UInt8` | 0 | 1 | constant [1] | ✅ `int16` |
| `MOUNTNUM` | ✅ | `UInt8` | 0 | 1 | per mount (≤11) [2] | ✅ `int8` |
| `CAMNUM` | ✅ | `UInt8` | 0 | 1 | low (<100) [1] | ✅ `int8` |
| `CROPID` | ✅ | `UInt8` | 0 | 1 | per row (sub-image) [24] | ✅ `int16` |
| `CCDID` | ❌ | `UInt8` | 0 | 1 | constant [1] | ✅ `int16` |
| `LEVEL` | ❌ | `LowCardinality(String)` | '' | — | constant [1] | ❌ |
| `IMTYPE` | ❌ | `LowCardinality(String)` | '' | — | constant [1] | ❌ |
| `VERSION` | ✅ | `UInt8` | 0 | 1 | low (<100) [1] | ❌ |
| `SUBDIR` | ✅ | `String` | '' | — | per visit [51] | ✅ `char16` |
| `DIRYEAR` | ✅ | `UInt16` | 0 | 1 | low (<100) [1] | ✅ `int16` |
| `DIRMON` | ✅ | `UInt8` | 0 | 1 | low (<100) [1] | ✅ `int8` |
| `DIRDAY` | ✅ | `UInt8` | 0 | 1 | low (<100) [1] | ✅ `int8` |
| `FILETIME` | ✅ | `String` | '' | 1 ms | per visit [51] | ✅ `string` |
| `TIME` | ✅ | `String` | '' | 1 ms | per visit [51] | ❌ |
| `PIPEVER` | ✅ | `LowCardinality(String)` | '' | — | low (<100) [1] | ✅ `char32` |
| `GITASTRP` | ✅ | `LowCardinality(String)` | '' | — | low (<100) [1] | ❌ |
| `GIT_CAMR` | ✅ | `LowCardinality(String)` | '' | — | low (<100) [1] | ❌ |
| `GITFOCUS` | ✅ | `LowCardinality(String)` | '' | — | low (<100) [1] | ❌ |
| `ID_COADD` | ✅ | `UInt64` | 0 | 1 | per row (sub-image) [1224] | ❌ |
| `ID_PROC` | ❌ | `UInt64` | 0 | 1 | per row (sub-image) [1224] | ❌ |
| `ID_PROCF` | ✅ | `UInt64` | 0 | 1 | per row (sub-image) [1224] | ❌ |
| `ID_PROCL` | ✅ | `UInt64` | 0 | 1 | per row (sub-image) [1224] | ❌ |
| `ID_RAW` | ✅ | `UInt64` | 0 | 1 | per visit [51] | ❌ |
| `BIAS_IM` | ✅ | `LowCardinality(String)` | '' | — | hundreds–few thousand [2] | ❌ |
| `ID_DARK` | ✅ | `UInt64` | 0 | 1 | hundreds–few thousand [2] | ✅ `int64` |
| `FLAT_IM` | ✅ | `LowCardinality(String)` | '' | — | hundreds–few thousand [2] | ❌ |
| `ID_FLAT` | ✅ | `UInt64` | 0 | 1 | hundreds–few thousand [2] | ✅ `int64` |
| `PSTATUS` | ✅ | `UInt32` | 0 | bit mask | low (<100) [2] | ❌ |

</details>

<details><summary><b>Target and observation time</b> (16 of 16 kept)</summary>

| Keyword | Include | Data type | Default | Resolution | Cardinality | In 2024 sheet |
|---|:---:|---|---|---|---|---|
| `OBJECT` | ✅ | `LowCardinality(String)` | '' | — | hundreds–few thousand [17] | ✅ `char16` |
| `FIELDID` | ✅ | `LowCardinality(String)` | '' | — | hundreds–few thousand [17] | ✅ `char16` |
| `SCHEDULR` | ✅ | `LowCardinality(String)` | '' | — | low (<100) [new, not in the sample] | ❌ |
| `DATE-OBS` | ✅ | `DateTime64(3,'UTC')` | 0 (1970-01-01) | 1 ms | per visit [51] | ✅ dateobs `DateTime64(3)` |
| `JD` | ✅ | `Float64` | nan | 1e-8 d (≈1 ms) | per visit [51] | ✅ jd_start `float64` |
| `JD_START` | ✅ | `Float64` | nan | 1e-8 d | per visit [51] | ❌ |
| `MIDJD` | ✅ | `Float64` | nan | 1e-8 d | per visit [51] | ✅ `float64` |
| `MINJD` | ✅ | `Float64` | nan | 1e-8 d | per visit [51] | ❌ |
| `MAXJD` | ✅ | `Float64` | nan | 1e-8 d | per visit [51] | ❌ |
| `LST` | ✅ | `Float32` | nan | 1e-4 deg | per visit [51] | ✅ `float64` |
| `CRDATE` | ✅ | `DateTime64(3,'UTC')` | 0 (1970-01-01) | 1 ms | per visit [51] | ❌ |
| `EXPTIME` | ✅ | `Float32` | nan | 1 ms | low (<100) [1] | ✅ `float32` |
| `EXPMODE` | ✅ | `LowCardinality(String)` | '' | — | low (<100) [1] | ❌ |
| `COUNTER` | ✅ | `UInt32` | 0 | 1 | constant [1] | ✅ `int16` |
| `FILTER` | ✅ | `LowCardinality(String)` | '' | — | low (<100) [1] | ✅ `char8` |
| `AIRMASS` | ✅ | `Float32` | nan | 1e-4 | per row (sub-image) [1224] | ✅ `float32` |

</details>

<details><summary><b>Observatory and mount</b> (22 of 24 kept)</summary>

| Keyword | Include | Data type | Default | Resolution | Cardinality | In 2024 sheet |
|---|:---:|---|---|---|---|---|
| `TIMEZONE` | ❌ | `Float32` | nan | — | constant [1] | ❌ |
| `OBSLON` | ✅ | `Float64` | nan | 1e-6 deg | per mount (≤11) [2] | ✅ `float32` |
| `OBSLAT` | ✅ | `Float64` | nan | 1e-6 deg | per mount (≤11) [2] | ✅ `float32` |
| `OBSALT` | ✅ | `Float32` | nan | 0.1 m | per mount (≤11) [1] | ✅ `float32` |
| `EQUINOX` | ❌ | `Float32` | nan | — | constant [1] | ❌ |
| `M_RA` | ✅ | `Float32` | nan | 1e-4 deg | per visit [51] | ✅ `float32` |
| `M_HA` | ✅ | `Float32` | nan | 1e-4 deg | per visit [51] | ✅ `float32` |
| `M_DEC` | ✅ | `Float32` | nan | 1e-4 deg | per visit [51] | ✅ `float32` |
| `M_AZ` | ✅ | `Float32` | nan | 1e-4 deg | per visit [51] | ❌ |
| `M_ALT` | ✅ | `Float32` | nan | 1e-4 deg | per visit [51] | ❌ |
| `M_JRA` | ✅ | `Float32` | nan | 1e-4 deg | per visit [51] | ✅ `float32` |
| `M_JDEC` | ✅ | `Float32` | nan | 1e-4 deg | per visit [50] | ✅ `float32` |
| `M_ARA` | ✅ | `Float32` | nan | 1e-4 deg | per visit [51] | ✅ `float32` |
| `M_AHA` | ✅ | `Float32` | nan | 1e-4 deg | per visit [51] | ✅ `float32` |
| `M_ADEC` | ✅ | `Float32` | nan | 1e-4 deg | per visit [51] | ✅ `float32` |
| `M_ADRA` | ✅ | `Float32` | nan | 1e-4 deg | per visit [51] | ✅ `float32` |
| `M_ADHA` | ✅ | `Float32` | nan | 1e-4 deg | per visit [51] | ✅ `float32` |
| `M_ADDEC` | ✅ | `Float32` | nan | 1e-4 deg | per visit [51] | ✅ `float32` |
| `TRK_RA` | ✅ | `Float32` | nan | 1e-6 arcsec/s | low (<100) [1] | ❌ |
| `TRK_DEC` | ✅ | `Float32` | nan | 1e-6 arcsec/s | low (<100) [1] | ❌ |
| `MNTTEMP` | ✅ | `Float32` | nan | 0.05 °C | per visit [27] | ✅ temp_mnt `float32` |
| `FOCUS` | ✅ | `Float32` | nan | 1 step | hundreds–few thousand [8] | ✅ `Int32` |
| `PRVFOCUS` | ✅ | `Float32` | nan | 1 step | hundreds–few thousand [8] | ✅ `Int32` |
| `OPT_CLN` | ✅ | `Float64` | nan | 1 d | per camera (≤44) [1; in 1/1224 files] | ✅ optics_cln `float32` |

</details>

<details><summary><b>Camera and detector</b> (22 of 22 kept)</summary>

| Keyword | Include | Data type | Default | Resolution | Cardinality | In 2024 sheet |
|---|:---:|---|---|---|---|---|
| `CAMNAME` | ✅ | `LowCardinality(String)` | '' | — | per camera (≤44) [2] | ❌ |
| `CAMTEMP` | ✅ | `Float32` | nan | 0.1 °C | low (<100) [2] | ❌ |
| `CAMCOOL` | ✅ | `Float32` | nan | 0.1 % | low (<100) [19] | ✅ `float32` |
| `CAMMODE` | ✅ | `UInt8` | 0 | 1 | low (<100) [1] | ❌ |
| `CAMGAIN` | ✅ | `UInt16` | 0 | 1 | low (<100) [1] | ✅ `float32` |
| `CAMOFFS` | ✅ | `UInt16` | 0 | 1 ADU | low (<100) [1] | ✅ `float32` |
| `GAIN` | ✅ | `Float32` | nan | 1e-5 e/ADU | per row (sub-image) [378] | ❌ |
| `ORIGGAIN` | ✅ | `Float32` | nan | 1e-3 | constant [1] | ✅ `float32` |
| `READNOI` | ✅ | `Float32` | nan | 1e-4 e | per row (sub-image) [201] | ✅ `float32` |
| `DARKCUR` | ✅ | `Float32` | nan | 1e-4 e/s | low (<100) [1] | ✅ `float32` |
| `SATURVAL` | ✅ | `Float32` | nan | 1 ADU | low (<100) [1] | ✅ `float32` |
| `NONLIN` | ✅ | `Float32` | nan | 1 ADU | low (<100) [1] | ✅ `float32` |
| `BINX` | ✅ | `UInt8` | 0 | 1 | constant [1] | ✅ `float32` |
| `BINY` | ✅ | `UInt8` | 0 | 1 | constant [1] | ✅ `float32` |
| `LIGHTSEC` | ✅ | 4 × `Int16`: `lightsec_xmin`, `_xmax`, `_ymin`, `_ymax` | 0 | 1 pix | constant [1] | ❌ |
| `OVERSCAN` | ✅ | 4 × `Int16`: `overscan_xmin`, `_xmax`, `_ymin`, `_ymax` | 0 | 1 pix | constant [1] | ❌ |
| `CCDSEC` | ✅ | 4 × `Int16`: `ccdsec_xmin`, `_xmax`, `_ymin`, `_ymax` | 0 | 1 pix | constant [1] | ❌ |
| `ORIGSEC` | ✅ | 4 × `Int16`: `origsec_xmin`, `_xmax`, `_ymin`, `_ymax` | 0 | 1 pix | per row (sub-image) [24] | ✅ split into 4 `int16` |
| `ORIGUSEC` | ✅ | 4 × `Int16`: `origusec_xmin`, `_xmax`, `_ymin`, `_ymax` | 0 | 1 pix | per row (sub-image) [24] | ✅ split into 4 `int16` |
| `UNIQSEC` | ✅ | 4 × `Int16`: `uniqsec_xmin`, `_xmax`, `_ymin`, `_ymax` | 0 | 1 pix | per row (sub-image) [12] | ✅ split into 4 `int16` |
| `EXCLSEC` | ✅ | 4 × `Int16`: `exclsec_xmin`, `_xmax`, `_ymin`, `_ymax` | 0 | 1 pix | per row (sub-image) [12] | ❌ |
| `ORIGESEC` | ✅ | 4 × `Int16`: `origesec_xmin`, `_xmax`, `_ymin`, `_ymax` | 0 | 1 pix | per row (sub-image) [24] | ❌ |

</details>

<details><summary><b>Coaddition</b> (5 of 6 kept)</summary>

| Keyword | Include | Data type | Default | Resolution | Cardinality | In 2024 sheet |
|---|:---:|---|---|---|---|---|
| `NCOADD` | ✅ | `UInt8` | 0 | 1 | low (<100) [1] | ✅ `int16` |
| `COADDOP` | ✅ | `LowCardinality(String)` | '' | — | constant [1] | ✅ `char16` |
| `AVNCOADD` | ✅ | `Float32` | nan | 1e-3 | per row (sub-image) [1160] | ✅ `float32` |
| `MINCOADD` | ✅ | `UInt8` | 0 | 1 | low (<100) [1] | ❌ |
| `MAXBCKD` | ✅ | `Float32` | nan | 0.01 e | per row (sub-image) [new, not in the sample] | ❌ |
| `SUBLEVEL` | ❌ | `LowCardinality(String)` | '' | — | constant [1] | ❌ |

</details>

<details><summary><b>Background and image statistics</b> (17 of 18 kept)</summary>

| Keyword | Include | Data type | Default | Resolution | Cardinality | In 2024 sheet |
|---|:---:|---|---|---|---|---|
| `MEANBCK` | ✅ | `Float32` | nan | 0.01 e | per row (sub-image) [1224] | ✅ `float32` |
| `MEDBCK` | ✅ | `Float32` | nan | 0.01 e | per row (sub-image) [1223] | ✅ `float32` |
| `STDBCK` | ✅ | `Float32` | nan | 1e-3 e | per row (sub-image) [1224] | ✅ `float32` |
| `MINBCK` | ✅ | `Float32` | nan | 0.01 e | per row (sub-image) [1223] | ❌ |
| `MAXBCK` | ✅ | `Float32` | nan | 0.01 e | per row (sub-image) [1223] | ❌ |
| `MEANVAR` | ✅ | `Float32` | nan | 1e-3 e² | per row (sub-image) [1224] | ✅ `float32` |
| `MEDVAR` | ✅ | `Float32` | nan | 1e-3 e² | per row (sub-image) [1222] | ✅ `float32` |
| `BCKMTHD` | ✅ | `UInt8` | 0 | 1 | constant [1] | ❌ |
| `VARMTHD` | ✅ | `UInt8` | 0 | 1 | constant [1] | ❌ |
| `BACKMAG` | ✅ | `Float32` | nan | 1e-3 mag | per row (sub-image) [1218] | ✅ `float32` |
| `N_STARS` | ✅ | `UInt32` | 0 | 1 | per row (sub-image) [844] | ❌ |
| `MAG_95Q` | ✅ | `Float32` | nan | 1e-3 mag | per row (sub-image) [1224] | ❌ |
| `M_CHI2D` | ✅ | `Float32` | nan | 1e-3 | per row (sub-image) [1224] | ❌ |
| `SPATQ50` | ✅ | `Float32` | nan | 0.1 | low (<100) [24] | ❌ |
| `SPATQ75` | ✅ | `Float32` | nan | 0.1 | low (<100) [27] | ❌ |
| `SPATQ90` | ✅ | `Float32` | nan | 0.1 | low (<100) [90] | ❌ |
| `SPATQ95` | ✅ | `Float32` | nan | 0.1 | low (<100) [178] | ❌ |
| `SPATNBIN` | ❌ | `UInt16` | 0 | 1 | constant [1] | ❌ |

</details>

<details><summary><b>Astrometry and sky footprint</b> (33 of 33 kept)</summary>

| Keyword | Include | Data type | Default | Resolution | Cardinality | In 2024 sheet |
|---|:---:|---|---|---|---|---|
| `RA` | ✅ | `Float64` | nan | 1e-7 deg | per row (sub-image) [1224] | ✅ `float64` |
| `DEC` | ✅ | `Float64` | nan | 1e-7 deg | per row (sub-image) [1224] | ✅ `float64` |
| `UPIX_PAR` | ✅ | `UInt64` | 0 | 1 | hundreds–few thousand [23] | ✅ upix_partition `uint64` |
| `UPIX_LOW` | ✅ | `UInt64` | 0 | 1 | per row (sub-image) [476] | ✅ `uint64` |
| `UPIX_HIG` | ✅ | `UInt64` | 0 | 1 | per row (sub-image) [1224] | ✅ upix_high `uint64` |
| `AST_NSRC` | ✅ | `UInt16` | 0 | 1 | per row (sub-image) [301] | ✅ `Int32` |
| `AST_ARMS` | ✅ | `Float32` | nan | 1e-5 arcsec | per row (sub-image) [1224] | ✅ `float32` |
| `AST_ERRM` | ✅ | `Float32` | nan | 1e-6 arcsec | per row (sub-image) [1224] | ✅ `float32` |
| `AST_CAT` | ✅ | `LowCardinality(String)` | '' | — | constant [1] | ❌ |
| `RP_MRMS` | ✅ | `Float32` | nan | 1e-5 | per row (sub-image) [1211] | ❌ |
| `RP_MMRMS` | ✅ | `Float32` | nan | 1e-3 | per row (sub-image) [1211] | ❌ |
| `PIXSCALE` | ✅ | `Float32` | nan | 1e-5 arcsec | per row (sub-image) [1224] | ❌ |
| `ROTAT` | ✅ | `Float32` | nan | 1e-4 deg | per row (sub-image) [1224] | ❌ |
| `RA1` | ✅ | `Float64` | nan | 1e-7 deg | per row (sub-image) [1224] | ✅ `float32` |
| `RA2` | ✅ | `Float64` | nan | 1e-7 deg | per row (sub-image) [1224] | ✅ `float32` |
| `RA3` | ✅ | `Float64` | nan | 1e-7 deg | per row (sub-image) [1224] | ✅ `float32` |
| `RA4` | ✅ | `Float64` | nan | 1e-7 deg | per row (sub-image) [1224] | ✅ `float32` |
| `DEC1` | ✅ | `Float64` | nan | 1e-7 deg | per row (sub-image) [1224] | ✅ `float32` |
| `DEC2` | ✅ | `Float64` | nan | 1e-7 deg | per row (sub-image) [1224] | ✅ `float32` |
| `DEC3` | ✅ | `Float64` | nan | 1e-7 deg | per row (sub-image) [1224] | ✅ `float32` |
| `DEC4` | ✅ | `Float64` | nan | 1e-7 deg | per row (sub-image) [1224] | ✅ `float32` |
| `RAU1` | ✅ | `Float64` | nan | 1e-7 deg | per row (sub-image) [1224] | ✅ `float32` |
| `RAU2` | ✅ | `Float64` | nan | 1e-7 deg | per row (sub-image) [1224] | ✅ `float32` |
| `RAU3` | ✅ | `Float64` | nan | 1e-7 deg | per row (sub-image) [1224] | ✅ `float32` |
| `RAU4` | ✅ | `Float64` | nan | 1e-7 deg | per row (sub-image) [1224] | ✅ `float32` |
| `DECU1` | ✅ | `Float64` | nan | 1e-7 deg | per row (sub-image) [1224] | ✅ `float32` |
| `DECU2` | ✅ | `Float64` | nan | 1e-7 deg | per row (sub-image) [1224] | ✅ `float32` |
| `DECU3` | ✅ | `Float64` | nan | 1e-7 deg | per row (sub-image) [1224] | ✅ `float32` |
| `DECU4` | ✅ | `Float64` | nan | 1e-7 deg | per row (sub-image) [1224] | ✅ `float32` |
| `GM_RATEX` | ✅ | `Float32` | nan | 1e-6 pix/s | per row (sub-image) [1224] | ✅ `float32` |
| `GM_STDX` | ✅ | `Float32` | nan | 1e-4 pix | per row (sub-image) [1224] | ✅ `float32` |
| `GM_RATEY` | ✅ | `Float32` | nan | 1e-6 pix/s | per row (sub-image) [1224] | ✅ `float32` |
| `GM_STDY` | ✅ | `Float32` | nan | 1e-4 pix | per row (sub-image) [1224] | ✅ `float32` |

</details>

<details><summary><b>WCS solution</b> (28 of 36 kept)</summary>

| Keyword | Include | Data type | Default | Resolution | Cardinality | In 2024 sheet |
|---|:---:|---|---|---|---|---|
| `CTYPE1` | ❌ | `LowCardinality(String)` | '' | — | constant [1] | ✅ ctype `char8` |
| `CTYPE2` | ❌ | `LowCardinality(String)` | '' | — | constant [1] | ❌ |
| `WCSAXES` | ❌ | `UInt8` | 0 | — | constant [1] | ❌ |
| `RADESYS` | ❌ | `LowCardinality(String)` | '' | — | constant [1] | ❌ |
| `LONPOLE` | ❌ | `Float32` | nan | — | constant [1] | ❌ |
| `LATPOLE` | ❌ | `Float32` | nan | — | constant [1] | ❌ |
| `CUNIT1` | ❌ | `LowCardinality(String)` | '' | — | constant [1] | ❌ |
| `CUNIT2` | ❌ | `LowCardinality(String)` | '' | — | constant [1] | ❌ |
| `CRPIX1` | ✅ | `Float64` | nan | 1e-6 pix | per row (sub-image) [1224] | ✅ `float32` |
| `CRPIX2` | ✅ | `Float64` | nan | 1e-6 pix | per row (sub-image) [1224] | ✅ `float32` |
| `CRVAL1` | ✅ | `Float64` | nan | 1e-9 deg | per row (sub-image) [1224] | ✅ `float32` |
| `CRVAL2` | ✅ | `Float64` | nan | 1e-9 deg | per row (sub-image) [1224] | ✅ `float32` |
| `CD1_1` | ✅ | `Float64` | nan | full (1e-12) | per row (sub-image) [1224] | ✅ `float32` |
| `CD1_2` | ✅ | `Float64` | nan | full (1e-12) | per row (sub-image) [1224] | ✅ `float32` |
| `CD2_1` | ✅ | `Float64` | nan | full (1e-12) | per row (sub-image) [1224] | ✅ `float32` |
| `CD2_2` | ✅ | `Float64` | nan | full (1e-12) | per row (sub-image) [1224] | ✅ `float32` |
| `PV1_0` | ✅ | `Float64` | nan | full (1e-12) | per row (sub-image) [1224] | ✅ `float32` |
| `PV1_1` | ✅ | `Float64` | nan | full (1e-12) | per row (sub-image) [1224] | ✅ `float32` |
| `PV1_2` | ✅ | `Float64` | nan | full (1e-12) | per row (sub-image) [1224] | ✅ `float32` |
| `PV1_4` | ✅ | `Float64` | nan | full (1e-12) | per row (sub-image) [1224] | ✅ `float32` |
| `PV1_5` | ✅ | `Float64` | nan | full (1e-12) | per row (sub-image) [1224] | ✅ `float32` |
| `PV1_6` | ✅ | `Float64` | nan | full (1e-12) | per row (sub-image) [1224] | ✅ `float32` |
| `PV1_7` | ✅ | `Float64` | nan | full (1e-12) | per row (sub-image) [1224] | ✅ `float32` |
| `PV1_8` | ✅ | `Float64` | nan | full (1e-12) | per row (sub-image) [1224] | ✅ `float32` |
| `PV1_9` | ✅ | `Float64` | nan | full (1e-12) | per row (sub-image) [1224] | ✅ `float32` |
| `PV1_10` | ✅ | `Float64` | nan | full (1e-12) | per row (sub-image) [1224] | ✅ `float32` |
| `PV2_0` | ✅ | `Float64` | nan | full (1e-12) | per row (sub-image) [1224] | ✅ `float32` |
| `PV2_1` | ✅ | `Float64` | nan | full (1e-12) | per row (sub-image) [1224] | ✅ `float32` |
| `PV2_2` | ✅ | `Float64` | nan | full (1e-12) | per row (sub-image) [1224] | ✅ `float32` |
| `PV2_4` | ✅ | `Float64` | nan | full (1e-12) | per row (sub-image) [1224] | ✅ `float32` |
| `PV2_5` | ✅ | `Float64` | nan | full (1e-12) | per row (sub-image) [1224] | ✅ `float32` |
| `PV2_6` | ✅ | `Float64` | nan | full (1e-12) | per row (sub-image) [1224] | ✅ `float32` |
| `PV2_7` | ✅ | `Float64` | nan | full (1e-12) | per row (sub-image) [1224] | ✅ `float32` |
| `PV2_8` | ✅ | `Float64` | nan | full (1e-12) | per row (sub-image) [1224] | ✅ `float32` |
| `PV2_9` | ✅ | `Float64` | nan | full (1e-12) | per row (sub-image) [1224] | ✅ `float32` |
| `PV2_10` | ✅ | `Float64` | nan | full (1e-12) | per row (sub-image) [1224] | ✅ `float32` |

</details>

<details><summary><b>PSF</b> (22 of 22 kept)</summary>

| Keyword | Include | Data type | Default | Resolution | Cardinality | In 2024 sheet |
|---|:---:|---|---|---|---|---|
| `FWHE` | ✅ | `Float32` | nan | 1e-3 arcsec | per row (sub-image) [1224] | ❌ |
| `FWHM` | ✅ | `Float32` | nan | 1e-3 arcsec | per row (sub-image) [1224] | ✅ `float32` |
| `PSF_FITN` | ✅ | `Float32` | nan | 1e-4 | per row (sub-image) [1224] | ❌ |
| `PSF_FITA` | ✅ | `Float32` | nan | 1e-4 | per row (sub-image) [1224] | ❌ |
| `PSF_FITB` | ✅ | `Float32` | nan | 1e-4 | per row (sub-image) [1224] | ❌ |
| `PSF_FITT` | ✅ | `Float32` | nan | 0.01 deg | per row (sub-image) [1224] | ❌ |
| `PSF_NST` | ✅ | `UInt16` | 0 | 1 | per row (sub-image) [132] | ❌ |
| `PSF_NPK` | ✅ | `UInt8` | 0 | 1 | low (<100) [3] | ❌ |
| `PSF_PKR` | ✅ | `Float32` | nan | 1e-3 | low (<100) [14] | ❌ |
| `PSF_DPK` | ✅ | `Float32` | nan | 1e-3 pix | low (<100) [11] | ❌ |
| `PSF_RPK` | ✅ | `Float32` | nan | 0.1 pix | low (<100) [1] | ❌ |
| `PSF_ERR` | ✅ | `Float32` | nan | 1e-6 | per row (sub-image) [1224] | ❌ |
| `PSF_S2` | ✅ | `Float32` | nan | 1e-6 | per row (sub-image) [1224] | ❌ |
| `PSF_AF_1` | ✅ | `Float32` | nan | 1e-4 | per row (sub-image) [1224] | ❌ |
| `PSF_AF_2` | ✅ | `Float32` | nan | 1e-4 | per row (sub-image) [1224] | ❌ |
| `PSF_AF_3` | ✅ | `Float32` | nan | 1e-4 | per row (sub-image) [1224] | ❌ |
| `PSF_AF_4` | ✅ | `Float32` | nan | 1e-4 | per row (sub-image) [1224] | ❌ |
| `MED_X2` | ✅ | `Float32` | nan | 1e-4 pix² | per row (sub-image) [1224] | ❌ |
| `MED_Y2` | ✅ | `Float32` | nan | 1e-4 pix² | per row (sub-image) [1224] | ❌ |
| `MED_XY` | ✅ | `Float32` | nan | 1e-5 pix² | per row (sub-image) [1224] | ❌ |
| `SNCHI2` | ✅ | `Float32` | nan | 1e-5 mag | per row (sub-image) [1223; in 1223/1224 files] | ❌ |
| `QUICFWHM` | ✅ | `Float32` | nan | 1e-3 pix | per visit [new, not in the sample] | ❌ |

</details>

<details><summary><b>Relative photometric calibration (PH_)</b> (8 of 9 kept)</summary>

| Keyword | Include | Data type | Default | Resolution | Cardinality | In 2024 sheet |
|---|:---:|---|---|---|---|---|
| `PH_ZP` | ✅ | `Float32` | nan | 1e-4 mag | per row (sub-image) [1224] | ✅ `float32` |
| `PH_COL1` | ✅ | `Float32` | nan | 1e-4 mag | per row (sub-image) [1224] | ✅ `float32` |
| `PH_MEDC` | ✅ | `Float32` | nan | 1e-4 mag | per row (sub-image) [1098] | ✅ `float32` |
| `PH_RMS` | ✅ | `Float32` | nan | 1e-4 mag | per row (sub-image) [1224] | ✅ `float32` |
| `PH_NSRC` | ✅ | `UInt16` | 0 | 1 | per row (sub-image) [152] | ✅ `Int32` |
| `PH_MAGSY` | ✅ | `LowCardinality(String)` | '' | — | constant [1] | ✅ `char8` |
| `PH_MAGT` | ✅ | `LowCardinality(String)` | '' | — | constant [1] | ✅ `char16` |
| `PH_MAGTE` | ❌ | `LowCardinality(String)` | '' | — | constant [1] | ✅ `char16` |
| `PH_CAT` | ✅ | `LowCardinality(String)` | '' | — | constant [1] | ❌ |

</details>

<details><summary><b>Absolute photometric calibration (PT_): summary</b> (23 of 23 kept)</summary>

| Keyword | Include | Data type | Default | Resolution | Cardinality | In 2024 sheet |
|---|:---:|---|---|---|---|---|
| `PT_ZP` | ✅ | `Float32` | nan | 1e-4 mag | per row (sub-image) [1224] | ❌ |
| `PT_NORMU` | ✅ | `LowCardinality(String)` | '' | — | constant [new, not in the sample] | ❌ |
| `PT_FNORM` | ✅ | `Float32` | nan | 1e-5 | per row (sub-image) [new, not in the sample] | ❌ |
| `LIMMAG` | ✅ | `Float32` | nan | 1e-3 mag | per row (sub-image) [1224] | ✅ `float32` |
| `LIMMAGER` | ✅ | `Float32` | nan | 1e-4 mag | per row (sub-image) [1224] | ❌ |
| `PT_RMS` | ✅ | `Float32` | nan | 1e-4 mag | per row (sub-image) [1224] | ✅ `float32` |
| `PT_ARMS` | ✅ | `Float32` | nan | 1e-4 mag | per row (sub-image) [1215] | ❌ |
| `PT_CHI2` | ✅ | `Float32` | nan | 1e-3 | per row (sub-image) [1224] | ❌ |
| `PT_DOF` | ✅ | `UInt16` | 0 | 1 | per row (sub-image) [116] | ❌ |
| `PT_NCALI` | ✅ | `UInt16` | 0 | 1 | per row (sub-image) [119] | ❌ |
| `PT_DZPAB` | ✅ | `Float32` | nan | 1e-5 mag | per row (sub-image) [1224] | ❌ |
| `PT_CTA` | ✅ | `Float32` | nan | 1e-5 mag | per row (sub-image) [1224] | ❌ |
| `PT_CTA2` | ✅ | `Float32` | nan | 1e-5 mag | per row (sub-image) [1224] | ❌ |
| `PT_CTA3` | ✅ | `Float32` | nan | 1e-6 mag | per row (sub-image) [1224] | ❌ |
| `PT_CTA4` | ✅ | `Float32` | nan | 1e-6 mag | per row (sub-image) [1224] | ❌ |
| `PT_CTAE` | ✅ | `Float32` | nan | 1e-6 mag | per row (sub-image) [1224] | ❌ |
| `PT_AREF` | ✅ | `LowCardinality(String)` | '' | — | constant [1] | ❌ |
| `PT_SPEC` | ✅ | `LowCardinality(String)` | '' | — | constant [1] | ❌ |
| `PT_CAT` | ✅ | `LowCardinality(String)` | '' | — | constant [1] | ❌ |
| `PT_REFSL` | ✅ | `Float32` | nan | 1e-3 | constant [1] | ❌ |
| `PT_REFPV` | ✅ | `Float32` | nan | 1 | constant [1] | ❌ |
| `PT_REFC` | ✅ | `Float32` | nan | 1e-3 | constant [1] | ❌ |
| `PT_CO2PP` | ✅ | `Float32` | nan | 1 ppm | constant [1] | ❌ |

</details>

<details><summary><b>Absolute photometric calibration (PT_): transmission-model parameters</b> (77 of 77 kept)</summary>

| Keyword | Include | Data type | Default | Resolution | Cardinality | In 2024 sheet |
|---|:---:|---|---|---|---|---|
| `PT_1_N` | ✅ | `LowCardinality(String)` | '' | — | constant [1] | ❌ |
| `PT_1_V1` | ✅ | `Float32` | nan | 1e-5 | per row (sub-image) [1224] | ❌ |
| `PT_1_F1` | ✅ | `UInt8` | 0 | 1 | constant [2] | ❌ |
| `PT_2_N` | ✅ | `LowCardinality(String)` | '' | — | constant [1] | ❌ |
| `PT_2_V1` | ✅ | `Float32` | nan | 1e-5 | per row (sub-image) [1224] | ❌ |
| `PT_2_F1` | ✅ | `UInt8` | 0 | 1 | constant [2] | ❌ |
| `PT_2_V2` | ✅ | `Float32` | nan | 1e-5 | low (<100) [2] | ❌ |
| `PT_2_F2` | ✅ | `UInt8` | 0 | 1 | constant [2] | ❌ |
| `PT_3_N` | ✅ | `LowCardinality(String)` | '' | — | constant [1] | ❌ |
| `PT_3_V1` | ✅ | `Float32` | nan | 1e-5 | per row (sub-image) [1224] | ❌ |
| `PT_3_F1` | ✅ | `UInt8` | 0 | 1 | constant [2] | ❌ |
| `PT_3_V2` | ✅ | `Float32` | nan | 1e-5 | per row (sub-image) [1214] | ❌ |
| `PT_3_F2` | ✅ | `UInt8` | 0 | 1 | constant [3] | ❌ |
| `PT_3_V3` | ✅ | `Float32` | nan | 1e-5 | low (<100) [2] | ❌ |
| `PT_3_F3` | ✅ | `UInt8` | 0 | 1 | constant [2] | ❌ |
| `PT_4_N` | ✅ | `LowCardinality(String)` | '' | — | constant [1] | ❌ |
| `PT_4_V1` | ✅ | `Float32` | nan | 1e-5 | per row (sub-image) [1224] | ❌ |
| `PT_4_F1` | ✅ | `UInt8` | 0 | 1 | constant [2] | ❌ |
| `PT_4_V2` | ✅ | `Float32` | nan | 1e-5 | low (<100) [2] | ❌ |
| `PT_4_F2` | ✅ | `UInt8` | 0 | 1 | constant [2] | ❌ |
| `PT_5_N` | ✅ | `LowCardinality(String)` | '' | — | constant [1] | ❌ |
| `PT_5_V1` | ✅ | `Float32` | nan | 1e-5 | per row (sub-image) [1224] | ❌ |
| `PT_5_F1` | ✅ | `UInt8` | 0 | 1 | constant [2] | ❌ |
| `PT_5_V2` | ✅ | `Float32` | nan | 1e-5 | per row (sub-image) [1210] | ❌ |
| `PT_5_F2` | ✅ | `UInt8` | 0 | 1 | constant [3] | ❌ |
| `PT_5_V3` | ✅ | `Float32` | nan | 1e-5 | low (<100) [2] | ❌ |
| `PT_5_F3` | ✅ | `UInt8` | 0 | 1 | constant [2] | ❌ |
| `PT_6_N` | ✅ | `LowCardinality(String)` | '' | — | constant [1] | ❌ |
| `PT_6_V1` | ✅ | `Float32` | nan | 1e-5 | per row (sub-image) [1224] | ❌ |
| `PT_6_F1` | ✅ | `UInt8` | 0 | 1 | constant [2] | ❌ |
| `PT_6_V2` | ✅ | `Float32` | nan | 1e-5 | low (<100) [28] | ❌ |
| `PT_6_F2` | ✅ | `UInt8` | 0 | 1 | constant [2] | ❌ |
| `PT_6_V3` | ✅ | `Float32` | nan | 1e-5 | low (<100) [2] | ❌ |
| `PT_6_F3` | ✅ | `UInt8` | 0 | 1 | constant [2] | ❌ |
| `PT_6_V4` | ✅ | `Float32` | nan | 1e-5 | low (<100) [2] | ❌ |
| `PT_6_F4` | ✅ | `UInt8` | 0 | 1 | constant [2] | ❌ |
| `PT_7_N` | ✅ | `LowCardinality(String)` | '' | — | constant [1] | ❌ |
| `PT_7_V1` | ✅ | `Float32` | nan | 1e-5 | per row (sub-image) [2] | ❌ |
| `PT_7_F1` | ✅ | `UInt8` | 0 | 1 | constant [2] | ❌ |
| `PT_7_V2` | ✅ | `Float32` | nan | 1e-5 | low (<100) [2] | ❌ |
| `PT_7_F2` | ✅ | `UInt8` | 0 | 1 | constant [2] | ❌ |
| `PT_7_V3` | ✅ | `Float32` | nan | 1e-5 | low (<100) [2] | ❌ |
| `PT_7_F3` | ✅ | `UInt8` | 0 | 1 | constant [2] | ❌ |
| `PT_7_V4` | ✅ | `Float32` | nan | 1e-5 | low (<100) [2] | ❌ |
| `PT_7_F4` | ✅ | `UInt8` | 0 | 1 | constant [2] | ❌ |
| `PT_7_V5` | ✅ | `Float32` | nan | 1e-5 | low (<100) [2] | ❌ |
| `PT_7_F5` | ✅ | `UInt8` | 0 | 1 | constant [2] | ❌ |
| `PT_7_V6` | ✅ | `Float32` | nan | 1e-5 | low (<100) [2] | ❌ |
| `PT_7_F6` | ✅ | `UInt8` | 0 | 1 | constant [2] | ❌ |
| `PT_7_V7` | ✅ | `Float32` | nan | 1e-5 | low (<100) [2] | ❌ |
| `PT_7_F7` | ✅ | `UInt8` | 0 | 1 | constant [2] | ❌ |
| `PT_7_V8` | ✅ | `Float32` | nan | 1e-5 | low (<100) [2] | ❌ |
| `PT_7_F8` | ✅ | `UInt8` | 0 | 1 | constant [2] | ❌ |
| `PT_7_V9` | ✅ | `Float32` | nan | 1e-5 | low (<100) [2] | ❌ |
| `PT_7_F9` | ✅ | `UInt8` | 0 | 1 | constant [2] | ❌ |
| `PT_8_N` | ✅ | `LowCardinality(String)` | '' | — | constant [1] | ❌ |
| `PT_8_V1` | ✅ | `Float32` | nan | 1e-5 | per row (sub-image) [2] | ❌ |
| `PT_8_F1` | ✅ | `UInt8` | 0 | 1 | constant [2] | ❌ |
| `PT_8_V2` | ✅ | `Float32` | nan | 1e-5 | per row (sub-image) [1214] | ❌ |
| `PT_8_F2` | ✅ | `UInt8` | 0 | 1 | constant [3] | ❌ |
| `PT_8_V3` | ✅ | `Float32` | nan | 1e-5 | low (<100) [2] | ❌ |
| `PT_8_F3` | ✅ | `UInt8` | 0 | 1 | constant [2] | ❌ |
| `PT_8_V4` | ✅ | `Float32` | nan | 1e-5 | low (<100) [2] | ❌ |
| `PT_8_F4` | ✅ | `UInt8` | 0 | 1 | constant [2] | ❌ |
| `PT_P_N` | ✅ | `LowCardinality(String)` | '' | — | constant [1] | ❌ |
| `PT_P_NX1` | ✅ | `Float32` | nan | 0.5 pix | constant [2] | ❌ |
| `PT_P_NX2` | ✅ | `Float32` | nan | 0.5 pix | constant [2] | ❌ |
| `PT_P_NY1` | ✅ | `Float32` | nan | 0.5 pix | constant [2] | ❌ |
| `PT_P_NY2` | ✅ | `Float32` | nan | 0.5 pix | constant [2] | ❌ |
| `PT_P_V1` | ✅ | `Float32` | nan | 1e-6 mag | constant [2] | ❌ |
| `PT_P_F1` | ✅ | `UInt8` | 0 | 1 | constant [3] | ❌ |
| `PT_P_V2` | ✅ | `Float32` | nan | 1e-6 mag | per row (sub-image) [1214] | ❌ |
| `PT_P_F2` | ✅ | `UInt8` | 0 | 1 | constant [3] | ❌ |
| `PT_P_V3` | ✅ | `Float32` | nan | 1e-6 mag | per row (sub-image) [1214] | ❌ |
| `PT_P_F3` | ✅ | `UInt8` | 0 | 1 | constant [3] | ❌ |
| `PT_P_V4` | ✅ | `Float32` | nan | 1e-6 mag | per row (sub-image) [1214] | ❌ |
| `PT_P_F4` | ✅ | `UInt8` | 0 | 1 | constant [3] | ❌ |

</details>

<details><summary><b>Aperture corrections</b> (26 of 26 kept)</summary>

| Keyword | Include | Data type | Default | Resolution | Cardinality | In 2024 sheet |
|---|:---:|---|---|---|---|---|
| `APC0_A1` | ✅ | `Float32` | nan | 1e-6 mag | per row (sub-image) [1224] | ❌ |
| `APCX_A1` | ✅ | `Float32` | nan | 1e-6 mag | per row (sub-image) [1224] | ❌ |
| `APCY_A1` | ✅ | `Float32` | nan | 1e-6 mag | per row (sub-image) [1224] | ❌ |
| `APCXY_A1` | ✅ | `Float32` | nan | 1e-6 mag | per row (sub-image) [1224] | ❌ |
| `APCC_A1` | ✅ | `Float32` | nan | 1e-6 mag | per row (sub-image) [1224] | ❌ |
| `APCCE_A1` | ✅ | `Float32` | nan | 1e-6 mag | per row (sub-image) [1224] | ❌ |
| `APC0_A2` | ✅ | `Float32` | nan | 1e-6 mag | per row (sub-image) [1224] | ❌ |
| `APCX_A2` | ✅ | `Float32` | nan | 1e-6 mag | per row (sub-image) [1224] | ❌ |
| `APCY_A2` | ✅ | `Float32` | nan | 1e-6 mag | per row (sub-image) [1224] | ❌ |
| `APCXY_A2` | ✅ | `Float32` | nan | 1e-6 mag | per row (sub-image) [1224] | ❌ |
| `APCC_A2` | ✅ | `Float32` | nan | 1e-6 mag | per row (sub-image) [1224] | ❌ |
| `APCCE_A2` | ✅ | `Float32` | nan | 1e-6 mag | per row (sub-image) [1224] | ❌ |
| `APC0_A4` | ✅ | `Float32` | nan | 1e-6 mag | per row (sub-image) [1224] | ❌ |
| `APCX_A4` | ✅ | `Float32` | nan | 1e-6 mag | per row (sub-image) [1224] | ❌ |
| `APCY_A4` | ✅ | `Float32` | nan | 1e-6 mag | per row (sub-image) [1224] | ❌ |
| `APCXY_A4` | ✅ | `Float32` | nan | 1e-6 mag | per row (sub-image) [1224] | ❌ |
| `APCC_A4` | ✅ | `Float32` | nan | 1e-6 mag | per row (sub-image) [1224] | ❌ |
| `APCCE_A4` | ✅ | `Float32` | nan | 1e-6 mag | per row (sub-image) [1224] | ❌ |
| `APC0_PS` | ✅ | `Float32` | nan | 1e-6 mag | per row (sub-image) [1224] | ❌ |
| `APCX_PS` | ✅ | `Float32` | nan | 1e-6 mag | per row (sub-image) [1224] | ❌ |
| `APCY_PS` | ✅ | `Float32` | nan | 1e-6 mag | per row (sub-image) [1224] | ❌ |
| `APCXY_PS` | ✅ | `Float32` | nan | 1e-6 mag | per row (sub-image) [1224] | ❌ |
| `APCC_PS` | ✅ | `Float32` | nan | 1e-6 mag | per row (sub-image) [1224] | ❌ |
| `APCCE_PS` | ✅ | `Float32` | nan | 1e-6 mag | per row (sub-image) [1224] | ❌ |
| `APCC_REF` | ✅ | `Float32` | nan | 1e-3 mag | constant [2] | ❌ |
| `APCOR_N` | ✅ | `UInt16` | 0 | 1 | per row (sub-image) [421] | ❌ |

</details>

### Columns computed or corrected at ingestion

These columns cannot be copied from the header as they are. The ingestion code has to compute or convert them.

| Column(s) | Type | What the ingestion code does |
|---|---|---|
| `ingestion_time_jd` (new, not in the header) | `Float64` | JD (UTC) of the moment the row is inserted, taken from the ingesting computer's clock. Same as the column of the same name in `diff_src`. Default `nan`. |
| `ID_COADD`, `ID_PROCF`, `ID_PROCL`, `ID_RAW`, `ID_DARK`, `ID_FLAT` | `UInt64` | The header holds these IDs as text (e.g. `'2224430602496017002'`). Convert the text directly to an integer, never through a `double`. A `double` is exact only up to about 9×10¹⁵, and the IDs are about 2×10¹⁸, so the lowest bits, which hold the image type and level, would be lost. That is also why the IDs are text in the header on purpose (`imProc.db.generateImageID`, since 2024): MATLAB's FITS writer rejects `uint64`, and AstroPack reads numeric header values as `double`. Missing or blank → 0. |
| `UPIX_PAR`, `UPIX_LOW`, `UPIX_HIG` | `UInt64` | Written as text in the header (e.g. `'51198298771'`); convert to an integer. Missing → 0. |
| `LIGHTSEC`, `OVERSCAN`, `CCDSEC`, `ORIGSEC`, `ORIGUSEC`, `UNIQSEC`, `EXCLSEC`, `ORIGESEC` | 4 × `Int16` each | Split the text `'[xmin xmax ymin ymax]'` into four numbers (`imUtil.ccdsec.ccdsecStr2num`), written to `<keyword>_xmin`, `_xmax`, `_ymin`, `_ymax`. Missing → 0. |
| `DATE-OBS` (column `date_obs`) | `DateTime64(3,'UTC')` | Convert the ISO text `DATE-OBS` (e.g. `'2026-04-06T00:39:34.229'`) to a date-time with milliseconds. |
| `VERSION` | `UInt8` | Written as text (`'1'`); convert to an integer. |
| `FIELDID` | `LowCardinality(String)` | Numeric fields are written as real numbers (`1214.`), named targets as text (`'21109WD'`). Store the number without the trailing point (`'1214'`), so that it matches `OBJECT`. |
| `NODENUMB`, `MOUNTNUM`, `CAMNUM`, `CROPID`, `DIRYEAR`, `DIRMON`, `DIRDAY`, `COUNTER`, `CAMMODE`, `CAMGAIN`, `CAMOFFS`, `BINX`, `BINY`, `NCOADD`, `MINCOADD`, `N_STARS`, `AST_NSRC`, `PSF_NST`, `PSF_NPK`, `PH_NSRC`, `PT_DOF`, `PT_NCALI`, `PT_1_F1`, `PT_2_F1`, `PT_2_F2`, `PT_3_F1`, `PT_3_F2`, `PT_3_F3`, `PT_4_F1`, `PT_4_F2`, `PT_5_F1`, `PT_5_F2`, `PT_5_F3`, `PT_6_F1`, `PT_6_F2`, `PT_6_F3`, `PT_6_F4`, `PT_7_F1`, `PT_7_F2`, `PT_7_F3`, `PT_7_F4`, `PT_7_F5`, `PT_7_F6`, `PT_7_F7`, `PT_7_F8`, `PT_7_F9`, `PT_8_F1`, `PT_8_F2`, `PT_8_F3`, `PT_8_F4`, `PT_P_F1`, `PT_P_F2`, `PT_P_F3`, `PT_P_F4`, `APCOR_N` | `UInt8`/`UInt16`/`UInt32` | The pipeline holds these whole numbers as MATLAB `double`, so the header writer gives them a trailing point (`1.`), and writes a blank card when the value is NaN (e.g. failed photometric calibration). Convert to an integer; blank → 0. (`BCKMTHD`, `VARMTHD` and `PSTATUS` are set as integer types and written as integers already.) |
| All `Float32`/`Float64` columns | — | Blank values (empty card) → `nan`. Seen in the sample: `PSF_PKR` (1211), `PSF_DPK` (1211), `RP_MRMS` (14), `RP_MMRMS` (14), `PT_ARMS` (10), `PT_RMS` (1), `PT_CHI2` (1), `PT_ZP` (1) and 69 more keywords, all blank in the same failed-calibration sub-image. |
| All columns | — | A keyword missing from the header (e.g. in older products, or `OPT_CLN`, see note 4) → the column default. |

### Notes on specific choices

1. **`PT_` transmission model.** All `PT_` keywords are kept, including the model names (`PT_k_N`), the fixed inputs and the fit flags (`PT_k_Fn`), so the full calibration model of each image can be rebuilt from the table. Four values are fitted in production: `PT_1_V1` (normalisation), `PT_3_V2` (aerosol), `PT_5_V2` (water vapour) and `PT_8_V2` (detector QE centre), plus the spatial terms `PT_P_V2..V4`. Some inputs repeat other columns: `PT_2_V1` is the zenith angle and `PT_6_V2` is `MNTTEMP`.
2. **Full WCS (`CRPIX`, `CRVAL`, `CD`, `PV`, 30 columns) is marked ✅.** With it, the WCS of any sub-image can be rebuilt from the table alone. If we only need footprints, the corner coordinates (`RA1..4`/`DEC1..4`, `RAU*`/`DECU*`) are enough, and these 30 columns can be dropped.
3. **Mount coordinates are `Float32`.** At RA ≈ 250° a 4-byte number resolves about 0.1″, which is far below the pointing accuracy. The apparent mount coordinates (`M_ARA`, `M_AHA`, `M_ADEC`, `M_ADRA`, `M_ADHA`, `M_ADDEC`) are kept because they are needed for polar alignment. The sub-image centre `RA`/`DEC` and the corners are `Float64`, because they may be used for position searches.
4. **`OPT_CLN`** (date of the last optics cleaning, from the camera configuration) is in every raw image of LAST.01.03.01 that night, but in only 1 of that camera's 600 coadd headers. It is kept, with NaN as the default; why the coadd header loses it will be checked separately.
5. **`PSF_PKR`/`PSF_DPK`** are blank unless the PSF has more than one peak (13 of 1224 sub-images in the sample). They are kept because, together with `PSF_NPK`/`PSF_RPK`, they flag distorted (e.g. donut) PSFs.

### Differences from the 2024 design sheet

- **New:** 192 of the kept keywords are not in the 2024 sheet. They were mostly added to the headers since then: PSF quality, absolute photometry (`PT_*`, `APC*`), image IDs, source counts and the processing status mask.
- **Dropped:** 5 columns of the 2024 sheet are ❌ here: `NAXIS1/2`, `CCDID`, `CTYPE1` and `PH_MAGTE`. The 2024 sheet has a single column `ctype`, filled from `CTYPE1` with the projection name only (`TPV`, the same for both axes); here `CTYPE1` and `CTYPE2` are both ❌ because their values never change (`RA---TPV`, `DEC--TPV`).
- **Precision:** the 2024 sheet stores `CRVAL`, `CD`, `PV` and the corner coordinates as `float32`. At RA ≈ 250° a `float32` resolves only about 0.05″. That is comparable to the astrometric RMS (`AST_ARMS` 0.01–0.05″ in the sample), so `Float64` is proposed for these columns.

### Indexes

Draft for @ChenTishler to approve or edit. `discussion2sql` reads this table into `CREATE TABLE` and into an ALTER script for a table that already exists (types: `minmax`, `set(N)`, `bloom_filter`, `tokenbf_v1`, `ngrambf_v1`; granularity in blocks of 8192 rows; an empty table cell means this table).

| Index | Expression | Type | Granularity | Table | Purpose |
|---|---|---|---|---|---|
| `idx_jd` | `jd` | `minmax` | 1 | | time range |
| `idx_jd_start` | `jd_start` | `minmax` | 1 | | time range |
| `idx_midjd` | `midjd` | `minmax` | 1 | | time range |
| `idx_minjd` | `minjd` | `minmax` | 1 | | time range |
| `idx_maxjd` | `maxjd` | `minmax` | 1 | | time range |
| `idx_date_obs` | `date_obs` | `minmax` | 1 | | time range |
| `idx_ingest` | `ingestion_time_jd` | `minmax` | 1 | | rows added since a given time (copies, checks) |
| `idx_id_procf` | `id_procf` | `minmax` | 1 | | coadd of a given epoch image (`id_procf <= X AND id_procl >= X`) |
| `idx_id_procl` | `id_procl` | `minmax` | 1 | | coadd of a given epoch image |
| `idx_id_raw` | `id_raw` | `minmax` | 1 | | coadds of a given raw image |
| `idx_id_dark` | `id_dark` | `bloom_filter(0.01)` | 1 | | coadds made with a given dark |
| `idx_id_flat` | `id_flat` | `bloom_filter(0.01)` | 1 | | coadds made with a given flat |
| `idx_fieldid` | `fieldid` | `bloom_filter(0.01)` | 1 | | one field |
| `idx_object` | `object` | `bloom_filter(0.01)` | 1 | | one target |
| `idx_upix_par` | `upix_par` | `bloom_filter(0.01)` | 1 | | one sky partition (see question 1) |
| `idx_pipever` | `pipever` | `set(16)` | 1 | | one pipeline version |

### Index choices by column

The table is sorted by `id_coadd`, whose highest bits are the coadd time (`MIDJD`, checked on all 4920 rows in `last_test`; `ID_PROCF`, `ID_PROCL` and `ID_RAW` have the same layout, `ID_DARK` and `ID_FLAT` carry the time of the calibration image). So each block of 8192 rows holds about 1.3 hours of all cameras. A skip index helps only for a column whose values are bunched within such a block: time columns and the time-ordered IDs (`minmax`), and values that occur in few blocks, such as one field or one flat (`bloom_filter`). The table is small (about 8 million rows, 1000 blocks per year), so a query on a column without an index reads the whole column in a fraction of a second.

**Measured** on `last.visit_images` (v0 coadds of all cameras, last year, 1455 blocks, sorted by time; per block: 37 cameras, 69 fields, 77 `upix_par` values, 96° in Dec):

| Query | Rows selected | Blocks to read |
|---|---|---|
| one field (`fieldid`) | — | 3.7% (90% of fields: < 12%) |
| one flat (`id_flat`) | — | 1.2% |
| one sky partition (`upix_par`) | — | 13% (90%: < 27%) |
| Dec 10°–20° | 13% | 99% |
| FWHM < 2.5 | 0.9% | 66% |
| LIMMAG > 19.5 | 68% | 77% |
| airmass < 1.1 | 33% | 98% |

**Columns without an index** (every ✅ column not in the table above):

| Columns | Why no index |
|---|---|
| **ID, sorting key**: `id_coadd` | The sorting key itself (primary index). |
| **Camera, mount, node, crop**: `projname`, `nodenumb`, `mountnum`, `camnum`, `cropid`, `camname` | Every block holds all cameras (37 per block measured), so an index cannot skip blocks. Per-camera queries read the whole table, which is cheap (question 2). |
| **Other time columns**: `subdir`, `diryear`, `dirmon`, `dirday`, `filetime`, `time`, `lst`, `crdate` | `lst` repeats every day; `crdate` and `filetime`/`time` follow the reduction, not the observation; `diryear`/`dirmon`/`dirday`/`subdir` follow `jd`, use `jd`. |
| **File name**: `filename` | Kept for reference only: it is built from `projname`, `filetime`, `filter`, `fieldid`, `counter` and `cropid`. To find the row of one file, select on `projname` and `filetime`. |
| **Sky position**: `m_ra`, `m_ha`, `m_dec`, `m_az`, `m_alt`, `m_jra`, `m_jdec`, `m_ara`, `m_aha`, `m_adec`, `m_adra`, `m_adha`, `m_addec`, `ra`, `dec`, `upix_low`, `upix_hig`, `ra1`, `ra2`, `ra3`, `ra4`, `dec1`, `dec2`, `dec3`, `dec4`, `rau1`, `rau2`, `rau3`, `rau4`, `decu1`, `decu2`, `decu3`, `decu4` | Each block covers about 96° in Dec and 144° in RA: a 10° Dec band still reads 99% of the blocks. Use `upix_par` (question 1). |
| **Constant or few values (versions, methods, catalogues, settings, image sections, model names and flags)**: 126 columns: `version`, `gitastrp`, `git_camr`, `gitfocus`, `bias_im`, `flat_im`, `pstatus`, `schedulr`, `exptime`, `expmode`, `counter`, `filter`, `obslon`, `obslat`, `obsalt`, `trk_ra`, `trk_dec`, `opt_cln`, `camtemp`, `camcool`, `cammode`, `camgain`, `camoffs`, `origgain`, `darkcur`, `saturval`, `nonlin`, `binx`, `biny`, `lightsec_xmin`, `lightsec_xmax`, `lightsec_ymin`, `lightsec_ymax`, `overscan_xmin`, `overscan_xmax`, `overscan_ymin`, `overscan_ymax`, `ccdsec_xmin`, `ccdsec_xmax`, `ccdsec_ymin`, `ccdsec_ymax`, `origsec_xmin`, `origsec_xmax`, `origsec_ymin`, `origsec_ymax`, `origusec_xmin`, `origusec_xmax`, `origusec_ymin`, `origusec_ymax`, `uniqsec_xmin`, `uniqsec_xmax`, `uniqsec_ymin`, `uniqsec_ymax`, `exclsec_xmin`, `exclsec_xmax`, `exclsec_ymin`, `exclsec_ymax`, `origesec_xmin`, `origesec_xmax`, `origesec_ymin`, `origesec_ymax`, `ncoadd`, `coaddop`, `mincoadd`, `bckmthd`, `varmthd`, `ast_cat`, `psf_npk`, `psf_rpk`, `ph_magsy`, `ph_magt`, `ph_cat`, `pt_normu`, `pt_aref`, `pt_spec`, `pt_cat`, `pt_refsl`, `pt_refpv`, `pt_refc`, `pt_co2pp`, `pt_1_n`, `pt_1_f1`, `pt_2_n`, `pt_2_f1`, `pt_2_f2`, `pt_3_n`, `pt_3_f1`, `pt_3_f2`, `pt_3_f3`, `pt_4_n`, `pt_4_f1`, `pt_4_f2`, `pt_5_n`, `pt_5_f1`, `pt_5_f2`, `pt_5_f3`, `pt_6_n`, `pt_6_f1`, `pt_6_f2`, `pt_6_f3`, `pt_6_f4`, `pt_7_n`, `pt_7_f1`, `pt_7_f2`, `pt_7_f3`, `pt_7_f4`, `pt_7_f5`, `pt_7_f6`, `pt_7_f7`, `pt_7_f8`, `pt_7_f9`, `pt_8_n`, `pt_8_f1`, `pt_8_f2`, `pt_8_f3`, `pt_8_f4`, `pt_p_n`, `pt_p_nx1`, `pt_p_nx2`, `pt_p_ny1`, `pt_p_ny2`, `pt_p_f1`, `pt_p_f2`, `pt_p_f3`, `pt_p_f4`, `apcc_ref` | Few distinct values, mostly the same in every block, so an index cannot skip blocks. |
| **Measured values: image quality, background, PSF, WCS, calibration, temperatures, focus**: 159 columns: `airmass`, `mnttemp`, `focus`, `prvfocus`, `gain`, `readnoi`, `avncoadd`, `maxbckd`, `meanbck`, `medbck`, `stdbck`, `minbck`, `maxbck`, `meanvar`, `medvar`, `backmag`, `n_stars`, `mag_95q`, `m_chi2d`, `spatq50`, `spatq75`, `spatq90`, `spatq95`, `ast_nsrc`, `ast_arms`, `ast_errm`, `rp_mrms`, `rp_mmrms`, `pixscale`, `rotat`, `gm_ratex`, `gm_stdx`, `gm_ratey`, `gm_stdy`, `crpix1`, `crpix2`, `crval1`, `crval2`, `cd1_1`, `cd1_2`, `cd2_1`, `cd2_2`, `pv1_0`, `pv1_1`, `pv1_2`, `pv1_4`, `pv1_5`, `pv1_6`, `pv1_7`, `pv1_8`, `pv1_9`, `pv1_10`, `pv2_0`, `pv2_1`, `pv2_2`, `pv2_4`, `pv2_5`, `pv2_6`, `pv2_7`, `pv2_8`, `pv2_9`, `pv2_10`, `fwhe`, `fwhm`, `psf_fitn`, `psf_fita`, `psf_fitb`, `psf_fitt`, `psf_nst`, `psf_pkr`, `psf_dpk`, `psf_err`, `psf_s2`, `psf_af_1`, `psf_af_2`, `psf_af_3`, `psf_af_4`, `med_x2`, `med_y2`, `med_xy`, `snchi2`, `quicfwhm`, `ph_zp`, `ph_col1`, `ph_medc`, `ph_rms`, `ph_nsrc`, `pt_zp`, `pt_fnorm`, `limmag`, `limmager`, `pt_rms`, `pt_arms`, `pt_chi2`, `pt_dof`, `pt_ncali`, `pt_dzpab`, `pt_cta`, `pt_cta2`, `pt_cta3`, `pt_cta4`, `pt_ctae`, `pt_1_v1`, `pt_2_v1`, `pt_2_v2`, `pt_3_v1`, `pt_3_v2`, `pt_3_v3`, `pt_4_v1`, `pt_4_v2`, `pt_5_v1`, `pt_5_v2`, `pt_5_v3`, `pt_6_v1`, `pt_6_v2`, `pt_6_v3`, `pt_6_v4`, `pt_7_v1`, `pt_7_v2`, `pt_7_v3`, `pt_7_v4`, `pt_7_v5`, `pt_7_v6`, `pt_7_v7`, `pt_7_v8`, `pt_7_v9`, `pt_8_v1`, `pt_8_v2`, `pt_8_v3`, `pt_8_v4`, `pt_p_v1`, `pt_p_v2`, `pt_p_v3`, `pt_p_v4`, `apc0_a1`, `apcx_a1`, `apcy_a1`, `apcxy_a1`, `apcc_a1`, `apcce_a1`, `apc0_a2`, `apcx_a2`, `apcy_a2`, `apcxy_a2`, `apcc_a2`, `apcce_a2`, `apc0_a4`, `apcx_a4`, `apcy_a4`, `apcxy_a4`, `apcc_a4`, `apcce_a4`, `apc0_ps`, `apcx_ps`, `apcy_ps`, `apcxy_ps`, `apcc_ps`, `apcce_ps`, `apcor_n` | They do not follow time order: FWHM < 2.5 selects 0.9% of rows but 66% of the blocks; LIMMAG > 19.5 reads 77%, airmass < 1.1 reads 98%. |

**Questions for @ChenTishler:**
1. Sky position: the draft has a `bloom_filter` on `upix_par` (13% of the blocks for one partition). The alternative is a projection, a second copy of the table sorted by position, `SELECT * ORDER BY (upix_par, id_coadd)`, which reads about one block per partition and doubles the size of the table (a few GB per year).
2. Per-camera history (e.g. the FWHM of one camera over a year): no index helps; it reads the whole table (fast at this size). A projection sorted by `(nodenumb, mountnum, camnum, id_coadd)` would make it read only that camera, again at twice the size. Is it worth it?
3. Not yet tried on ClickHouse 22.1 (last0): `bloom_filter` on `LowCardinality(String)` columns and projections; both work on 26.6 (euclid).

