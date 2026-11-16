We want a new ClickHouse table, `image_proc`, that keeps the image header of every LAST v1 epoch image (one row per epoch sub-image, i.e. about 480 rows per visit: 20 epochs × 24 sub-images). The header is taken from the visit file `proc_Head_1.mat`, which holds all 20 × 24 epoch headers. Since #1417 the pipeline writes the epoch catalogs into MAT files instead of the 480 `proc_Cat` FITS files (production `lv1.1.2`, e.g. the night 2026-10-20 has none). For older visits the header is taken from the third layer (HDU 3) of their `proc_Cat` files; the content is the same. Below is a first proposal in the same format as #1409 (`image_visit`): for every header keyword (except HISTORY and COMMENT), whether to keep it as a column, its type, default value, the storage resolution it needs, and how many distinct values to expect. Please comment on the ✅/❌ choices.

**Related:** #1409 proposes the coadd image table `image_visit` and #1411 the source tables `src_visit` and `src_proc` in the same format. Once this proposal is settled, it will be used to create the actual ClickHouse table `image_proc`.

### What the proposal is based on

- **Example file:** `LAST.01.02.01_20260405.165513.627_clear_1351_001_001_001_sci_proc_Cat_1.fits`, HDU 3 (pipeline `lv1.0.5`).
- **Measured on:** all 12,216 `proc_Cat` files of the 26 evening visits (16:55–22:15 UT) of the night 2026-04-05 on cameras LAST.01.02.01 and LAST.01.03.01: 509 epochs. These are the same files as the #1411 sample.
- **Starting point:** the ✅/❌ choices, types, defaults and resolutions are those already agreed for `image_visit` in #1409. They are a starting point: which keywords are really needed for epochs is still to be decided.
- **Added on 2026-10-21:** the epoch headers of `lv1.1.2` (night 2026-10-20 on euclid) have the same four new keywords as the coadd header (#1409): `SCHEDULR` (one value per visit) and `QUICFWHM` (the camera software's quick FWHM estimate of each raw image, in pixels, so one value per epoch) from the raw image header, and `PT_FNORM`, `PT_NORMU` from the absolute calibration (#1431). Like the other `PT_` values, `PT_FNORM` of an epoch is the coadd's value, while `PT_1_V1` is the epoch's own, so in an epoch row `PT_FNORM` is not `PT_1_V1` divided by the telescope area. Older products do not have the four keywords and get the defaults.
- **Compared with the coadd header:** the epoch header has 326 keywords (330 since 2026-10-21), the coadd header 347 (352 since `MAXBCKD` and the four keywords above were added). The epoch header has no coadd-only keywords (`NCOADD`, `AVNCOADD`, `MINCOADD`, `COADDOP`, `SUBLEVEL`, `MINJD`, `MAXJD`, `MAXBCKD`, `ID_COADD`, `ID_PROCF`, `ID_PROCL`), and also lacks `SPATQ50`…`SPATQ95`, `SPATNBIN`, `RP_MRMS`, `RP_MMRMS`, `GM_RATEX`, `GM_RATEY`, `GM_STDX`, `GM_STDY`, `SNCHI2` and `OPT_CLN`. It has three extra keywords of the FITS extension layer (`XTENSION`, `PCOUNT`, `GCOUNT`).
- **Columns of the tables below:** as in #1409, plus one:
  - **Include:** ✅ keep as a column, ❌ leave out.
  - **Data type:** the proposed ClickHouse type.
  - **Default:** the value written when the keyword is missing or blank. NaN for real numbers, 0 for counts and IDs, an empty string for text.
  - **Resolution:** the smallest step in the value worth keeping (1 for whole numbers). It is not the ClickHouse index granularity.
  - **Cardinality:** how many different values to expect across the whole table. The number in brackets is the count of distinct values in the 12,216-header sample. *Per epoch* means one value per exposure (shared by its 24 sub-images); *per coadd sub-image* means one value for all epochs of the same visit and sub-image.
  - **Same as coadd:** ✅ when the value equals the one in the coadd header of the same visit and sub-image in every row of the sample (compared with the coadd header in HDU 3 of the `coadd_Cat` files). "✅ in N/12216" means equal except in rows where the epoch or the coadd header lacks the keyword. This column is meant to help decide which columns `image_proc` needs, since the coadd values can also be taken from `image_visit` through `id_coadd`.
  - **In 2024 sheet:** whether the existing design sheet (`Design-Database-Pipeline-ClickHouse.xlsx`, tab *Images*, `proc_images` column) already has this column, and with which name and type.
- **Column names:** as in #1409: the keyword in lower case, with `-` replaced by `_`. IDs and the `UPIX_*` values are `UInt64`; each image-section keyword (`*SEC`, `OVERSCAN`) becomes four `Int16` columns `<keyword>_xmin`, `_xmax`, `_ymin`, `_ymax`.
- **Size:** about 480 rows per visit × 1,100 visits per clear night (as in #1409) gives about 0.5 million rows per night, or about 1.6×10⁸ rows per year, 20 times `image_visit`. With about 323 columns (297 keywords, the image sections taking four columns each, plus `ingestion_time_jd` and `id_coadd`) that is some tens of GB per year after compression.

### Sorting and indexes

Proposed, as for `image_visit`: the table is sorted by `id_proc` (`ORDER BY id_proc`). The observation time sits in the highest bits of the ID, so this order is close to time order. Additional indexes will be defined by @ChenTishler.

### Keyword table

Click a group to open it.

<details><summary><b>FITS structure</b> (0 of 11 kept)</summary>

| Keyword | Include | Data type | Default | Resolution | Cardinality | Same as coadd | In 2024 sheet |
|---|:---:|---|---|---|---|:---:|---|
| `XTENSION` | ❌ | `LowCardinality(String)` | '' | — | constant [1] | ✅ | ❌ |
| `PCOUNT` | ❌ | `UInt8` | 0 | — | constant [1] | ✅ | ❌ |
| `GCOUNT` | ❌ | `UInt8` | 0 | — | constant [1] | ✅ | ❌ |
| `BITPIX` | ❌ | `Int8` | 0 | — | constant [1] | ✅ | ❌ |
| `NAXIS` | ❌ | `UInt8` | 0 | — | constant [1] | ✅ | ❌ |
| `NAXIS1` | ❌ | `UInt16` | 0 | 1 pix | constant [1] | ✅ | ✅ `int16` |
| `NAXIS2` | ❌ | `UInt16` | 0 | 1 pix | constant [1] | ✅ | ✅ `int16` |
| `EXTEND` | ❌ | `UInt8` | 0 | — | constant [1] | ✅ | ❌ |
| `BZERO` | ❌ | `Float32` | nan | — | constant [1] | ✅ | ❌ |
| `BSCALE` | ❌ | `Float32` | nan | — | constant [1] | ✅ | ❌ |
| `LONGSTRN` | ❌ | `LowCardinality(String)` | '' | — | constant [1] | ✅ | ❌ |

</details>

<details><summary><b>Identification, files and versions</b> (17 of 28 kept)</summary>

| Keyword | Include | Data type | Default | Resolution | Cardinality | Same as coadd | In 2024 sheet |
|---|:---:|---|---|---|---|:---:|---|
| `FILENAME` | ❌ | `String` | '' | full name | per row (sub-image) [12216] | — | ❌ |
| `PROJNAME` | ❌ | `LowCardinality(String)` | '' | — | per camera (≤44) [2] | ✅ | ❌ |
| `FULLPROJ` | ❌ | `LowCardinality(String)` | '' | — | per camera (≤44) [2] | ✅ | ❌ |
| `NODENUMB` | ✅ | `UInt8` | 0 | 1 | constant [1] | ✅ | ✅ `int16` |
| `MOUNTNUM` | ✅ | `UInt8` | 0 | 1 | per mount (≤11) [2] | ✅ | ✅ `int8` |
| `CAMNUM` | ✅ | `UInt8` | 0 | 1 | low (<100) [1] | ✅ | ✅ `int8` |
| `CROPID` | ✅ | `UInt8` | 0 | 1 | per coadd sub-image [24] | ✅ | ✅ `int16` |
| `CCDID` | ❌ | `UInt8` | 0 | 1 | constant [1] | ✅ | ✅ `int16` |
| `LEVEL` | ❌ | `LowCardinality(String)` | '' | — | constant [1] | — | ❌ |
| `IMTYPE` | ❌ | `LowCardinality(String)` | '' | — | constant [1] | ✅ | ❌ |
| `VERSION` | ✅ | `UInt8` | 0 | 1 | low (<100) [1] | ✅ | ❌ |
| `SUBDIR` | ❌ | `String` | '' | — | per visit [26] | ✅ | ✅ `char16` |
| `DIRYEAR` | ❌ | `UInt16` | 0 | 1 | low (<100) [1] | ✅ | ✅ `int16` |
| `DIRMON` | ❌ | `UInt8` | 0 | 1 | low (<100) [1] | ✅ | ✅ `int8` |
| `DIRDAY` | ❌ | `UInt8` | 0 | 1 | low (<100) [1] | ✅ | ✅ `int8` |
| `FILETIME` | ❌ | `String` | '' | 1 ms | per epoch [509] | — | ✅ `string` |
| `TIME` | ✅ | `String` | '' | 1 ms | per epoch [509] | — | ❌ |
| `PIPEVER` | ✅ | `LowCardinality(String)` | '' | — | low (<100) [1] | ✅ | ✅ `char32` |
| `GITASTRP` | ✅ | `LowCardinality(String)` | '' | — | low (<100) [1] | ✅ | ❌ |
| `GIT_CAMR` | ✅ | `LowCardinality(String)` | '' | — | low (<100) [1] | ✅ | ❌ |
| `GITFOCUS` | ✅ | `LowCardinality(String)` | '' | — | low (<100) [1] | ✅ | ❌ |
| `ID_PROC` | ✅ | `UInt64` | 0 | 1 | per row (sub-image) [12216] | — | ✅ `int64` |
| `ID_RAW` | ✅ | `UInt64` | 0 | 1 | per epoch [509] | — | ✅ `int64` |
| `BIAS_IM` | ✅ | `LowCardinality(String)` | '' | — | hundreds–few thousand [2] | ✅ | ❌ |
| `ID_DARK` | ✅ | `UInt64` | 0 | 1 | hundreds–few thousand [2] | ✅ | ✅ `int64` |
| `FLAT_IM` | ✅ | `LowCardinality(String)` | '' | — | hundreds–few thousand [2] | ✅ | ❌ |
| `ID_FLAT` | ✅ | `UInt64` | 0 | 1 | hundreds–few thousand [2] | ✅ | ✅ `int64` |
| `PSTATUS` | ✅ | `UInt32` | 0 | bit mask | low (<100) [3] | ✅ in 12213/12216 | ❌ |

</details>

<details><summary><b>Target and observation time</b> (14 of 14 kept)</summary>

| Keyword | Include | Data type | Default | Resolution | Cardinality | Same as coadd | In 2024 sheet |
|---|:---:|---|---|---|---|:---:|---|
| `OBJECT` | ✅ | `LowCardinality(String)` | '' | — | hundreds–few thousand [14] | ✅ | ✅ `char16` |
| `FIELDID` | ✅ | `LowCardinality(String)` | '' | — | hundreds–few thousand [14] | — | ✅ `char16` |
| `SCHEDULR` | ✅ | `LowCardinality(String)` | '' | — | low (<100) [new, not in the sample] | ✅ | ❌ |
| `DATE-OBS` | ✅ | `DateTime64(3,'UTC')` | 0 (1970-01-01) | 1 ms | per epoch [509] | — | ✅ dateobs `DateTime64(3)` |
| `JD` | ✅ | `Float64` | nan | 1e-8 d (≈1 ms) | per epoch [509] | — | ✅ jd_start `float64` |
| `JD_START` | ✅ | `Float64` | nan | 1e-8 d | per visit [26] | ✅ | ❌ |
| `MIDJD` | ✅ | `Float64` | nan | 1e-8 d | per epoch [509] | — | ✅ `float64` |
| `LST` | ✅ | `Float32` | nan | 1e-4 deg | per epoch [509] | — | ✅ `float64` |
| `CRDATE` | ✅ | `DateTime64(3,'UTC')` | 0 (1970-01-01) | 1 ms | per epoch [509] | — | ❌ |
| `EXPTIME` | ✅ | `Float32` | nan | 1 ms | low (<100) [1] | — | ✅ `float32` |
| `EXPMODE` | ✅ | `LowCardinality(String)` | '' | — | low (<100) [1] | ✅ | ❌ |
| `COUNTER` | ✅ | `UInt32` | 0 | 1 | low (<100) [20] | — | ✅ `int16` |
| `FILTER` | ✅ | `LowCardinality(String)` | '' | — | low (<100) [1] | ✅ | ✅ `char8` |
| `AIRMASS` | ✅ | `Float32` | nan | 1e-4 | per row (sub-image) [12216] | — | ✅ `float32` |

</details>

<details><summary><b>Observatory and mount</b> (21 of 23 kept)</summary>

| Keyword | Include | Data type | Default | Resolution | Cardinality | Same as coadd | In 2024 sheet |
|---|:---:|---|---|---|---|:---:|---|
| `TIMEZONE` | ❌ | `Float32` | nan | — | constant [1] | ✅ | ❌ |
| `OBSLON` | ✅ | `Float64` | nan | 1e-6 deg | per mount (≤11) [2] | ✅ | ✅ `float32` |
| `OBSLAT` | ✅ | `Float64` | nan | 1e-6 deg | per mount (≤11) [2] | ✅ | ✅ `float32` |
| `OBSALT` | ✅ | `Float32` | nan | 0.1 m | per mount (≤11) [1] | ✅ | ✅ `float32` |
| `EQUINOX` | ❌ | `Float32` | nan | — | constant [1] | ✅ | ❌ |
| `M_RA` | ✅ | `Float32` | nan | 1e-4 deg | per epoch [509] | — | ✅ `float32` |
| `M_HA` | ✅ | `Float32` | nan | 1e-4 deg | per epoch [509] | — | ✅ `float32` |
| `M_DEC` | ✅ | `Float32` | nan | 1e-4 deg | per visit [26] | ✅ | ✅ `float32` |
| `M_AZ` | ✅ | `Float32` | nan | 1e-4 deg | per epoch [509] | — | ❌ |
| `M_ALT` | ✅ | `Float32` | nan | 1e-4 deg | per epoch [509] | — | ❌ |
| `M_JRA` | ✅ | `Float32` | nan | 1e-4 deg | per epoch [509] | — | ✅ `float32` |
| `M_JDEC` | ✅ | `Float32` | nan | 1e-4 deg | per visit [26] | ✅ | ✅ `float32` |
| `M_ARA` | ✅ | `Float32` | nan | 1e-4 deg | per epoch [509] | — | ✅ `float32` |
| `M_AHA` | ✅ | `Float32` | nan | 1e-4 deg | per epoch [509] | — | ✅ `float32` |
| `M_ADEC` | ✅ | `Float32` | nan | 1e-4 deg | per visit [26] | ✅ | ✅ `float32` |
| `M_ADRA` | ✅ | `Float32` | nan | 1e-4 deg | per epoch [509] | — | ✅ `float32` |
| `M_ADHA` | ✅ | `Float32` | nan | 1e-4 deg | per epoch [509] | — | ✅ `float32` |
| `M_ADDEC` | ✅ | `Float32` | nan | 1e-4 deg | per visit [26] | ✅ | ✅ `float32` |
| `TRK_RA` | ✅ | `Float32` | nan | 1e-6 arcsec/s | low (<100) [1] | ✅ | ✅ `float32` |
| `TRK_DEC` | ✅ | `Float32` | nan | 1e-6 arcsec/s | low (<100) [1] | ✅ | ✅ `float32` |
| `MNTTEMP` | ✅ | `Float32` | nan | 0.05 °C | per visit [17] | ✅ | ✅ temp_mnt `float32` |
| `FOCUS` | ✅ | `Float32` | nan | 1 step | hundreds–few thousand [6] | ✅ | ✅ `Int32` |
| `PRVFOCUS` | ✅ | `Float32` | nan | 1 step | hundreds–few thousand [6] | ✅ | ✅ `Int32` |

</details>

<details><summary><b>Camera and detector</b> (22 of 22 kept)</summary>

| Keyword | Include | Data type | Default | Resolution | Cardinality | Same as coadd | In 2024 sheet |
|---|:---:|---|---|---|---|:---:|---|
| `CAMNAME` | ✅ | `LowCardinality(String)` | '' | — | per camera (≤44) [2] | ✅ | ❌ |
| `CAMTEMP` | ✅ | `Float32` | nan | 0.1 °C | low (<100) [17] | — | ❌ |
| `CAMCOOL` | ✅ | `Float32` | nan | 0.1 % | low (<100) [17] | — | ✅ `float32` |
| `CAMMODE` | ✅ | `UInt8` | 0 | 1 | low (<100) [1] | ✅ | ❌ |
| `CAMGAIN` | ✅ | `UInt16` | 0 | 1 | low (<100) [1] | ✅ | ✅ `float32` |
| `CAMOFFS` | ✅ | `UInt16` | 0 | 1 ADU | low (<100) [1] | ✅ | ✅ `float32` |
| `GAIN` | ✅ | `Float32` | nan | 1e-5 e/ADU | per row (sub-image) [1] | — | ❌ |
| `ORIGGAIN` | ✅ | `Float32` | nan | 1e-3 | constant [1] | ✅ | ✅ `float32` |
| `READNOI` | ✅ | `Float32` | nan | 1e-4 e | per row (sub-image) [1] | — | ✅ `float32` |
| `DARKCUR` | ✅ | `Float32` | nan | 1e-4 e/s | low (<100) [1] | ✅ | ✅ `float32` |
| `SATURVAL` | ✅ | `Float32` | nan | 1 ADU | low (<100) [1] | ✅ | ✅ `float32` |
| `NONLIN` | ✅ | `Float32` | nan | 1 ADU | low (<100) [1] | ✅ | ✅ `float32` |
| `BINX` | ✅ | `UInt8` | 0 | 1 | constant [1] | ✅ | ✅ `float32` |
| `BINY` | ✅ | `UInt8` | 0 | 1 | constant [1] | ✅ | ✅ `float32` |
| `LIGHTSEC` | ✅ | 4 × `Int16`: `lightsec_xmin`, `_xmax`, `_ymin`, `_ymax` | 0 | 1 pix | constant [1] | ✅ | ❌ |
| `OVERSCAN` | ✅ | 4 × `Int16`: `overscan_xmin`, `_xmax`, `_ymin`, `_ymax` | 0 | 1 pix | constant [1] | ✅ | ❌ |
| `CCDSEC` | ✅ | 4 × `Int16`: `ccdsec_xmin`, `_xmax`, `_ymin`, `_ymax` | 0 | 1 pix | constant [1] | ✅ | ❌ |
| `ORIGSEC` | ✅ | 4 × `Int16`: `origsec_xmin`, `_xmax`, `_ymin`, `_ymax` | 0 | 1 pix | per coadd sub-image [24] | ✅ | ✅ split into 4 `int16` |
| `ORIGUSEC` | ✅ | 4 × `Int16`: `origusec_xmin`, `_xmax`, `_ymin`, `_ymax` | 0 | 1 pix | per coadd sub-image [24] | ✅ | ✅ split into 4 `int16` |
| `UNIQSEC` | ✅ | 4 × `Int16`: `uniqsec_xmin`, `_xmax`, `_ymin`, `_ymax` | 0 | 1 pix | per coadd sub-image [12] | ✅ | ✅ split into 4 `int16` |
| `EXCLSEC` | ✅ | 4 × `Int16`: `exclsec_xmin`, `_xmax`, `_ymin`, `_ymax` | 0 | 1 pix | per coadd sub-image [12] | ✅ | ❌ |
| `ORIGESEC` | ✅ | 4 × `Int16`: `origesec_xmin`, `_xmax`, `_ymin`, `_ymax` | 0 | 1 pix | per coadd sub-image [24] | ✅ | ❌ |

</details>

<details><summary><b>Background and image statistics</b> (13 of 13 kept)</summary>

| Keyword | Include | Data type | Default | Resolution | Cardinality | Same as coadd | In 2024 sheet |
|---|:---:|---|---|---|---|:---:|---|
| `MEANBCK` | ✅ | `Float32` | nan | 0.01 e | per row (sub-image) [12048] | — | ✅ `float32` |
| `MEDBCK` | ✅ | `Float32` | nan | 0.01 e | per row (sub-image) [12052] | — | ✅ `float32` |
| `STDBCK` | ✅ | `Float32` | nan | 1e-3 e | per row (sub-image) [12204] | — | ✅ `float32` |
| `MINBCK` | ✅ | `Float32` | nan | 0.01 e | per row (sub-image) [12048] | — | ❌ |
| `MAXBCK` | ✅ | `Float32` | nan | 0.01 e | per row (sub-image) [12071] | — | ❌ |
| `MEANVAR` | ✅ | `Float32` | nan | 1e-3 e² | per row (sub-image) [12048] | — | ✅ `float32` |
| `MEDVAR` | ✅ | `Float32` | nan | 1e-3 e² | per row (sub-image) [12052] | — | ✅ `float32` |
| `BCKMTHD` | ✅ | `UInt8` | 0 | 1 | constant [1] | ✅ | ❌ |
| `VARMTHD` | ✅ | `UInt8` | 0 | 1 | constant [1] | ✅ | ❌ |
| `BACKMAG` | ✅ | `Float32` | nan | 1e-3 mag | per row (sub-image) [10614] | — | ✅ `float32` |
| `N_STARS` | ✅ | `UInt32` | 0 | 1 | per row (sub-image) [934] | — | ❌ |
| `MAG_95Q` | ✅ | `Float32` | nan | 1e-3 mag | per row (sub-image) [12189] | — | ❌ |
| `M_CHI2D` | ✅ | `Float32` | nan | 1e-3 | per row (sub-image) [12196] | — | ❌ |

</details>

<details><summary><b>Astrometry and sky footprint</b> (27 of 27 kept)</summary>

| Keyword | Include | Data type | Default | Resolution | Cardinality | Same as coadd | In 2024 sheet |
|---|:---:|---|---|---|---|:---:|---|
| `RA` | ✅ | `Float64` | nan | 1e-7 deg | per row (sub-image) [12216] | — | ✅ `float64` |
| `DEC` | ✅ | `Float64` | nan | 1e-7 deg | per row (sub-image) [12216] | — | ✅ `float64` |
| `UPIX_PAR` | ✅ | `UInt64` | 0 | 1 | hundreds–few thousand [21] | ✅ in 12214/12216 | ✅ upix_partition `uint64` |
| `UPIX_LOW` | ✅ | `UInt64` | 0 | 1 | per row (sub-image) [355] | — | ✅ `uint64` |
| `UPIX_HIG` | ✅ | `UInt64` | 0 | 1 | per row (sub-image) [2865] | — | ✅ upix_high `uint64` |
| `AST_NSRC` | ✅ | `UInt16` | 0 | 1 | per row (sub-image) [112] | — | ✅ `Int32` |
| `AST_ARMS` | ✅ | `Float32` | nan | 1e-5 arcsec | per row (sub-image) [12216] | — | ✅ `float32` |
| `AST_ERRM` | ✅ | `Float32` | nan | 1e-6 arcsec | per row (sub-image) [12216] | — | ✅ `float32` |
| `AST_CAT` | ✅ | `LowCardinality(String)` | '' | — | constant [1] | ✅ | ❌ |
| `PIXSCALE` | ✅ | `Float32` | nan | 1e-5 arcsec | per row (sub-image) [12216] | — | ❌ |
| `ROTAT` | ✅ | `Float32` | nan | 1e-4 deg | per row (sub-image) [12216] | — | ❌ |
| `RA1` | ✅ | `Float64` | nan | 1e-7 deg | per row (sub-image) [12216] | — | ✅ `float32` |
| `RA2` | ✅ | `Float64` | nan | 1e-7 deg | per row (sub-image) [12216] | — | ✅ `float32` |
| `RA3` | ✅ | `Float64` | nan | 1e-7 deg | per row (sub-image) [12216] | — | ✅ `float32` |
| `RA4` | ✅ | `Float64` | nan | 1e-7 deg | per row (sub-image) [12216] | — | ✅ `float32` |
| `DEC1` | ✅ | `Float64` | nan | 1e-7 deg | per row (sub-image) [12216] | — | ✅ `float32` |
| `DEC2` | ✅ | `Float64` | nan | 1e-7 deg | per row (sub-image) [12216] | — | ✅ `float32` |
| `DEC3` | ✅ | `Float64` | nan | 1e-7 deg | per row (sub-image) [12216] | — | ✅ `float32` |
| `DEC4` | ✅ | `Float64` | nan | 1e-7 deg | per row (sub-image) [12216] | — | ✅ `float32` |
| `RAU1` | ✅ | `Float64` | nan | 1e-7 deg | per row (sub-image) [12216] | — | ✅ `float32` |
| `RAU2` | ✅ | `Float64` | nan | 1e-7 deg | per row (sub-image) [12216] | — | ✅ `float32` |
| `RAU3` | ✅ | `Float64` | nan | 1e-7 deg | per row (sub-image) [12216] | — | ✅ `float32` |
| `RAU4` | ✅ | `Float64` | nan | 1e-7 deg | per row (sub-image) [12216] | — | ✅ `float32` |
| `DECU1` | ✅ | `Float64` | nan | 1e-7 deg | per row (sub-image) [12216] | — | ✅ `float32` |
| `DECU2` | ✅ | `Float64` | nan | 1e-7 deg | per row (sub-image) [12216] | — | ✅ `float32` |
| `DECU3` | ✅ | `Float64` | nan | 1e-7 deg | per row (sub-image) [12216] | — | ✅ `float32` |
| `DECU4` | ✅ | `Float64` | nan | 1e-7 deg | per row (sub-image) [12216] | — | ✅ `float32` |

</details>

<details><summary><b>WCS solution</b> (28 of 36 kept)</summary>

| Keyword | Include | Data type | Default | Resolution | Cardinality | Same as coadd | In 2024 sheet |
|---|:---:|---|---|---|---|:---:|---|
| `CTYPE1` | ❌ | `LowCardinality(String)` | '' | — | constant [1] | ✅ | ✅ ctype `char8` |
| `CTYPE2` | ❌ | `LowCardinality(String)` | '' | — | constant [1] | ✅ | ❌ |
| `WCSAXES` | ❌ | `UInt8` | 0 | — | constant [1] | ✅ | ❌ |
| `RADESYS` | ❌ | `LowCardinality(String)` | '' | — | constant [1] | ✅ | ❌ |
| `LONPOLE` | ❌ | `Float32` | nan | — | constant [1] | ✅ | ❌ |
| `LATPOLE` | ❌ | `Float32` | nan | — | constant [1] | ✅ | ❌ |
| `CUNIT1` | ❌ | `LowCardinality(String)` | '' | — | constant [1] | ✅ | ❌ |
| `CUNIT2` | ❌ | `LowCardinality(String)` | '' | — | constant [1] | ✅ | ❌ |
| `CRPIX1` | ✅ | `Float64` | nan | 1e-6 pix | per row (sub-image) [12216] | — | ✅ `float32` |
| `CRPIX2` | ✅ | `Float64` | nan | 1e-6 pix | per row (sub-image) [12216] | — | ✅ `float32` |
| `CRVAL1` | ✅ | `Float64` | nan | 1e-9 deg | per row (sub-image) [12216] | — | ✅ `float32` |
| `CRVAL2` | ✅ | `Float64` | nan | 1e-9 deg | per row (sub-image) [12216] | — | ✅ `float32` |
| `CD1_1` | ✅ | `Float64` | nan | full (1e-12) | per row (sub-image) [12216] | — | ✅ `float32` |
| `CD1_2` | ✅ | `Float64` | nan | full (1e-12) | per row (sub-image) [12216] | — | ✅ `float32` |
| `CD2_1` | ✅ | `Float64` | nan | full (1e-12) | per row (sub-image) [12216] | — | ✅ `float32` |
| `CD2_2` | ✅ | `Float64` | nan | full (1e-12) | per row (sub-image) [12216] | — | ✅ `float32` |
| `PV1_0` | ✅ | `Float64` | nan | full (1e-12) | per row (sub-image) [12216] | — | ✅ `float32` |
| `PV1_1` | ✅ | `Float64` | nan | full (1e-12) | per row (sub-image) [12216] | — | ✅ `float32` |
| `PV1_2` | ✅ | `Float64` | nan | full (1e-12) | per row (sub-image) [12216] | — | ✅ `float32` |
| `PV1_4` | ✅ | `Float64` | nan | full (1e-12) | per row (sub-image) [12216] | — | ✅ `float32` |
| `PV1_5` | ✅ | `Float64` | nan | full (1e-12) | per row (sub-image) [12216] | — | ✅ `float32` |
| `PV1_6` | ✅ | `Float64` | nan | full (1e-12) | per row (sub-image) [12216] | — | ✅ `float32` |
| `PV1_7` | ✅ | `Float64` | nan | full (1e-12) | per row (sub-image) [12216] | — | ✅ `float32` |
| `PV1_8` | ✅ | `Float64` | nan | full (1e-12) | per row (sub-image) [12216] | — | ✅ `float32` |
| `PV1_9` | ✅ | `Float64` | nan | full (1e-12) | per row (sub-image) [12216] | — | ✅ `float32` |
| `PV1_10` | ✅ | `Float64` | nan | full (1e-12) | per row (sub-image) [12216] | — | ✅ `float32` |
| `PV2_0` | ✅ | `Float64` | nan | full (1e-12) | per row (sub-image) [12216] | — | ✅ `float32` |
| `PV2_1` | ✅ | `Float64` | nan | full (1e-12) | per row (sub-image) [12216] | — | ✅ `float32` |
| `PV2_2` | ✅ | `Float64` | nan | full (1e-12) | per row (sub-image) [12216] | — | ✅ `float32` |
| `PV2_4` | ✅ | `Float64` | nan | full (1e-12) | per row (sub-image) [12216] | — | ✅ `float32` |
| `PV2_5` | ✅ | `Float64` | nan | full (1e-12) | per row (sub-image) [12216] | — | ✅ `float32` |
| `PV2_6` | ✅ | `Float64` | nan | full (1e-12) | per row (sub-image) [12216] | — | ✅ `float32` |
| `PV2_7` | ✅ | `Float64` | nan | full (1e-12) | per row (sub-image) [12216] | — | ✅ `float32` |
| `PV2_8` | ✅ | `Float64` | nan | full (1e-12) | per row (sub-image) [12216] | — | ✅ `float32` |
| `PV2_9` | ✅ | `Float64` | nan | full (1e-12) | per row (sub-image) [12216] | — | ✅ `float32` |
| `PV2_10` | ✅ | `Float64` | nan | full (1e-12) | per row (sub-image) [12216] | — | ✅ `float32` |

</details>

<details><summary><b>PSF</b> (21 of 21 kept)</summary>

| Keyword | Include | Data type | Default | Resolution | Cardinality | Same as coadd | In 2024 sheet |
|---|:---:|---|---|---|---|:---:|---|
| `FWHE` | ✅ | `Float32` | nan | 1e-3 arcsec | per row (sub-image) [12216] | — | ❌ |
| `FWHM` | ✅ | `Float32` | nan | 1e-3 arcsec | per row (sub-image) [12216] | — | ✅ `float32` |
| `PSF_FITN` | ✅ | `Float32` | nan | 1e-4 | per row (sub-image) [12216] | — | ❌ |
| `PSF_FITA` | ✅ | `Float32` | nan | 1e-4 | per row (sub-image) [12216] | — | ❌ |
| `PSF_FITB` | ✅ | `Float32` | nan | 1e-4 | per row (sub-image) [12216] | — | ❌ |
| `PSF_FITT` | ✅ | `Float32` | nan | 0.01 deg | per row (sub-image) [12216] | — | ❌ |
| `PSF_NST` | ✅ | `UInt16` | 0 | 1 | per row (sub-image) [98] | — | ❌ |
| `PSF_NPK` | ✅ | `UInt8` | 0 | 1 | low (<100) [13] | — | ❌ |
| `PSF_PKR` | ✅ | `Float32` | nan | 1e-3 | low (<100) [769] | — | ❌ |
| `PSF_DPK` | ✅ | `Float32` | nan | 1e-3 pix | low (<100) [31] | — | ❌ |
| `PSF_RPK` | ✅ | `Float32` | nan | 0.1 pix | low (<100) [1] | ✅ | ❌ |
| `PSF_ERR` | ✅ | `Float32` | nan | 1e-6 | per row (sub-image) [12189] | — | ❌ |
| `PSF_S2` | ✅ | `Float32` | nan | 1e-6 | per row (sub-image) [12198] | — | ❌ |
| `PSF_AF_1` | ✅ | `Float32` | nan | 1e-4 | per row (sub-image) [12216] | — | ❌ |
| `PSF_AF_2` | ✅ | `Float32` | nan | 1e-4 | per row (sub-image) [12216] | — | ❌ |
| `PSF_AF_3` | ✅ | `Float32` | nan | 1e-4 | per row (sub-image) [12216] | — | ❌ |
| `PSF_AF_4` | ✅ | `Float32` | nan | 1e-4 | per row (sub-image) [12216] | — | ❌ |
| `MED_X2` | ✅ | `Float32` | nan | 1e-4 pix² | per row (sub-image) [12216] | — | ❌ |
| `MED_Y2` | ✅ | `Float32` | nan | 1e-4 pix² | per row (sub-image) [12216] | — | ❌ |
| `MED_XY` | ✅ | `Float32` | nan | 1e-5 pix² | per row (sub-image) [12216] | — | ❌ |
| `QUICFWHM` | ✅ | `Float32` | nan | 1e-3 pix | per epoch [new, not in the sample] | — | ❌ |

</details>

<details><summary><b>Relative photometric calibration (PH_)</b> (8 of 9 kept)</summary>

| Keyword | Include | Data type | Default | Resolution | Cardinality | Same as coadd | In 2024 sheet |
|---|:---:|---|---|---|---|:---:|---|
| `PH_ZP` | ✅ | `Float32` | nan | 1e-4 mag | per row (sub-image) [12213; in 12213/12216 files] | — | ✅ `float32` |
| `PH_COL1` | ✅ | `Float32` | nan | 1e-4 mag | per row (sub-image) [12202; in 12213/12216 files] | — | ✅ `float32` |
| `PH_MEDC` | ✅ | `Float32` | nan | 1e-4 mag | per row (sub-image) [1109; in 12213/12216 files] | — | ✅ `float32` |
| `PH_RMS` | ✅ | `Float32` | nan | 1e-4 mag | per row (sub-image) [12200; in 12213/12216 files] | — | ✅ `float32` |
| `PH_NSRC` | ✅ | `UInt16` | 0 | 1 | per row (sub-image) [72; in 12213/12216 files] | — | ✅ `Int32` |
| `PH_MAGSY` | ✅ | `LowCardinality(String)` | '' | — | constant [1; in 12213/12216 files] | ✅ in 12213/12216 | ✅ `char8` |
| `PH_MAGT` | ✅ | `LowCardinality(String)` | '' | — | constant [1; in 12213/12216 files] | ✅ in 12213/12216 | ✅ `char16` |
| `PH_MAGTE` | ❌ | `LowCardinality(String)` | '' | — | constant [1; in 12213/12216 files] | ✅ in 12213/12216 | ✅ `char16` |
| `PH_CAT` | ✅ | `LowCardinality(String)` | '' | — | constant [1; in 12213/12216 files] | ✅ in 12213/12216 | ❌ |

</details>

<details><summary><b>Absolute photometric calibration (PT_): summary</b> (23 of 23 kept)</summary>

| Keyword | Include | Data type | Default | Resolution | Cardinality | Same as coadd | In 2024 sheet |
|---|:---:|---|---|---|---|:---:|---|
| `PT_ZP` | ✅ | `Float32` | nan | 1e-4 mag | per coadd sub-image [624] | ✅ | ❌ |
| `PT_NORMU` | ✅ | `LowCardinality(String)` | '' | — | constant [new, not in the sample] | ✅ | ❌ |
| `PT_FNORM` | ✅ | `Float32` | nan | 1e-5 | per coadd sub-image [new, not in the sample] | ✅ | ❌ |
| `LIMMAG` | ✅ | `Float32` | nan | 1e-3 mag | per row (sub-image) [12214] | — | ✅ `float32` |
| `LIMMAGER` | ✅ | `Float32` | nan | 1e-4 mag | per row (sub-image) [12214] | — | ❌ |
| `PT_RMS` | ✅ | `Float32` | nan | 1e-4 mag | per coadd sub-image [620] | ✅ | ✅ `float32` |
| `PT_ARMS` | ✅ | `Float32` | nan | 1e-4 mag | per coadd sub-image [601] | ✅ | ❌ |
| `PT_CHI2` | ✅ | `Float32` | nan | 1e-3 | per coadd sub-image [624] | ✅ | ❌ |
| `PT_DOF` | ✅ | `UInt16` | 0 | 1 | per coadd sub-image [65] | ✅ | ❌ |
| `PT_NCALI` | ✅ | `UInt16` | 0 | 1 | per coadd sub-image [72] | ✅ | ❌ |
| `PT_DZPAB` | ✅ | `Float32` | nan | 1e-5 mag | per coadd sub-image [624] | ✅ | ❌ |
| `PT_CTA` | ✅ | `Float32` | nan | 1e-5 mag | per coadd sub-image [624] | ✅ | ❌ |
| `PT_CTA2` | ✅ | `Float32` | nan | 1e-5 mag | per coadd sub-image [624] | ✅ | ❌ |
| `PT_CTA3` | ✅ | `Float32` | nan | 1e-6 mag | per coadd sub-image [624] | ✅ | ❌ |
| `PT_CTA4` | ✅ | `Float32` | nan | 1e-6 mag | per coadd sub-image [624] | ✅ | ❌ |
| `PT_CTAE` | ✅ | `Float32` | nan | 1e-6 mag | per coadd sub-image [624] | ✅ | ❌ |
| `PT_AREF` | ✅ | `LowCardinality(String)` | '' | — | constant [1] | ✅ | ❌ |
| `PT_SPEC` | ✅ | `LowCardinality(String)` | '' | — | constant [1] | ✅ | ❌ |
| `PT_CAT` | ✅ | `LowCardinality(String)` | '' | — | constant [1] | ✅ | ❌ |
| `PT_REFSL` | ✅ | `Float32` | nan | 1e-3 | constant [1] | ✅ | ❌ |
| `PT_REFPV` | ✅ | `Float32` | nan | 1 | constant [1] | ✅ | ❌ |
| `PT_REFC` | ✅ | `Float32` | nan | 1e-3 | constant [1] | ✅ | ❌ |
| `PT_CO2PP` | ✅ | `Float32` | nan | 1 ppm | constant [1] | ✅ | ❌ |

</details>

<details><summary><b>Absolute photometric calibration (PT_): transmission-model parameters</b> (77 of 77 kept)</summary>

| Keyword | Include | Data type | Default | Resolution | Cardinality | Same as coadd | In 2024 sheet |
|---|:---:|---|---|---|---|:---:|---|
| `PT_1_N` | ✅ | `LowCardinality(String)` | '' | — | constant [1] | ✅ | ❌ |
| `PT_1_V1` | ✅ | `Float32` | nan | 1e-5 | per row (sub-image) [12216] | — | ❌ |
| `PT_1_F1` | ✅ | `UInt8` | 0 | 1 | constant [1] | ✅ in 12196/12216 | ❌ |
| `PT_2_N` | ✅ | `LowCardinality(String)` | '' | — | constant [1] | ✅ | ❌ |
| `PT_2_V1` | ✅ | `Float32` | nan | 1e-5 | per coadd sub-image [624] | ✅ in 12196/12216 | ❌ |
| `PT_2_F1` | ✅ | `UInt8` | 0 | 1 | constant [1] | ✅ in 12196/12216 | ❌ |
| `PT_2_V2` | ✅ | `Float32` | nan | 1e-5 | low (<100) [1] | ✅ in 12196/12216 | ❌ |
| `PT_2_F2` | ✅ | `UInt8` | 0 | 1 | constant [1] | ✅ in 12196/12216 | ❌ |
| `PT_3_N` | ✅ | `LowCardinality(String)` | '' | — | constant [1] | ✅ | ❌ |
| `PT_3_V1` | ✅ | `Float32` | nan | 1e-5 | per coadd sub-image [624] | ✅ in 12196/12216 | ❌ |
| `PT_3_F1` | ✅ | `UInt8` | 0 | 1 | constant [1] | ✅ in 12196/12216 | ❌ |
| `PT_3_V2` | ✅ | `Float32` | nan | 1e-5 | per coadd sub-image [557] | ✅ in 12196/12216 | ❌ |
| `PT_3_F2` | ✅ | `UInt8` | 0 | 1 | constant [2] | ✅ in 12196/12216 | ❌ |
| `PT_3_V3` | ✅ | `Float32` | nan | 1e-5 | low (<100) [1] | ✅ in 12196/12216 | ❌ |
| `PT_3_F3` | ✅ | `UInt8` | 0 | 1 | constant [1] | ✅ in 12196/12216 | ❌ |
| `PT_4_N` | ✅ | `LowCardinality(String)` | '' | — | constant [1] | ✅ | ❌ |
| `PT_4_V1` | ✅ | `Float32` | nan | 1e-5 | per coadd sub-image [624] | ✅ in 12196/12216 | ❌ |
| `PT_4_F1` | ✅ | `UInt8` | 0 | 1 | constant [1] | ✅ in 12196/12216 | ❌ |
| `PT_4_V2` | ✅ | `Float32` | nan | 1e-5 | low (<100) [1] | ✅ in 12196/12216 | ❌ |
| `PT_4_F2` | ✅ | `UInt8` | 0 | 1 | constant [1] | ✅ in 12196/12216 | ❌ |
| `PT_5_N` | ✅ | `LowCardinality(String)` | '' | — | constant [1] | ✅ | ❌ |
| `PT_5_V1` | ✅ | `Float32` | nan | 1e-5 | per coadd sub-image [624] | ✅ in 12196/12216 | ❌ |
| `PT_5_F1` | ✅ | `UInt8` | 0 | 1 | constant [1] | ✅ in 12196/12216 | ❌ |
| `PT_5_V2` | ✅ | `Float32` | nan | 1e-5 | per coadd sub-image [555] | ✅ in 12196/12216 | ❌ |
| `PT_5_F2` | ✅ | `UInt8` | 0 | 1 | constant [2] | ✅ in 12196/12216 | ❌ |
| `PT_5_V3` | ✅ | `Float32` | nan | 1e-5 | low (<100) [1] | ✅ in 12196/12216 | ❌ |
| `PT_5_F3` | ✅ | `UInt8` | 0 | 1 | constant [1] | ✅ in 12196/12216 | ❌ |
| `PT_6_N` | ✅ | `LowCardinality(String)` | '' | — | constant [1] | ✅ | ❌ |
| `PT_6_V1` | ✅ | `Float32` | nan | 1e-5 | per coadd sub-image [624] | ✅ in 12196/12216 | ❌ |
| `PT_6_F1` | ✅ | `UInt8` | 0 | 1 | constant [1] | ✅ in 12196/12216 | ❌ |
| `PT_6_V2` | ✅ | `Float32` | nan | 1e-5 | low (<100) [17] | ✅ in 12196/12216 | ❌ |
| `PT_6_F2` | ✅ | `UInt8` | 0 | 1 | constant [1] | ✅ in 12196/12216 | ❌ |
| `PT_6_V3` | ✅ | `Float32` | nan | 1e-5 | low (<100) [1] | ✅ in 12196/12216 | ❌ |
| `PT_6_F3` | ✅ | `UInt8` | 0 | 1 | constant [1] | ✅ in 12196/12216 | ❌ |
| `PT_6_V4` | ✅ | `Float32` | nan | 1e-5 | low (<100) [1] | ✅ in 12196/12216 | ❌ |
| `PT_6_F4` | ✅ | `UInt8` | 0 | 1 | constant [1] | ✅ in 12196/12216 | ❌ |
| `PT_7_N` | ✅ | `LowCardinality(String)` | '' | — | constant [1] | ✅ | ❌ |
| `PT_7_V1` | ✅ | `Float32` | nan | 1e-5 | per coadd sub-image [1] | ✅ in 12196/12216 | ❌ |
| `PT_7_F1` | ✅ | `UInt8` | 0 | 1 | constant [1] | ✅ in 12196/12216 | ❌ |
| `PT_7_V2` | ✅ | `Float32` | nan | 1e-5 | low (<100) [1] | ✅ in 12196/12216 | ❌ |
| `PT_7_F2` | ✅ | `UInt8` | 0 | 1 | constant [1] | ✅ in 12196/12216 | ❌ |
| `PT_7_V3` | ✅ | `Float32` | nan | 1e-5 | low (<100) [1] | ✅ in 12196/12216 | ❌ |
| `PT_7_F3` | ✅ | `UInt8` | 0 | 1 | constant [1] | ✅ in 12196/12216 | ❌ |
| `PT_7_V4` | ✅ | `Float32` | nan | 1e-5 | low (<100) [1] | ✅ in 12196/12216 | ❌ |
| `PT_7_F4` | ✅ | `UInt8` | 0 | 1 | constant [1] | ✅ in 12196/12216 | ❌ |
| `PT_7_V5` | ✅ | `Float32` | nan | 1e-5 | low (<100) [1] | ✅ in 12196/12216 | ❌ |
| `PT_7_F5` | ✅ | `UInt8` | 0 | 1 | constant [1] | ✅ in 12196/12216 | ❌ |
| `PT_7_V6` | ✅ | `Float32` | nan | 1e-5 | low (<100) [1] | ✅ in 12196/12216 | ❌ |
| `PT_7_F6` | ✅ | `UInt8` | 0 | 1 | constant [1] | ✅ in 12196/12216 | ❌ |
| `PT_7_V7` | ✅ | `Float32` | nan | 1e-5 | low (<100) [1] | ✅ in 12196/12216 | ❌ |
| `PT_7_F7` | ✅ | `UInt8` | 0 | 1 | constant [1] | ✅ in 12196/12216 | ❌ |
| `PT_7_V8` | ✅ | `Float32` | nan | 1e-5 | low (<100) [1] | ✅ in 12196/12216 | ❌ |
| `PT_7_F8` | ✅ | `UInt8` | 0 | 1 | constant [1] | ✅ in 12196/12216 | ❌ |
| `PT_7_V9` | ✅ | `Float32` | nan | 1e-5 | low (<100) [1] | ✅ in 12196/12216 | ❌ |
| `PT_7_F9` | ✅ | `UInt8` | 0 | 1 | constant [1] | ✅ in 12196/12216 | ❌ |
| `PT_8_N` | ✅ | `LowCardinality(String)` | '' | — | constant [1] | ✅ | ❌ |
| `PT_8_V1` | ✅ | `Float32` | nan | 1e-5 | per coadd sub-image [1] | ✅ in 12196/12216 | ❌ |
| `PT_8_F1` | ✅ | `UInt8` | 0 | 1 | constant [1] | ✅ in 12196/12216 | ❌ |
| `PT_8_V2` | ✅ | `Float32` | nan | 1e-5 | per coadd sub-image [557] | ✅ in 12196/12216 | ❌ |
| `PT_8_F2` | ✅ | `UInt8` | 0 | 1 | constant [2] | ✅ in 12196/12216 | ❌ |
| `PT_8_V3` | ✅ | `Float32` | nan | 1e-5 | low (<100) [1] | ✅ in 12196/12216 | ❌ |
| `PT_8_F3` | ✅ | `UInt8` | 0 | 1 | constant [1] | ✅ in 12196/12216 | ❌ |
| `PT_8_V4` | ✅ | `Float32` | nan | 1e-5 | low (<100) [1] | ✅ in 12196/12216 | ❌ |
| `PT_8_F4` | ✅ | `UInt8` | 0 | 1 | constant [1] | ✅ in 12196/12216 | ❌ |
| `PT_P_N` | ✅ | `LowCardinality(String)` | '' | — | constant [1] | ✅ | ❌ |
| `PT_P_NX1` | ✅ | `Float32` | nan | 0.5 pix | constant [1] | ✅ in 12196/12216 | ❌ |
| `PT_P_NX2` | ✅ | `Float32` | nan | 0.5 pix | constant [1] | ✅ in 12196/12216 | ❌ |
| `PT_P_NY1` | ✅ | `Float32` | nan | 0.5 pix | constant [1] | ✅ in 12196/12216 | ❌ |
| `PT_P_NY2` | ✅ | `Float32` | nan | 0.5 pix | constant [1] | ✅ in 12196/12216 | ❌ |
| `PT_P_V1` | ✅ | `Float32` | nan | 1e-6 mag | constant [1] | ✅ in 12196/12216 | ❌ |
| `PT_P_F1` | ✅ | `UInt8` | 0 | 1 | constant [2] | ✅ in 12196/12216 | ❌ |
| `PT_P_V2` | ✅ | `Float32` | nan | 1e-6 mag | per coadd sub-image [557] | ✅ in 12196/12216 | ❌ |
| `PT_P_F2` | ✅ | `UInt8` | 0 | 1 | constant [2] | ✅ in 12196/12216 | ❌ |
| `PT_P_V3` | ✅ | `Float32` | nan | 1e-6 mag | per coadd sub-image [557] | ✅ in 12196/12216 | ❌ |
| `PT_P_F3` | ✅ | `UInt8` | 0 | 1 | constant [2] | ✅ in 12196/12216 | ❌ |
| `PT_P_V4` | ✅ | `Float32` | nan | 1e-6 mag | per coadd sub-image [557] | ✅ in 12196/12216 | ❌ |
| `PT_P_F4` | ✅ | `UInt8` | 0 | 1 | constant [2] | ✅ in 12196/12216 | ❌ |

</details>

<details><summary><b>Aperture corrections</b> (26 of 26 kept)</summary>

| Keyword | Include | Data type | Default | Resolution | Cardinality | Same as coadd | In 2024 sheet |
|---|:---:|---|---|---|---|:---:|---|
| `APC0_A1` | ✅ | `Float32` | nan | 1e-6 mag | per coadd sub-image [623; in 12196/12216 files] | ✅ in 12196/12216 | ❌ |
| `APCX_A1` | ✅ | `Float32` | nan | 1e-6 mag | per coadd sub-image [623; in 12196/12216 files] | ✅ in 12196/12216 | ❌ |
| `APCY_A1` | ✅ | `Float32` | nan | 1e-6 mag | per coadd sub-image [623; in 12196/12216 files] | ✅ in 12196/12216 | ❌ |
| `APCXY_A1` | ✅ | `Float32` | nan | 1e-6 mag | per coadd sub-image [623; in 12196/12216 files] | ✅ in 12196/12216 | ❌ |
| `APCC_A1` | ✅ | `Float32` | nan | 1e-6 mag | per coadd sub-image [623; in 12196/12216 files] | ✅ in 12196/12216 | ❌ |
| `APCCE_A1` | ✅ | `Float32` | nan | 1e-6 mag | per coadd sub-image [623; in 12196/12216 files] | ✅ in 12196/12216 | ❌ |
| `APC0_A2` | ✅ | `Float32` | nan | 1e-6 mag | per coadd sub-image [623; in 12196/12216 files] | ✅ in 12196/12216 | ❌ |
| `APCX_A2` | ✅ | `Float32` | nan | 1e-6 mag | per coadd sub-image [623; in 12196/12216 files] | ✅ in 12196/12216 | ❌ |
| `APCY_A2` | ✅ | `Float32` | nan | 1e-6 mag | per coadd sub-image [623; in 12196/12216 files] | ✅ in 12196/12216 | ❌ |
| `APCXY_A2` | ✅ | `Float32` | nan | 1e-6 mag | per coadd sub-image [623; in 12196/12216 files] | ✅ in 12196/12216 | ❌ |
| `APCC_A2` | ✅ | `Float32` | nan | 1e-6 mag | per coadd sub-image [623; in 12196/12216 files] | ✅ in 12196/12216 | ❌ |
| `APCCE_A2` | ✅ | `Float32` | nan | 1e-6 mag | per coadd sub-image [623; in 12196/12216 files] | ✅ in 12196/12216 | ❌ |
| `APC0_A4` | ✅ | `Float32` | nan | 1e-6 mag | per coadd sub-image [623; in 12196/12216 files] | ✅ in 12196/12216 | ❌ |
| `APCX_A4` | ✅ | `Float32` | nan | 1e-6 mag | per coadd sub-image [623; in 12196/12216 files] | ✅ in 12196/12216 | ❌ |
| `APCY_A4` | ✅ | `Float32` | nan | 1e-6 mag | per coadd sub-image [623; in 12196/12216 files] | ✅ in 12196/12216 | ❌ |
| `APCXY_A4` | ✅ | `Float32` | nan | 1e-6 mag | per coadd sub-image [623; in 12196/12216 files] | ✅ in 12196/12216 | ❌ |
| `APCC_A4` | ✅ | `Float32` | nan | 1e-6 mag | per coadd sub-image [623; in 12196/12216 files] | ✅ in 12196/12216 | ❌ |
| `APCCE_A4` | ✅ | `Float32` | nan | 1e-6 mag | per coadd sub-image [623; in 12196/12216 files] | ✅ in 12196/12216 | ❌ |
| `APC0_PS` | ✅ | `Float32` | nan | 1e-6 mag | per coadd sub-image [623; in 12196/12216 files] | ✅ in 12196/12216 | ❌ |
| `APCX_PS` | ✅ | `Float32` | nan | 1e-6 mag | per coadd sub-image [623; in 12196/12216 files] | ✅ in 12196/12216 | ❌ |
| `APCY_PS` | ✅ | `Float32` | nan | 1e-6 mag | per coadd sub-image [623; in 12196/12216 files] | ✅ in 12196/12216 | ❌ |
| `APCXY_PS` | ✅ | `Float32` | nan | 1e-6 mag | per coadd sub-image [623; in 12196/12216 files] | ✅ in 12196/12216 | ❌ |
| `APCC_PS` | ✅ | `Float32` | nan | 1e-6 mag | per coadd sub-image [623; in 12196/12216 files] | ✅ in 12196/12216 | ❌ |
| `APCCE_PS` | ✅ | `Float32` | nan | 1e-6 mag | per coadd sub-image [623; in 12196/12216 files] | ✅ in 12196/12216 | ❌ |
| `APCC_REF` | ✅ | `Float32` | nan | 1e-3 mag | constant [1; in 12196/12216 files] | ✅ in 12196/12216 | ❌ |
| `APCOR_N` | ✅ | `UInt16` | 0 | 1 | per coadd sub-image [182; in 12196/12216 files] | ✅ in 12196/12216 | ❌ |

</details>

### Columns computed or corrected at ingestion

These columns cannot be copied from the header as they are. The ingestion code has to compute or convert them.

| Column(s) | Type | What the ingestion code does |
|---|---|---|
| `ingestion_time_jd` (new, not in the header) | `Float64` | JD (UTC) of the moment the row is inserted, taken from the ingesting computer's clock, as in `diff_src` and `image_visit`. Default `nan`. |
| `id_coadd` (new, not in the header; option, see note 2) | `UInt64` | The `ID_COADD` of the coadd this epoch went into: the coadd of the same camera, visit (`SUBDIR`) and `CROPID`, whose `ID_PROCF` … `ID_PROCL` range contains this `ID_PROC`. 0 if the visit has no coadd. |
| `ID_PROC`, `ID_RAW`, `ID_DARK`, `ID_FLAT` | `UInt64` | The header holds these IDs as text (e.g. `'2224430602496017002'`). Convert the text directly to an integer, never through a `double`. A `double` is exact only up to about 9×10¹⁵, and the IDs are about 2×10¹⁸, so the lowest bits, which hold the image type and level, would be lost. That is also why the IDs are text in the header on purpose (`imProc.db.generateImageID`, since 2024): MATLAB's FITS writer rejects `uint64`, and AstroPack reads numeric header values as `double`. Missing or blank → 0. |
| `UPIX_PAR`, `UPIX_LOW`, `UPIX_HIG` | `UInt64` | Written as text in the header (e.g. `'51198298771'`); convert to an integer. Missing → 0. |
| `LIGHTSEC`, `OVERSCAN`, `CCDSEC`, `ORIGSEC`, `ORIGUSEC`, `UNIQSEC`, `EXCLSEC`, `ORIGESEC` | 4 × `Int16` each | Split the text `'[xmin xmax ymin ymax]'` into four numbers (`imUtil.ccdsec.ccdsecStr2num`), written to `<keyword>_xmin`, `_xmax`, `_ymin`, `_ymax`. Missing → 0. |
| `DATE-OBS` (column `date_obs`) | `DateTime64(3,'UTC')` | Convert the ISO text `DATE-OBS` (e.g. `'2026-04-06T00:39:34.229'`) to a date-time with milliseconds. |
| `VERSION` | `UInt8` | Written as text (`'1'`); convert to an integer. |
| `FIELDID` | `LowCardinality(String)` | Numeric fields are written as real numbers (`1214.`), named targets as text (`'21109WD'`). Store the number without the trailing point (`'1214'`), so that it matches `OBJECT`. |
| `NODENUMB`, `MOUNTNUM`, `CAMNUM`, `CROPID`, `COUNTER`, `CAMMODE`, `CAMGAIN`, `CAMOFFS`, `BINX`, `BINY`, `N_STARS`, `AST_NSRC`, `PSF_NST`, `PSF_NPK`, `PH_NSRC`, `PT_DOF`, `PT_NCALI`, `PT_1_F1`, `PT_2_F1`, `PT_2_F2`, `PT_3_F1`, `PT_3_F2`, `PT_3_F3`, `PT_4_F1`, `PT_4_F2`, `PT_5_F1`, `PT_5_F2`, `PT_5_F3`, `PT_6_F1`, `PT_6_F2`, `PT_6_F3`, `PT_6_F4`, `PT_7_F1`, `PT_7_F2`, `PT_7_F3`, `PT_7_F4`, `PT_7_F5`, `PT_7_F6`, `PT_7_F7`, `PT_7_F8`, `PT_7_F9`, `PT_8_F1`, `PT_8_F2`, `PT_8_F3`, `PT_8_F4`, `PT_P_F1`, `PT_P_F2`, `PT_P_F3`, `PT_P_F4`, `APCOR_N` | `UInt8`/`UInt16`/`UInt32` | The pipeline holds these whole numbers as MATLAB `double`, so the header writer gives them a trailing point (`1.`), and writes a blank card when the value is NaN (e.g. failed photometric calibration). Convert to an integer; blank → 0. (`BCKMTHD`, `VARMTHD` and `PSTATUS` are set as integer types and written as integers already.) |
| All `Float32`/`Float64` columns | — | Blank values (empty card) → `nan`. Seen in the sample: `PSF_PKR` (11448), `PSF_DPK` (11448), `PT_ARMS` (480), `PT_RMS` (100), `PT_CHI2` (20), `PT_DZPAB` (20), `PT_CTA` (20), `PT_CTA2` (20) and 10 more keywords. The `PT_` blanks are the coadd's: they appear in all epochs of a coadd sub-image whose calibration is blank. |
| All columns | — | A keyword missing from the header (e.g. in older products) → the column default. |

### Notes on specific choices

1. **About two thirds of the epoch header repeat the coadd header.** 207 of the 326 keywords have the coadd's value (column *Same as coadd*), 177 of them ✅ now. Most are naturally the same for a whole visit (camera, mount, file and software keywords). But all absolute-calibration (`PT_`) values except the normalisation `PT_1_V1`, and all aperture corrections (`APC*`), are the coadd's values: in the sample, `PT_ZP`, `PT_RMS` and the transmission parameters of an epoch equal those of its coadd (except in one sub-image, 20 epochs, where the coadd's are blank), while `PT_1_V1`, `LIMMAG` and the relative calibration `PH_*` differ from epoch to epoch. These 121 columns are kept for now; whether to drop them and take them from `image_visit` through `id_coadd` (note 2) will be discussed later. The camera, mount and software keywords that repeat the coadd are needed in `image_proc` and stay.
2. **`id_coadd` link.** The epoch header has no `ID_COADD`, so `image_proc` gets an `id_coadd` column computed at ingestion: the coadd header of the same visit and sub-image has `ID_PROCF` and `ID_PROCL`, the `ID_PROC` of its first and last epoch (both matched an epoch `ID_PROC` in all 624 coadd headers of the sample). A range on `ID_PROC` alone is not enough, because the IDs of the other sub-images of the same visit fall in the same range; the crop number (`CROPID`) must match too.
3. **`LEVEL` is wrong in the epoch header.** It says `raw` in all 12,216 headers, although the files are `proc` products. The `ID_PROC` values carry the same wrong level (`raw`) in their level bits. The IDs are still unique (`ID_RAW` differs in the crop number and the time), so this does not block the table, but `LEVEL` is ❌ here (it is fixed for this table) and the level bits of `id_proc` should not be relied on until this is fixed in the pipeline (#1412).
4. **`OPT_CLN`** (date of the last optics cleaning) is not in the epoch headers at all.
5. **File name and location keywords are ❌** in `image_proc`: `FILENAME`, `PROJNAME`, `SUBDIR`, `DIRYEAR`, `DIRMON`, `DIRDAY` and `FILETIME`. They are kept in `image_visit`, and the link to it is `id_coadd`.

### Differences from the 2024 design sheet

- **New:** 174 of the kept keywords are not in the `proc_images` column of the 2024 sheet: mostly PSF quality, absolute photometry (`PT_*`, `APC*`), image IDs, source counts and the processing status mask, as in #1409.
- **Dropped:** 10 columns of the 2024 sheet are ❌ here: `NAXIS1/2`, `CCDID`, `CTYPE1` and `PH_MAGTE` (the same as in #1409), and the file location keywords `SUBDIR`, `DIRYEAR`, `DIRMON`, `DIRDAY` and `FILETIME` (note 5).
- **Precision:** as in #1409, `Float64` instead of `float32` for `CRVAL`, `CD`, `PV` and the corner coordinates.

### Indexes

Draft for @ChenTishler to approve or edit. `discussion2sql` reads this table into `CREATE TABLE` and into an ALTER script for a table that already exists (types: `minmax`, `set(N)`, `bloom_filter`, `tokenbf_v1`, `ngrambf_v1`; granularity in blocks of 8192 rows; an empty table cell means this table).

| Index | Expression | Type | Granularity | Table | Purpose |
|---|---|---|---|---|---|
| `idx_jd` | `jd` | `minmax` | 1 | | time range |
| `idx_jd_start` | `jd_start` | `minmax` | 1 | | time range |
| `idx_midjd` | `midjd` | `minmax` | 1 | | time range |
| `idx_date_obs` | `date_obs` | `minmax` | 1 | | time range |
| `idx_ingest` | `ingestion_time_jd` | `minmax` | 1 | | rows added since a given time (copies, checks) |
| `idx_id_coadd` | `id_coadd` | `minmax` | 1 | | the epochs of a given coadd (`image_visit` row) |
| `idx_id_raw` | `id_raw` | `minmax` | 1 | | the epoch image of a given raw image |
| `idx_id_dark` | `id_dark` | `bloom_filter(0.01)` | 1 | | epochs made with a given dark |
| `idx_id_flat` | `id_flat` | `bloom_filter(0.01)` | 1 | | epochs made with a given flat |
| `idx_fieldid` | `fieldid` | `bloom_filter(0.01)` | 1 | | one field |
| `idx_object` | `object` | `bloom_filter(0.01)` | 1 | | one target |
| `idx_upix_par` | `upix_par` | `bloom_filter(0.01)` | 1 | | one sky partition (see question 1) |
| `idx_pipever` | `pipever` | `set(16)` | 1 | | one pipeline version |

### Index choices by column

The table is sorted by `id_proc`, whose highest bits are the epoch time (`MIDJD`, checked on all 98,352 rows in `last_test`; `ID_COADD` and `ID_RAW` have the same layout with the coadd and raw time, `ID_DARK` and `ID_FLAT` the time of the calibration image). So each block of 8192 rows holds about 4 minutes of all cameras, i.e. about one epoch of 12 visits. A skip index helps only for a column whose values are bunched within such a block: time columns and the time-ordered IDs (`minmax`), and values that occur in few blocks, such as one field or one flat (`bloom_filter`). At about 1.6×10⁸ rows (20,000 blocks) per year the table is 20 times `image_visit`, so a column without an index takes up to about a second to read.

**Measured** on a simulation of this table (no multi-camera epoch table exists yet): every coadd row of `last.visit_images` of the last 60 days made into 20 epochs 20 s apart, sorted like `id_proc`, 6190 blocks; per block: 33 cameras, 12 fields, 28 `upix_par` values, 78° in Dec:

| Query | Rows selected | Blocks to read |
|---|---|---|
| the epochs of one coadd (`id_coadd`) | — | 2.7 blocks (at most 4) |
| one field (`fieldid`) | — | 2.7% (90% of fields: < 6%) |
| one flat (`id_flat`) | — | 2.8% |
| one sky partition (`upix_par`) | — | 9% (90%: < 24%) |
| Dec 10°–20° | — | 99% |
| FWHM < 2.5 | 0.2% | 18% |
| airmass < 1.1 | — | 96% |

**Columns without an index** (every ✅ column not in the table above):

| Columns | Why no index |
|---|---|
| **ID, sorting key**: `id_proc` | The sorting key itself (primary index). |
| **Camera, mount, node, crop**: `nodenumb`, `mountnum`, `camnum`, `cropid`, `camname` | Every block holds most cameras (33 per block in the simulation), so an index cannot skip blocks. Per-camera queries read the whole column (up to about a second per year of data; question 2). |
| **Other time columns**: `time`, `lst`, `crdate` | `lst` repeats every day; `crdate` and `time` follow the reduction, not the observation. |
| **Sky position**: `m_ra`, `m_ha`, `m_dec`, `m_az`, `m_alt`, `m_jra`, `m_jdec`, `m_ara`, `m_aha`, `m_adec`, `m_adra`, `m_adha`, `m_addec`, `ra`, `dec`, `upix_low`, `upix_hig`, `ra1`, `ra2`, `ra3`, `ra4`, `dec1`, `dec2`, `dec3`, `dec4`, `rau1`, `rau2`, `rau3`, `rau4`, `decu1`, `decu2`, `decu3`, `decu4` | Each block covers about 78° in Dec: a 10° Dec band still reads 99% of the blocks. Use `upix_par` (question 1). |
| **Constant or few values (versions, methods, catalogues, settings, image sections, model names and flags)**: 122 columns: `version`, `gitastrp`, `git_camr`, `gitfocus`, `bias_im`, `flat_im`, `pstatus`, `schedulr`, `exptime`, `expmode`, `counter`, `filter`, `obslon`, `obslat`, `obsalt`, `trk_ra`, `trk_dec`, `camtemp`, `camcool`, `cammode`, `camgain`, `camoffs`, `origgain`, `darkcur`, `saturval`, `nonlin`, `binx`, `biny`, `lightsec_xmin`, `lightsec_xmax`, `lightsec_ymin`, `lightsec_ymax`, `overscan_xmin`, `overscan_xmax`, `overscan_ymin`, `overscan_ymax`, `ccdsec_xmin`, `ccdsec_xmax`, `ccdsec_ymin`, `ccdsec_ymax`, `origsec_xmin`, `origsec_xmax`, `origsec_ymin`, `origsec_ymax`, `origusec_xmin`, `origusec_xmax`, `origusec_ymin`, `origusec_ymax`, `uniqsec_xmin`, `uniqsec_xmax`, `uniqsec_ymin`, `uniqsec_ymax`, `exclsec_xmin`, `exclsec_xmax`, `exclsec_ymin`, `exclsec_ymax`, `origesec_xmin`, `origesec_xmax`, `origesec_ymin`, `origesec_ymax`, `bckmthd`, `varmthd`, `ast_cat`, `psf_npk`, `psf_rpk`, `ph_magsy`, `ph_magt`, `ph_cat`, `pt_normu`, `pt_aref`, `pt_spec`, `pt_cat`, `pt_refsl`, `pt_refpv`, `pt_refc`, `pt_co2pp`, `pt_1_n`, `pt_1_f1`, `pt_2_n`, `pt_2_f1`, `pt_2_f2`, `pt_3_n`, `pt_3_f1`, `pt_3_f2`, `pt_3_f3`, `pt_4_n`, `pt_4_f1`, `pt_4_f2`, `pt_5_n`, `pt_5_f1`, `pt_5_f2`, `pt_5_f3`, `pt_6_n`, `pt_6_f1`, `pt_6_f2`, `pt_6_f3`, `pt_6_f4`, `pt_7_n`, `pt_7_f1`, `pt_7_f2`, `pt_7_f3`, `pt_7_f4`, `pt_7_f5`, `pt_7_f6`, `pt_7_f7`, `pt_7_f8`, `pt_7_f9`, `pt_8_n`, `pt_8_f1`, `pt_8_f2`, `pt_8_f3`, `pt_8_f4`, `pt_p_n`, `pt_p_nx1`, `pt_p_nx2`, `pt_p_ny1`, `pt_p_ny2`, `pt_p_f1`, `pt_p_f2`, `pt_p_f3`, `pt_p_f4`, `apcc_ref` | Few distinct values, mostly the same in every block, so an index cannot skip blocks. |
| **Measured values: image quality, background, PSF, WCS, calibration, temperatures, focus**: 146 columns: `airmass`, `mnttemp`, `focus`, `prvfocus`, `gain`, `readnoi`, `meanbck`, `medbck`, `stdbck`, `minbck`, `maxbck`, `meanvar`, `medvar`, `backmag`, `n_stars`, `mag_95q`, `m_chi2d`, `ast_nsrc`, `ast_arms`, `ast_errm`, `pixscale`, `rotat`, `crpix1`, `crpix2`, `crval1`, `crval2`, `cd1_1`, `cd1_2`, `cd2_1`, `cd2_2`, `pv1_0`, `pv1_1`, `pv1_2`, `pv1_4`, `pv1_5`, `pv1_6`, `pv1_7`, `pv1_8`, `pv1_9`, `pv1_10`, `pv2_0`, `pv2_1`, `pv2_2`, `pv2_4`, `pv2_5`, `pv2_6`, `pv2_7`, `pv2_8`, `pv2_9`, `pv2_10`, `fwhe`, `fwhm`, `psf_fitn`, `psf_fita`, `psf_fitb`, `psf_fitt`, `psf_nst`, `psf_pkr`, `psf_dpk`, `psf_err`, `psf_s2`, `psf_af_1`, `psf_af_2`, `psf_af_3`, `psf_af_4`, `med_x2`, `med_y2`, `med_xy`, `quicfwhm`, `ph_zp`, `ph_col1`, `ph_medc`, `ph_rms`, `ph_nsrc`, `pt_zp`, `pt_fnorm`, `limmag`, `limmager`, `pt_rms`, `pt_arms`, `pt_chi2`, `pt_dof`, `pt_ncali`, `pt_dzpab`, `pt_cta`, `pt_cta2`, `pt_cta3`, `pt_cta4`, `pt_ctae`, `pt_1_v1`, `pt_2_v1`, `pt_2_v2`, `pt_3_v1`, `pt_3_v2`, `pt_3_v3`, `pt_4_v1`, `pt_4_v2`, `pt_5_v1`, `pt_5_v2`, `pt_5_v3`, `pt_6_v1`, `pt_6_v2`, `pt_6_v3`, `pt_6_v4`, `pt_7_v1`, `pt_7_v2`, `pt_7_v3`, `pt_7_v4`, `pt_7_v5`, `pt_7_v6`, `pt_7_v7`, `pt_7_v8`, `pt_7_v9`, `pt_8_v1`, `pt_8_v2`, `pt_8_v3`, `pt_8_v4`, `pt_p_v1`, `pt_p_v2`, `pt_p_v3`, `pt_p_v4`, `apc0_a1`, `apcx_a1`, `apcy_a1`, `apcxy_a1`, `apcc_a1`, `apcce_a1`, `apc0_a2`, `apcx_a2`, `apcy_a2`, `apcxy_a2`, `apcc_a2`, `apcce_a2`, `apc0_a4`, `apcx_a4`, `apcy_a4`, `apcxy_a4`, `apcc_a4`, `apcce_a4`, `apc0_ps`, `apcx_ps`, `apcy_ps`, `apcxy_ps`, `apcc_ps`, `apcce_ps`, `apcor_n` | They do not follow time order: airmass < 1.1 reads 96% of the blocks; FWHM < 2.5 selects 0.2% of rows but 18% of the blocks in the simulation, where each epoch has the FWHM of its coadd, so probably more in reality (question 4). |

**Questions for @ChenTishler:**
1. Sky position: as in #1409, a `bloom_filter` on `upix_par` (9% of the blocks for one partition) or a projection sorted by position, `SELECT * ORDER BY (upix_par, id_proc)`, which reads about one block per partition but doubles the size of the table (tens of GB per year here).
2. Per-camera history: no index helps; a projection sorted by `(nodenumb, mountnum, camnum, id_proc)` would read only that camera, at twice the size.
3. Not yet tried on ClickHouse 22.1 (last0): `bloom_filter` on `LowCardinality(String)` columns and projections; both work on 26.6 (euclid).
4. Image quality: since a block holds only about 12 visits, a `minmax` on `fwhm` (and similar values) skips some blocks (82% for FWHM < 2.5 in the simulation). Worth adding for the most used quality cuts, once real epoch data can be measured?

