# Euclid vs WRX: what consumes proc catalog read time

Analysis of two measure-overhead visits with similar Cat size (~92 MiB, 480 files). Source: `insertDB_perf.log` lines from 2026-10-12. **Note:** those logs used **sec/MiB** (`rawSecPerMB`); current perf logging uses **MiB/s** (`rawMBps`, higher = faster) and run-level **`AVG catalogReadRates`** — see [logging.md](logging.md).

## Run context

| | Euclid | WRX80 |
|---|--------|-------|
| Visit | `.../2026/02/15/proc/171534v0` | `.../2026/03/15/proc/210455v0` |
| `sizeMB` | 91.69 | 93.76 |
| `n` Cat files | 480 | 480 |
| `SkipInsert` | true (no ClickHouse) | true |
| `CatalogRawFileRead` | true (diagnostic extra read) | true |

Different nights (Feb on euclid, Mar on WRX) follow the [disjoint-month protocol](hosts_and_matlab.md#cold-compare-disjoint-months-euclid-vs-wrx) to reduce cross-host cache reuse.

## Where the seconds go (per visit)

| Stage | Euclid | WRX | ~Share of `visitTotal` |
|-------|--------|-----|-------------------------|
| `readCatalogFilesRaw` | 6.0 s | 6.4 s | **~68–70%** |
| `readAstroCatalog` | 1.4 s | 1.5 s | ~16% |
| `readAstroHeader` | 1.3 s | 1.2 s | ~14% |
| `catalogFileInventory`, `keywordPrep`, skip insert | ~0 s | ~0 s | — |
| **`visitEnd` (total)** | **8.8 s** | **9.2 s** | 100% |

Almost all wall time is **opening and reading ~480 small proc Cat FITS**, not keyword prep and not DB insert (insert was skipped).

### Diagnostic vs production path

- **`readCatalogFilesRaw`** is enabled only for benchmarking (`MEASURE_CATALOG_RAW_FILE_READ=1`). It does a full `fread` of every Cat file **before** `AstroCatalog` runs. Production ingest does **not** do this pass.
- **Production-shaped read** for this pair is roughly **`readAstroCatalog` + `readAstroHeader`** ≈ **2.7 s** per visit (euclid ~2.7 s, WRX ~2.7 s).
- With raw enabled, visit total is inflated by ~6 s so cold open+read shows up in the log. See [logging.md](logging.md#proc-catalog-io-baseline-flags-catalogfileinventory--catalograwfileread).

## Host gap (this pair)

| Metric | Euclid | WRX | Notes |
|--------|--------|-----|--------|
| `rawSecPerMB` | 0.0652 | 0.0680 | ~**+4%** on WRX (NFS path) |
| `catSecPerMB` | 0.0157 | 0.0156 | Same (parse/CPU) |
| `hdrSecPerMB` | 0.0137 | 0.0129 | Similar |
| `visitTotal` | 8.8 s | 9.2 s | ~**+0.4 s (~5%)** |

Parse and header stages match. The only soft NFS signal is the raw I/O rate, and it is small on these warm-ish samples.

## Example log excerpts

**Euclid**

```text
insertArchiveCatalogs2DB_perf readCatalogFilesRaw n=480 sizeMB=91.69 secPerMB=0.0652 ... 6.0s
insertArchiveCatalogs2DB_perf readAstroCatalog sizeMB=91.69 secPerMB=0.0157 ... 1.4s
insertArchiveCatalogs2DB_perf readAstroHeader ... 1.3s
insertArchiveCatalogs2DB_perf visitEnd injected ... 8.8s
```

**WRX**

```text
insertArchiveCatalogs2DB_perf readCatalogFilesRaw n=480 sizeMB=93.76 secPerMB=0.0680 ... 6.4s
insertArchiveCatalogs2DB_perf readAstroCatalog sizeMB=93.76 secPerMB=0.0156 ... 1.5s
insertArchiveCatalogs2DB_perf readAstroHeader ... 1.2s
insertArchiveCatalogs2DB_perf visitEnd injected ... 9.2s
```

## Is catalog read alone a reason to run on euclid?

**For this workload and these samples: no.** A few percent per visit (~0.4 s here) is not a strong operational reason to move the daily cron from WRX to euclid.

Catalog read is **CPU + many small files**; WRX can look as fast as local RAID when cache is warm. See [hosts_and_matlab.md — Why NFS catalog read can look as fast as local](hosts_and_matlab.md#why-nfs-catalog-read-can-look-as-fast-as-local).

### What this analysis does not cover

1. **`insertCatalog`** — not measured here (`SkipInsert=true`). WRX still sends rows to ClickHouse on **euclid**; compare `visitInsert`, `secPer100k`, and **`AVG insertCatalog`** from [insert benchmark runs](hosts_and_matlab.md#insert-benchmark-last_perfproc_src_perf-only) before deciding on host for writes.
2. **Single visits** — use night-level **medians** of `catalogReadRates` and `AVG fullWork`, not one folder per host.
3. **Cold cache** — first touch of a night or dropped page cache can widen the raw I/O gap; these logs may reflect warm client or server cache.
4. **Other products** — coadd catalogs and visit images use larger FITS; NFS cost may show up more there.

### Practical guidance

- Keep **WRX cron** if ops and scheduling are simpler, unless insert benchmarks or month-scale load show a clear win on euclid.
- To compare read without the raw pass, set `MEASURE_CATALOG_RAW_FILE_READ=0` or use production-shaped flags when interpreting `visitTotal`.
- For controlled cross-host compares, follow [Controlled comparison protocol](hosts_and_matlab.md#controlled-comparison-protocol).

## Related

- [hosts_and_matlab.md](hosts_and_matlab.md) — wrappers, env pitfalls, insert benchmark nights
- [logging.md](logging.md) — stage names, `catalogReadRates`, insert row rates
- [folder_file_estimates.md](folder_file_estimates.md) — ~480 Cat files per visit, file sizes
