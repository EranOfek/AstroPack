-- ============================================================================
--  last.proc_src  —  OPTIMIZED for ClickHouse 26.6
--  Column order is IDENTICAL to the original for easy diffing.
--
--  CHANGE LEGEND (see trailing comment on each line):
--    [REMOVED]   column dropped; get it via JOIN to proc_images on id_proc_im
--    [ADDED]     new column (decoded FLAGS bit) — not in the original
--    [ALIAS]     was MATERIALIZED (stored); now ALIAS (computed on read)
--    [IDX→BLOOM] skip index changed from minmax to bloom_filter (exact match)
--    [IDX KEEP]  minmax retained (column correlates with on-disk sort order)
--    [IDX DROP]  minmax removed (pruned nothing here) — replaced by PROJECTION
--                or by the primary key, as noted
--    [→PROJ H/M/L] served by a lightweight _part_offset projection (see bottom);
--                  H = build now, M = build when query logs justify, L = defer
--    [SORT KEY]  now part of ORDER BY
--
--  Rationale: table is sorted by HEALPix, so minmax prunes nothing for
--  scattered columns (ids, mags, S/N, jd, is_forced). Exact-match ids → bloom;
--  semi-random float ranges (mag/S-N) → lightweight _part_offset projections
--  (granule-level pruning since CH 25.11; multiple cooperate since 25.6).
-- ============================================================================

CREATE TABLE last.proc_src
(
    `id_proc_src` Int64,                                  -- [IDX→BLOOM] local detection id (exact match / provenance)
    `id_uniq_src` Int64,                                  -- [IDX→BLOOM] cross-epoch source id (light-curve retrieval) — HOT
    `id_cat_src` Int64,                                   -- [IDX→BLOOM] reference/catalog source id
    -- `id_raw_im` Int64,                                 -- [REMOVED] derivable from id_proc_im via proc_images (raw<->proc). ~0.8 TB @100B
    `id_proc_im` Int64,                                   -- [IDX→BLOOM] processed-image id ("all sources in image X" + join key) — HOT
    `upix_partition` UInt64,                              -- partition key (see PARTITION BY) — verify NSide → partition count
    `upix_low` UInt64,                                    -- [IDX KEEP] coarse HEALPix, correlated with sort → minmax prunes
    `upix_high` UInt64,                                   -- [IDX DROP] finest HEALPix = sort-key head; sparse PK already covers it
    `is_forced` Bool,                                     -- [→PROJ M] skewed bool, scattered → set/minmax fail; project or split table
    `jd` Float64,                                         -- [SORT KEY] folded into ORDER BY (upix_high, jd); minmax dropped
    `bjd` Float64,                                        -- per-source barycentric JD; no index (L) — depends on RA/Dec, not joinable
    `nepoch` Int64,                                       -- [→PROJ M] "well-observed" range cut; minmax dropped
    `ra` Float64,                                         -- [IDX KEEP*] PK-served for cone; minmax helps box cuts (*unreliable at 0/360 wrap)
    `dec` Float64,                                        -- [IDX KEEP] no wrap → minmax reliable under spatial sort (dec-band scans)
    `xpeak` Int16,                                        -- pixel position; no index
    `ypeak` Int16,                                        -- pixel position; no index
    `x` Float32,                                          -- pixel position; no index
    `y` Float32,                                          -- pixel position; no index
    `x1` Float32,                                         -- first moment; no index
    `y1` Float32,                                         -- first moment; no index
    `flags` UInt32,                                       -- bitmask: no index prunes flags & mask. Decode hot bits below; PREWHERE otherwise
    `flag_saturated` UInt8 MATERIALIZED bitTest(flags, 0),-- [ADDED] decode hot FLAGS bit — SET REAL BIT POSITION per your FLAGS spec
    `flag_edge` UInt8 MATERIALIZED bitTest(flags, 1),     -- [ADDED] decode hot FLAGS bit — SET REAL BIT POSITION
    `flag_cr` UInt8 MATERIALIZED bitTest(flags, 2),       -- [ADDED] decode hot FLAGS bit (cosmic ray / bad pixel) — SET REAL BIT POSITION
    `x2` Float32,                                         -- [→PROJ L] second moment (extendedness) — project only for morphology cuts
    `y2` Float32,                                         -- [→PROJ L] second moment (extendedness)
    `xy` Float32,                                         -- cross-moment; no index
    `sn` Float32,                                         -- [→PROJ H] PSF-fit S/N — hottest quality cut; minmax dropped
    `sn_ext1` Nullable(Float32),                          -- [→PROJ L] extended-source S/N
    `sn_ext2` Nullable(Float32),                          -- [→PROJ L] extended-source S/N
    `sn_delta` Nullable(Float32),                         -- [→PROJ M] hot-pixel/CR discriminator (precomputed SN_2-SN_1)
    `sn_1` Float32,                                       -- [→PROJ M] matched-filter S/N (delta hypothesis)
    `sn_2` Float32,                                       -- [→PROJ M] matched-filter S/N (PSF hypothesis)
    `sn_3` Float32,                                       -- [→PROJ L] extended hypothesis
    `sn_4` Float32,                                       -- [→PROJ L] extended hypothesis
    `sn_5` Float32,                                       -- [→PROJ L] extended hypothesis
    `flux_aper_1` Float32,                                -- no index (users cut on magnitudes, not fluxes)
    `fluxerr_aper_1` Float32,                             -- no index
    `mag_aper_1` Float32,                                 -- [→PROJ M] aperture-magnitude cut
    `magerr_aper_1` Float32,                              -- no index
    `flux_aper_2` Float32,                                -- no index
    `fluxerr_aper_2` Float32,                             -- no index
    `mag_aper_2` Float32,                                 -- [→PROJ M] aperture-magnitude cut
    `magerr_aper_2` Float32,                              -- no index
    `flux_aper_3` Float32,                                -- no index
    `fluxerr_aper_3` Float32,                             -- no index
    `mag_aper_3` Float32,                                 -- [→PROJ H] main aperture magnitude; minmax dropped
    `magerr_aper_3` Float32,                              -- [→PROJ L] effectively an S/N cut for the main aperture
    `flux_psf` Float32,                                   -- no index
    `fluxerr_psf` Nullable(Float32),                      -- no index
    `mag_psf` Float32,                                    -- [→PROJ H] primary magnitude, hottest float cut; minmax dropped
    `magerr_psf` Float32,                                 -- [→PROJ M] quality; pairs with mag/S-N
    `back_im` Nullable(Float32),                          -- [→PROJ L] local background quality cut
    `var_im` Nullable(Float32),                           -- [→PROJ L] local variance (bright-star artifact test)
    `back_annulus` Float32,                               -- derived; no index
    `std_annulus` Float32,                                -- [→PROJ L] pairs with var_im in bogus-source test
    -- `nodenumb` Int32,                                  -- [REMOVED] per-image constant → join from proc_images (see caveat*)
    -- `mountnum` Int32,                                  -- [REMOVED] per-image constant → join from proc_images (see caveat*)
    -- `camnum` Int32,                                    -- [REMOVED] per-image constant → join from proc_images (see caveat*)
    -- `cropid` Int32,                                    -- [REMOVED] per-crop constant → implied by id_proc_im (join)
    `baryvel` Nullable(Float32) COMMENT 'barycentric velocity',   -- per-source; no index
    `ingestion_time` Nullable(DateTime64(3)) DEFAULT now(),       -- [→PROJ L] ops "recently ingested"; minmax weak (insert order != sort order)
    `ingestion_time_jd` Float64,                          -- [→PROJ L] ops; same as above

    -- id: was MATERIALIZED (stored 16 B/row ≈ 1.6 TB @100B). ALIAS = computed on read, still usable in WHERE.
    -- If you look up by combined id, either keep the two blooms (query id_proc_im + id_proc_src), or revert to
    -- MATERIALIZED and add: INDEX idx_id id TYPE bloom_filter(0.01) GRANULARITY 1.
    `id` UInt128 ALIAS bitOr(bitShiftLeft(CAST(id_proc_im, 'UInt128'), 64), CAST(id_proc_src, 'UInt128')),  -- [ALIAS]

    -- ---- Skip indexes -------------------------------------------------------
    -- Exact-match id columns → bloom_filter (was minmax; minmax pruned nothing here)
    INDEX idx_id_proc_src id_proc_src TYPE bloom_filter(0.01) GRANULARITY 1,
    INDEX idx_id_uniq_src id_uniq_src TYPE bloom_filter(0.01) GRANULARITY 1,
    INDEX idx_id_cat_src  id_cat_src  TYPE bloom_filter(0.01) GRANULARITY 1,
    INDEX idx_id_proc_im  id_proc_im  TYPE bloom_filter(0.01) GRANULARITY 1,
    -- Spatial helper (correlates with HEALPix sort). Lower GRANULARITY = finer pruning.
    INDEX idx_upix_low upix_low TYPE minmax GRANULARITY 16,
    -- Position: cheap minmax helpers; cone search is served by the PK.
    INDEX idx_ra  ra  TYPE minmax GRANULARITY 16,   -- weak near RA 0/360 wrap
    INDEX idx_dec dec TYPE minmax GRANULARITY 16
    -- DROPPED vs original: idx_upix_high (sort-key head, PK covers it),
    --   idx_is_forced, idx_jd, idx_nepoch, idx_sn, idx_mag_aper_3, idx_mag_psf,
    --   idx_id_raw_im  (all minmax → useless on scattered cols, or column removed)

    -- ---- Projections: BUILD NOW (H priority) --------------------------------
    -- Lightweight _part_offset projections: ~few % storage, granule-level pruning on 26.6.
    ,PROJECTION proj_mag_psf    (SELECT _part_offset ORDER BY mag_psf)
    ,PROJECTION proj_sn         (SELECT _part_offset ORDER BY sn)
    ,PROJECTION proj_mag_aper_3 (SELECT _part_offset ORDER BY mag_aper_3)

    -- ---- Projections: BUILD WHEN JUSTIFIED (M) — uncomment per query logs ----
    -- ,PROJECTION proj_mag_aper_1 (SELECT _part_offset ORDER BY mag_aper_1)
    -- ,PROJECTION proj_mag_aper_2 (SELECT _part_offset ORDER BY mag_aper_2)
    -- ,PROJECTION proj_magerr_psf (SELECT _part_offset ORDER BY magerr_psf)
    -- ,PROJECTION proj_nepoch     (SELECT _part_offset ORDER BY nepoch)
    -- ,PROJECTION proj_is_forced  (SELECT _part_offset ORDER BY is_forced)   -- or split forced photometry into its own table/partition
    -- Composite for the joint "clean detection" filter (serves sn+mag together, read directly):
    -- ,PROJECTION proj_sn_mag     (SELECT _part_offset ORDER BY (sn, mag_psf))
    -- S/N family (only if filtered): proj_sn_1, proj_sn_2, proj_sn_delta ... same pattern.

    -- ---- Projections: DEFER (L) ---------------------------------------------
    -- x2/y2 (morphology), sn_3/4/5, sn_ext1/2, back_im, var_im, std_annulus,
    -- magerr_aper_3, ingestion_time* — build a projection only for a proven query.
)
ENGINE = MergeTree
PARTITION BY upix_partition                               -- verify this yields hundreds–few thousand partitions, not 100k+
ORDER BY (upix_high, jd)                                  -- CHANGED from `ORDER BY upix_high`: jd folded in → light curves contiguous & time-ordered, cone+time PK-served
SETTINGS storage_policy = 'pipeline_policy', index_granularity = 8192

-- ============================================================================
-- *CAVEAT on nodenumb/mountnum/camnum/cropid removal:
--    If each sky field is ALWAYS observed by a fixed (mountnum, camnum), these
--    correlate with position → they compress well inline and a cheap
--    set()/minmax would prune. In that case keeping them (with a set index) can
--    beat a join. If field→telescope is NOT fixed, removal + join is strictly
--    better. Decide from your field-assignment scheme.
--
-- Backfilling projections/indexes on existing 100B-row parts is a background
-- mutation (ALTER TABLE ... MATERIALIZE PROJECTION / INDEX). Watch system.mutations.
-- Verify every projection is actually used:  EXPLAIN projections = 1  SELECT ...
-- ============================================================================
