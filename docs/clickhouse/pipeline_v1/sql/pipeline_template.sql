-- TEMPLATE: visit pipeline tables (raw → visit image → visit sources).
-- Skeleton only: add science/metadata columns before production use.

/* ------------------------------------------------------------------ raw_images */
CREATE TABLE IF NOT EXISTS last.raw_images
(
    `id_raw` Int64,                          -- pipeline image id (time-encoded)
    `jd` Float64,
    `dateobs` DateTime64(3, 'UTC'),          -- partition key; derived from jd at ingest
    `ra` Float64,
    `dec` Float64,
    `upix_low` UInt64,
    `upix_high` UInt64,
    `mountnum` Int8,
    `camnum` Int8,
    `filter` String,
    `exptime` Float32,

    INDEX idx_jd jd TYPE minmax GRANULARITY 16  -- jd ~ monotonic with ORDER BY id_raw
)
ENGINE = MergeTree
PARTITION BY toYYYYMM(dateobs)
ORDER BY id_raw
SETTINGS index_granularity = 8192;
-- SETTINGS storage_policy = 'pipeline_policy';


/* ---------------------------------------------------------------- visit_images */
CREATE TABLE IF NOT EXISTS last.visit_images
(
    `id_visit` Int64,                        -- visit/coadd product id
    `id_raw` Int64,                          -- FK → raw_images.id_raw
    `id_dark` Int64,
    `id_flat` Int64,
    `jd` Float64,
    `dateobs` DateTime64(3, 'UTC'),
    `ra` Float64,
    `dec` Float64,
    `upix_low` UInt64,
    `upix_high` UInt64,
    `filter` String,
    `exptime` Float32

    /* add WCS, photometry, quality, coadd metadata */
)
ENGINE = MergeTree
PARTITION BY toYYYYMM(dateobs)
ORDER BY id_visit
SETTINGS index_granularity = 8192;
-- SETTINGS storage_policy = 'pipeline_policy';


/* ------------------------------------------------------------------- visit_src */
CREATE TABLE IF NOT EXISTS last.visit_src
(
    `id_visit_src` Int64,                    -- running number per image (pipeline)
    `id_visit_im` Int64,                     -- FK → visit_images.id_visit
    `id_uniq_src` Int64,                     -- unused in current pipeline (see PIPELINE_TEMPLATE.md)
    `id_cat_src` Int64,                      -- unused in current pipeline
    `upix_low` UInt64,
    `upix_high` UInt64,
    `jd` Float64,
    `dateobs` DateTime64(3, 'UTC'),
    `ra` Float64,
    `dec` Float64,
    `sn` Float32,
    `mag_psf` Float32,                       -- semi-random vs on-disk order → projection, not minmax

    `id` UInt128 MATERIALIZED bitOr(
        bitShiftLeft(CAST(id_visit_im, 'UInt128'), 64),
        CAST(id_visit_src, 'UInt128')),

    PROJECTION prj_image
    (
        SELECT _part_offset
        ORDER BY id_visit_im
    ),
    PROJECTION prj_mag_psf
    (
        SELECT _part_offset
        ORDER BY mag_psf
    )

    /* add apertures, flags, forced phot, ingestion columns */
)
ENGINE = MergeTree
PARTITION BY toYYYYMM(dateobs)
ORDER BY upix_high
SETTINGS index_granularity = 8192;
-- SETTINGS storage_policy = 'pipeline_policy';
