/*
ClickHouse image table templates
================================

Purpose:
- Show only important columns, keys, partitions, projections, and optional indexes.
- Replace placeholders before use.
- Do not run against production without benchmarking and review.
*/

/* -------------------------------------------------------------------------
1. Full-frame/raw image metadata
--------------------------------------------------------------------------- */

CREATE TABLE {database}.{raw_images_table}
(
    image_id UInt64,
    exposure_id UInt64,
    dateobs DateTime64(3, 'UTC'),

    camera_id UInt16,
    mount_id UInt16,
    field_id UInt32,
    filter_id LowCardinality(String),

    ra Float64,
    dec Float64,
    upix_low UInt64,
    upix_high UInt64,

    quality_status UInt8,
    processing_version UInt32,

    /* additional raw-image metadata columns */

    PROJECTION prj_time_camera
    (
        SELECT _part_offset
        ORDER BY (dateobs, camera_id, image_id)
    ),

    /* Optional: add only after a field-query benchmark.
    PROJECTION prj_field_time
    (
        SELECT _part_offset
        ORDER BY (field_id, dateobs, camera_id, image_id)
    )
    */
)
ENGINE = MergeTree
PARTITION BY toYYYYMM(dateobs)
ORDER BY image_id
SETTINGS
    storage_policy = '{storage_policy}',
    index_granularity = 8192;


/* -------------------------------------------------------------------------
2. Processed/crop/derived image metadata
--------------------------------------------------------------------------- */

CREATE TABLE {database}.{proc_images_table}
(
    image_id UInt64,
    exposure_id UInt64,
    parent_image_id UInt64,
    raw_image_id UInt64,

    dateobs DateTime64(3, 'UTC'),
    camera_id UInt16,
    crop_id UInt32,
    field_id UInt32,
    filter_id LowCardinality(String),

    ra Float64,
    dec Float64,
    upix_low UInt64,
    upix_high UInt64,

    processing_version UInt32,
    quality_status UInt8,

    /* additional proc/visit/ref/diff metadata columns */

    PROJECTION prj_time_camera
    (
        SELECT _part_offset
        ORDER BY (dateobs, camera_id, image_id)
    ),

    PROJECTION prj_parent_image
    (
        SELECT _part_offset
        ORDER BY (parent_image_id, crop_id, image_id)
    )

    /* Optional: add only after a sky-query benchmark.
    ,PROJECTION prj_sky_time
    (
        SELECT _part_offset
        ORDER BY (upix_low, dateobs, image_id)
    )
    */

    /* Optional: occasional exact lineage lookup.
       Prefer a projection if the lookup is frequent.
    ,INDEX idx_raw_image_id raw_image_id
        TYPE bloom_filter
        GRANULARITY 4
    */
)
ENGINE = MergeTree
PARTITION BY toYYYYMM(dateobs)
ORDER BY image_id
SETTINGS
    storage_policy = '{storage_policy}',
    index_granularity = 8192;


/* -------------------------------------------------------------------------
3. Alternative crop layout when exposure-centric retrieval dominates

Use this only after benchmark evidence.
Add prj_image_id so exact joins/lookups still have an efficient path.
--------------------------------------------------------------------------- */

/*
CREATE TABLE {database}.{proc_images_exposure_ordered_table}
(
    image_id UInt64,
    exposure_id UInt64,
    parent_image_id UInt64,
    dateobs DateTime64(3, 'UTC'),
    camera_id UInt16,
    crop_id UInt32,

    /* additional columns *\/

    PROJECTION prj_image_id
    (
        SELECT _part_offset
        ORDER BY image_id
    ),

    PROJECTION prj_time_camera
    (
        SELECT _part_offset
        ORDER BY (dateobs, camera_id, image_id)
    )
)
ENGINE = MergeTree
PARTITION BY toYYYYMM(dateobs)
ORDER BY (exposure_id, camera_id, crop_id, image_id)
SETTINGS
    storage_policy = '{storage_policy}',
    index_granularity = 8192;
*/


/* -------------------------------------------------------------------------
4. Source-table support for image joins

Apply only to a source table whose base ORDER BY must remain sky-oriented.
The image_id column must already exist and match the image table's type.
--------------------------------------------------------------------------- */

/*
ALTER TABLE {database}.{source_table}
ADD PROJECTION IF NOT EXISTS prj_image_id
(
    SELECT _part_offset
    ORDER BY image_id
);

ALTER TABLE {database}.{source_table}
MATERIALIZE PROJECTION prj_image_id
SETTINGS mutations_sync = 1;
*/


/* -------------------------------------------------------------------------
5. Join/query placeholders
--------------------------------------------------------------------------- */

/* Source-selective query: retrieve image metadata. */
/*
SELECT
    s.image_id,
    s.source_id,
    i.dateobs,
    i.camera_id
FROM {database}.{source_table} AS s
ANY INNER JOIN {database}.{image_table} AS i
    ON s.image_id = i.image_id
WHERE
    {selective_source_predicate};
*/


/* Image-selective query: filter images first, then retrieve sources. */
/*
SELECT
    s.image_id,
    count()
FROM {database}.{source_table} AS s
INNER JOIN
(
    SELECT image_id
    FROM {database}.{image_table}
    WHERE
        dateobs >= {start_time:DateTime64}
        AND dateobs < {end_time:DateTime64}
        AND camera_id = {camera_id:UInt16}
) AS i
ON s.image_id = i.image_id
GROUP BY s.image_id;
*/


/* Filtering only: benchmark IN against JOIN. */
/*
SELECT count()
FROM {database}.{source_table} AS s
WHERE s.image_id IN
(
    SELECT image_id
    FROM {database}.{image_table}
    WHERE {selective_image_predicate}
);
*/


/* -------------------------------------------------------------------------
6. Validation placeholders
--------------------------------------------------------------------------- */

/*
EXPLAIN indexes = 1, projections = 1
SELECT *
FROM {database}.{image_table}
WHERE image_id = {image_id:UInt64};

EXPLAIN indexes = 1, projections = 1
SELECT *
FROM {database}.{image_table}
WHERE
    dateobs >= {start_time:DateTime64}
    AND dateobs < {end_time:DateTime64}
    AND camera_id = {camera_id:UInt16};

SELECT
    projection,
    sum(rows) AS rows,
    formatReadableSize(sum(bytes_on_disk)) AS size
FROM system.projection_parts
WHERE
    database = '{database}'
    AND table = '{image_table}'
    AND active
GROUP BY projection
ORDER BY projection;
*/
