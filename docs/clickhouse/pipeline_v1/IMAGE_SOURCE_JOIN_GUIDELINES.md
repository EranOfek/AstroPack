# Image and Source Join Guidelines

## Goal

Define practical query patterns for joining ULTRASAT and LAST source tables to image metadata tables without forcing full scans of very large source datasets.

---

# Relationship model

Use the same join key in both tables:

```sql
images.image_id UInt64
sources.image_id UInt64
```

For crop-level products, keep both product and parent relationships:

```sql
image_id UInt64,
parent_image_id UInt64,
exposure_id UInt64,
crop_id UInt32
```

A source row should reference the exact image product from which it was measured.

---

# Cardinality expectations

| Dataset | Image rows | Source rows | Join implication |
|---|---:|---:|---|
| ULTRASAT full frames | ~631 thousand | billions | image side is small |
| ULTRASAT crops | ~216.69 million | billions | filtered image side is preferred |
| LAST camera frames | ~189.35 million | tens of billions | avoid unfiltered whole-table hash joins |
| LAST crops/products | ~43.34 billion | tens to hundreds of billions | both sides can be large; prune first |

Do not assume every image table is a small dimension table.

---

# Query pattern 1: source → image metadata

Use when the source predicate is already selective:

```sql
SELECT
    s.image_id,
    s.source_id,
    i.dateobs,
    i.camera_id
FROM source_table AS s
ANY INNER JOIN image_table AS i
    ON s.image_id = i.image_id
WHERE
    /* selective source predicate */;
```

The image table should be ordered by `image_id`.

---

# Query pattern 2: image metadata → matching sources

Use when time, camera, field, or quality selects a small image set:

```sql
SELECT
    s.image_id,
    count()
FROM source_table AS s
INNER JOIN
(
    SELECT image_id
    FROM image_table
    WHERE
        dateobs >= {start_time:DateTime64}
        AND dateobs < {end_time:DateTime64}
        AND camera_id = {camera_id:UInt16}
) AS i
ON s.image_id = i.image_id
GROUP BY s.image_id;
```

The source table must have either:

```text
ORDER BY beginning with image_id
```

or:

```text
a lightweight projection ordered by image_id
```

Otherwise, the image table may be filtered efficiently while the source table still performs a very large scan.

---

# Query pattern 3: filtering only

When no image columns are returned, benchmark `IN`:

```sql
SELECT count()
FROM source_table AS s
WHERE s.image_id IN
(
    SELECT image_id
    FROM image_table
    WHERE
        /* selective image predicate */
);
```

This is often simpler and can be faster than a general join.

---

# Query pattern 4: parent image → crops/products

```sql
SELECT
    image_id,
    crop_id,
    dateobs
FROM proc_images
WHERE parent_image_id = {parent_image_id:UInt64}
ORDER BY crop_id;
```

Support this with:

```sql
PROJECTION prj_parent_image
(
    SELECT _part_offset
    ORDER BY (parent_image_id, crop_id, image_id)
)
```

---

# Query pattern 5: image lineage

```sql
SELECT
    p.image_id AS proc_image_id,
    r.image_id AS raw_image_id,
    p.processing_version
FROM proc_images AS p
ANY INNER JOIN raw_images AS r
    ON p.raw_image_id = r.image_id
WHERE p.image_id = {image_id:UInt64};
```

Keep lineage keys as integers and avoid nullable joins.

---

# Join algorithm guidance

## Hash join

Good when the filtered right side is small enough for memory.

Place the smaller filtered image result on the right:

```sql
source_table AS s
JOIN (SELECT ... FROM image_table WHERE ...) AS i
```

## Full sorting merge join

Benchmark when both sides are large and sorted by the join key.

This is most relevant when both source and image tables have an access path ordered by `image_id`.

## Direct join

Consider only for a genuinely small, current-state image lookup represented as a dictionary or key-value engine. A 43-billion-row crop table is not an in-memory dictionary candidate.

## Automatic selection

Use the default/automatic algorithm as a baseline, but log the chosen algorithm and peak memory. Do not assume the same algorithm is optimal for every image-table class.

---

# Denormalization policy

Copy small immutable columns into source tables only when they are used by most queries:

```text
dateobs
camera_id
field_id
filter_id
upix_low
```

Benefits:

- fewer joins;
- better source-table pruning;
- simpler science queries.

Costs:

- repeated storage;
- schema duplication;
- harder corrections when metadata changes.

Keep large or mutable image metadata only in image tables.

---

# Benchmark matrix

For each image-table class, test:

| Test | Image-set size | Expected purpose |
|---|---:|---|
| Exact image | 1 | point lookup and one-image source retrieval |
| Small batch | 10 | interactive comparison |
| Medium batch | 100 | visit/night subset |
| Large batch | 1,000 | batch processing |
| Night filter | variable | operational workflow |
| Camera-night filter | variable | LAST camera workflow |
| Field-time filter | variable | survey workflow |
| Sky-time filter | variable | astronomy workflow |
| Parent lookup | all crops of one exposure | lineage/product retrieval |

For each test capture:

```text
elapsed time
read_rows
read_bytes
result rows
selected parts/granules
projection used
join algorithm
peak memory
```

Compare:

```text
JOIN
ANY INNER JOIN
IN subquery
projections enabled
projections disabled
```

---

# Correctness rules

- Ensure one canonical image row per `image_id`, or select an explicit version.
- Use `ANY` only when one matching image row is semantically correct.
- Confirm source and image tables use identical ID types.
- Prevent duplicate image rows from multiplying source results.
- Test missing-image behavior with `LEFT JOIN` separately.
- Do not use `FINAL` routinely as a substitute for a clean current-state model.

---

# Initial recommendation

1. Order image tables by `image_id`.
2. Partition image tables monthly by `dateobs`.
3. Add source-table access by `image_id`.
4. Filter images before joining.
5. Use `IN` when image columns are not required.
6. Denormalize only the few stable image attributes used in most source queries.
7. Benchmark join algorithms with representative ULTRASAT and LAST cardinalities.
