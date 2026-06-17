# HEALPix Cone Search — Diagrams

All diagrams use [Mermaid](https://mermaid.js.org/).  
Render with any Mermaid-aware viewer (GitHub, VS Code extension, mermaid.live).

---

## 1. End-to-End Data Flow

```mermaid
flowchart TD
    A([User: RA, Dec, Radius\ntable, column, algo]) --> B

    B[Validate inputs\nRA ∈ 0..360\nDec ∈ -90..90\nRadius > 0]
    B --> C

    C[Choose NSideSearch\n= 2^floor log2 sqrt3/radius_rad\ncapped at NSide_CAT = 65536]
    C --> D

    D{Algo?}

    D -->|NEIGHBOR| E[ang2pix at NSideSearch\n→ centre pixel\n→ get_all_neighbours\n→ centre + up to 8 neighbours\n≤ 9 pixels total]

    D -->|CONE| F[query_disc at NSideSearch\ninclusive=False\n→ pixels whose centres\nare inside the cone]

    F --> G{Any pixels\nreturned?}
    G -->|No: sub-pixel radius| H[Fallback: single pixel\nat NSideSearch]
    G -->|Yes| I
    H --> I

    E --> I[pixels_to_ranges\nnchild = NSide_CAT÷NSideSearch ²\nlo = pix × nchild\nhi = lo + nchild - 1]

    I --> J[Sort by lo\nMerge adjacent/overlapping ranges]
    J --> K[PixelRanges object\nlist of lo hi tuples]

    K --> L[Build SQL\nSELECT cols FROM table\nWHERE col BETWEEN lo AND hi\nOR ...]

    K --> M{post_filter?}
    M -->|cosine| N[Direction cosines\ncx0 cy0 cz0 = f RA Dec\ncos_r = cos radius\nAND cx×cx0 + cy×cy0 + cz×cz0 ≥ cos_r]
    M -->|greatcircle| O[AND greatCircleAngle\nra dec RA0 Dec0 ≤ radius]
    M -->|None| P[ ]

    L --> Q([Return: sql_string\npost_filter_string])
    N --> Q
    O --> Q
```

---

## 2. NSideSearch Selection

```mermaid
flowchart LR
    A([radius_deg]) --> B[radius_rad = radians radius_deg]
    B --> C[ideal = sqrt 3 ÷ radius_rad]
    C --> D[level = floor log2 ideal]
    D --> E[nside = 2^level]
    E --> F{nside > NSIDE_CAT?}
    F -->|Yes| G[nside = NSIDE_CAT]
    F -->|No| H{nside < 1?}
    H -->|Yes| I[nside = 1]
    H -->|No| J([NSideSearch])
    G --> J
    I --> J
```

---

## 3. NESTED Pixel Range Expansion

```mermaid
flowchart TD
    A([low-NSide pixel P\nnside_search]) --> B
    B[factor = NSIDE_CAT ÷ nside_search]
    B --> C[nchild = factor²]
    C --> D[lo = P × nchild]
    C --> E[hi = lo + nchild - 1]
    D --> F([Range: lo .. hi\nat level 16])
    E --> F

    subgraph Example["Example: P=5, nside_search=NSide_CAT÷4, factor=4, nchild=16"]
        G[lo = 5 × 16 = 80]
        H[hi = 80 + 16 - 1 = 95]
        I([Range: 80 .. 95])
        G --> I
        H --> I
    end
```

---

## 4. Range Merging

```mermaid
flowchart TD
    A([sorted lo hi pairs]) --> B[cur_lo = first lo\ncur_hi = first hi]
    B --> C{next range?}
    C -->|yes: lo hi| D{lo ≤ cur_hi + 1?}
    D -->|yes: adjacent or overlapping| E[cur_hi = max cur_hi hi]
    E --> C
    D -->|no: gap| F[emit cur_lo cur_hi\ncur_lo = lo\ncur_hi = hi]
    F --> C
    C -->|no| G[emit cur_lo cur_hi]
    G --> H([merged ranges])
```

---

## 5. Cross-Platform Backend Selection

```mermaid
flowchart TD
    A([get_backend called]) --> B{Already loaded?}
    B -->|yes| C([return cached backend])
    B -->|no| D{platform.system?}
    D -->|Windows| E[Load astropy_healpix backend]
    D -->|Linux / other| F{import healpy?}
    F -->|success| G[Load healpy backend]
    F -->|ImportError| E
    E --> H([_AstropyBackend\nang2pix_nested\nquery_disc_nested\nneighbours_nested\npix2ang_nested])
    G --> I([_HealpyBackend\nang2pix_nested\nquery_disc_nested\nneighbours_nested\npix2ang_nested])
    H --> J([cache and return])
    I --> J
```

---

## 6. NEIGHBOR vs CONE Coverage

```mermaid
block-beta
    columns 3

    block:neighbor["NEIGHBOR algo"]:1
        N1["Central pixel"]
        N2["8 neighbours"]
        N3["Always ≤ 9 pixels\nat NSideSearch"]
        N4["Expand to level-16\n→ ≤ 9 ranges"]
        N5["Over-inclusive:\ncorners can be\n2–3× radius away"]
    end

    space

    block:cone["CONE algo"]:1
        C1["query_disc\ninclusive=False"]
        C2["Only pixels whose\ncentres are inside cone"]
        C3["Typically 4–8 pixels\nat NSideSearch"]
        C4["Expand + merge\n→ fewer ranges"]
        C5["Tighter:\nalmost all candidates\nare genuine"]
    end
```

---

## 7. ClickHouse Query Execution Path

```mermaid
sequenceDiagram
    participant App
    participant CHClient as ClickHouse Client
    participant CHServer as ClickHouse Server
    participant Index as Sparse Index on upix_high
    participant Data as MergeTree Data Parts

    App->>CHClient: cone_search_sql(RA, Dec, R, ...)
    CHClient->>CHClient: generate SQL with BETWEEN ranges
    CHClient->>CHServer: SELECT * FROM proc_src WHERE (upix_high BETWEEN lo1 AND hi1) OR ...

    CHServer->>Index: lookup lo1..hi1 → granule offsets
    Index-->>CHServer: granule list (O(ranges) lookups)

    CHServer->>Data: read only relevant granules
    Data-->>CHServer: candidate rows (superset of cone)

    CHServer->>CHServer: apply post-filter\n(dot product or greatCircleAngle)
    CHServer-->>CHClient: exact cone results

    CHClient-->>App: result rows
```

---

## 8. Module Structure

```mermaid
classDiagram
    class healpix_cone_search {
        +HEALPIX_LEVEL_CAT = 16
        +NSIDE_CAT = 65536
        +MAX_PIX_ID = 51539607551
    }

    class Algo {
        <<enumeration>>
        NEIGHBOR
        CONE
    }

    class PixelRanges {
        +ranges: List[Tuple[int,int]]
        +nside_search: int
        +algo: Algo
        +n_search_pixels: int
        +n_ranges: int
    }

    class _HealpyBackend {
        +name = "healpy"
        +ang2pix_nested(nside, ra, dec) int
        +query_disc_nested(nside, ra, dec, r) ndarray
        +neighbours_nested(nside, pix) ndarray
        +pix2ang_nested(nside, pix) tuple
    }

    class _AstropyBackend {
        +name = "astropy_healpix"
        +ang2pix_nested(nside, ra, dec) int
        +query_disc_nested(nside, ra, dec, r) ndarray
        +neighbours_nested(nside, pix) ndarray
        +pix2ang_nested(nside, pix) tuple
    }

    class PublicAPI {
        +get_backend()
        +cone_to_pixel_ranges(ra,dec,r,algo) PixelRanges
        +cone_search_sql(ra,dec,r,table,col,...) tuple
        +cone_search_sql_full(ra,dec,r,table,col,...) str
    }

    healpix_cone_search --> Algo
    healpix_cone_search --> PixelRanges
    healpix_cone_search --> _HealpyBackend
    healpix_cone_search --> _AstropyBackend
    healpix_cone_search --> PublicAPI
    PixelRanges --> Algo
    PublicAPI --> PixelRanges
```
