# ***************************************************************************
# Project     : AstroPack
# Filename    : healpix_cone_search.py
# Author      : Chen Tishler
# Created     : 17/06/2026
# Modified    : 17/06/2026
# Description : Cone-search → ClickHouse SQL generator using HEALPix NESTED ordering.
# ***************************************************************************


"""
healpix_cone_search.py
======================
Cone-search → ClickHouse SQL generator using HEALPix NESTED ordering.

Assumptions
-----------
* Sky positions stored as HEALPix NESTED pixel index at **level 16**
  (NSide = 2**16 = 65 536, pixel size ≈ 3.2 arcsec).
* The healpix column in ClickHouse is the 64-bit unsigned integer pixel id.
* Cross-platform:
    - Linux  → healpy   (fast C extension)
    - Windows → astropy_healpix (pure Python / no C deps)

Algorithm choices (Algo enum)
------------------------------
NEIGHBOR
    Central pixel + 8 neighbours at NSideSearch, then expand to NSideCat.
    Always returns ≤ 9 ranges. Fast, conservative (over-inclusive).

CONE
    query_disc / cone_search at NSideSearch, then expand to NSideCat.
    Returns fewer, tighter ranges. Slightly slower at query-plan time.

Usage
-----
    from healpix_cone_search import cone_search_sql, Algo

    sql, post = cone_search_sql(
        ra=254.0, dec=64.0, radius_deg=1.0,
        table="proc_src", column="upix_high",
        algo=Algo.CONE,
    )
    print(sql)
    print(post)   # optional exact-distance post-filter fragment
"""

import math
import platform
import sys
from dataclasses import dataclass, field
from enum import Enum
from typing import List, Optional, Tuple

import numpy as np

# ---------------------------------------------------------------------------
# Platform-aware HEALPix backend
# ---------------------------------------------------------------------------

def _is_windows() -> bool:
    # healpy requires compiled C extensions that are not available via pip on
    # Windows, so we select the backend based on the detected OS.
    return platform.system() == "Windows"


def _load_backend():
    """Return a namespace with ang2pix, query_disc, neighbours functions."""
    if _is_windows():
        # Windows: always use the pure-Python astropy_healpix backend
        return _AstropyBackend()
    else:
        try:
            # Linux/Mac: prefer healpy (10-100x faster for large catalogues)
            return _HealpyBackend()
        except ImportError:
            # healpy not installed → fall back gracefully
            return _AstropyBackend()


class _HealpyBackend:
    name = "healpy"

    def __init__(self):
        import healpy as hp  # noqa: F401
        self._hp = hp

    def ang2pix_nested(self, nside: int, ra_deg: float, dec_deg: float) -> int:
        """RA/Dec degrees → nested pixel index."""
        # healpy uses physics convention: theta = colatitude from north pole,
        # phi = azimuth (= RA).  Dec = 90° - theta, so theta = 90° - Dec.
        theta = math.radians(90.0 - dec_deg)
        phi   = math.radians(ra_deg)
        return int(self._hp.ang2pix(nside, theta, phi, nest=True))

    def query_disc_nested(self, nside: int, ra_deg: float, dec_deg: float,
                          radius_deg: float) -> np.ndarray:
        """Return nested pixel indices within radius_deg of (ra_deg, dec_deg)."""
        hp = self._hp
        # ang2vec also takes (theta, phi) in physics convention
        vec = hp.ang2vec(math.radians(90.0 - dec_deg), math.radians(ra_deg))
        # inclusive=False: only pixels whose *centres* lie strictly inside the
        # cone.  inclusive=True would also grab pixels that merely touch the
        # boundary, causing more false positives.
        pix = hp.query_disc(nside, vec, math.radians(radius_deg),
                            nest=True, inclusive=False)
        return pix.astype(np.int64)

    def neighbours_nested(self, nside: int, pix: int) -> np.ndarray:
        """Return 8 neighbours + self (nested). -1 entries removed."""
        hp = self._hp
        neighb = hp.get_all_neighbours(nside, pix, nest=True)
        # healpy uses -1 as a sentinel for missing neighbours (e.g. at poles
        # where a pixel has fewer than 8 neighbours)
        valid  = neighb[neighb >= 0]
        # always include the centre pixel itself so the search area is fully covered
        return np.unique(np.append(valid, pix)).astype(np.int64)

    def pix2ang_nested(self, nside: int, pix: int) -> Tuple[float, float]:
        """nested pixel → (ra_deg, dec_deg)."""
        theta, phi = self._hp.pix2ang(nside, pix, nest=True)
        return math.degrees(phi), 90.0 - math.degrees(theta)


class _AstropyBackend:
    name = "astropy_healpix"

    def __init__(self):
        import astropy_healpix as ah
        from astropy_healpix import HEALPix   # OO interface used for cone search
        from astropy import units as u
        self._ah = ah
        self._HEALPix = HEALPix
        self._u = u

    def ang2pix_nested(self, nside: int, ra_deg: float, dec_deg: float) -> int:
        ah = self._ah
        # lonlat_to_healpix takes (lon, lat) = (RA, Dec) as Quantity objects
        return int(ah.lonlat_to_healpix(
            self._u.Quantity(ra_deg,  unit="deg"),
            self._u.Quantity(dec_deg, unit="deg"),
            nside, order="nested"))

    def query_disc_nested(self, nside: int, ra_deg: float, dec_deg: float,
                          radius_deg: float) -> np.ndarray:
        u  = self._u
        # The cone search API lives on the HEALPix class, not on the module.
        # A new instance is cheap; nside varies per call so we can't cache it.
        hp = self._HEALPix(nside=nside, order="nested")
        # cone_search_lonlat returns indices of pixels whose centres are inside
        # the cone (same semantics as healpy's inclusive=False)
        pix = hp.cone_search_lonlat(ra_deg * u.deg, dec_deg * u.deg,
                                    radius=radius_deg * u.deg)
        return pix.astype(np.int64)

    def neighbours_nested(self, nside: int, pix: int) -> np.ndarray:
        ah = self._ah
        # astropy_healpix v1.1+ signature: neighbours(healpix_index, nside, ...)
        # The pixel index comes FIRST — opposite of what one might expect from
        # the module name order.  Passing (nside, pix) would raise
        # "nside must be a power of two" because nside is misread as pix.
        neighb = ah.neighbours(np.array([pix], dtype=np.int64), nside,
                               order="nested").flatten()
        # -1 sentinels mark missing neighbours at map boundaries / poles
        valid  = neighb[neighb >= 0]
        # include the centre pixel so it is always covered
        return np.unique(np.append(valid, pix)).astype(np.int64)

    def pix2ang_nested(self, nside: int, pix: int) -> Tuple[float, float]:
        ah  = self._ah
        u   = self._u
        lon, lat = ah.healpix_to_lonlat(
            np.array([pix], dtype=np.int64), nside, order="nested")
        return float(lon.to(u.deg).value[0]), float(lat.to(u.deg).value[0])


# Singleton backend — loaded once on first use, reused for all subsequent calls
_BACKEND = None

def get_backend():
    global _BACKEND
    if _BACKEND is None:
        _BACKEND = _load_backend()
    return _BACKEND


# ---------------------------------------------------------------------------
# Constants
# ---------------------------------------------------------------------------

HEALPIX_LEVEL_CAT = 16
NSIDE_CAT         = 2 ** HEALPIX_LEVEL_CAT   # 65 536  (pixel size ≈ 3.2 arcsec)
MAX_PIX_ID        = 12 * NSIDE_CAT * NSIDE_CAT - 1   # 51 539 607 551 — requires UInt64


# ---------------------------------------------------------------------------
# Public types
# ---------------------------------------------------------------------------

class Algo(str, Enum):
    NEIGHBOR = "neighbor"   # central + 8 neighbours, always ≤ 9 ranges
    CONE     = "cone"       # query_disc, fewer ranges, more accurate


@dataclass
class PixelRanges:
    """List of [lo, hi] inclusive pixel-id ranges at NSide = NSIDE_CAT."""
    ranges: List[Tuple[int, int]] = field(default_factory=list)
    nside_search: int = 0
    algo: Algo = Algo.CONE
    n_search_pixels: int = 0      # how many low-NSide pixels were found

    @property
    def n_ranges(self) -> int:
        return len(self.ranges)

    def __repr__(self):
        lines = [
            f"PixelRanges(algo={self.algo.value}, "
            f"nside_search={self.nside_search}, "
            f"search_pixels={self.n_search_pixels}, "
            f"ranges={self.n_ranges}):"
        ]
        for lo, hi in self.ranges:
            lines.append(f"  [{lo:>14d}, {hi:>14d}]  (width {hi-lo+1})")
        return "\n".join(lines)


# ---------------------------------------------------------------------------
# Core helpers
# ---------------------------------------------------------------------------

def _best_nside_for_radius(radius_deg: float,
                           mode: str = "conservative") -> int:
    """
    Choose the best NSide (power of 2) for a given search radius.

    Three modes (all cap at NSIDE_CAT):

    "conservative"  [DEFAULT - Eran's preference]
        pixel_size ~ 1/NSide [rad]. We want pixel_size >= radius, so:
            NSide <= 1/radius_rad
        Result: deliberately coarse - pixel bigger than cone, so centre+
        neighbours is guaranteed to fully envelope the search area.
        "Look wider, don't miss anything."

    "area"
        Match pixel area to cone area:
            pixel area = pi/(3*NSide^2), cone area = pi*r^2
            Set equal -> NSide = 1/(sqrt(3)*r)
        Even coarser than "conservative" for most radii.

    "circumradius"
        pixel circumradius ~ sqrt(3)/NSide [rad]. Tightest coverage:
            NSide <= sqrt(3)/radius_rad
        Finer NSide -> tighter pixel grid -> fewer false positives,
        but risks the 3x3 neighbour box not fully covering the cone
        at its corners if the geometry is tight.

    Parameters
    ----------
    radius_deg : float  - search radius in degrees (> 0)
    mode       : str    - "conservative" | "area" | "circumradius"

    Returns
    -------
    int - NSide value (power of 2, 1 ... NSIDE_CAT)
    """
    radius_rad = math.radians(radius_deg)
    if radius_rad <= 0:
        raise ValueError("radius_deg must be > 0")

    if mode == "conservative":
        # pixel edge ≈ 1/NSide rad; want edge >= radius → NSide <= 1/radius
        ideal = 1.0 / radius_rad
    elif mode == "area":
        # equate HEALPix pixel area (π/3N²) with cone area (πr²) → N = 1/(√3·r)
        ideal = 1.0 / (math.sqrt(3.0) * radius_rad)
    elif mode == "circumradius":
        # pixel circumradius ≈ √3/NSide; want circumradius >= radius → NSide <= √3/r
        ideal = math.sqrt(3.0) / radius_rad
    else:
        raise ValueError(f"Unknown mode '{mode}'. "
                         f"Choose 'conservative', 'area', or 'circumradius'.")

    # Round *down* to the nearest power of 2 so the pixel is never smaller than
    # the cone (which would leave gaps between neighbours)
    level = int(math.floor(math.log2(ideal)))
    level = max(level, 0)               # guard against radius > 1 radian (huge)
    nside = min(2 ** level, NSIDE_CAT)  # never exceed the catalogue resolution
    return int(nside)


def _pixels_to_ranges(pixels: np.ndarray,
                      nside_search: int) -> List[Tuple[int, int]]:
    """
    Expand low-NSide nested pixels to ranges at NSIDE_CAT, then merge
    contiguous ranges.

    In NESTED ordering every child of a parent pixel occupies a contiguous
    block of IDs:
        lo = parent_pix * nchild
        hi = lo + nchild - 1
    where nchild = (NSIDE_CAT / nside_search) ** 2.
    """
    # Each low-resolution pixel contains (factor × factor) high-res pixels.
    # This works because NESTED ordering preserves spatial locality by
    # construction — a pixel's children always form a contiguous ID block.
    factor = NSIDE_CAT // nside_search          # integer, power of 2
    nchild = factor * factor                    # fine pixels per coarse pixel

    pixels = np.unique(pixels.astype(np.int64))  # deduplicate + sort

    # Compute the first and last fine-pixel ID for each coarse pixel
    lo_arr = pixels * nchild
    hi_arr = lo_arr + nchild - 1

    # Sort by lo so we can do a single linear-time merge pass
    order  = np.argsort(lo_arr)
    lo_arr = lo_arr[order]
    hi_arr = hi_arr[order]

    merged: List[Tuple[int, int]] = []
    cur_lo = int(lo_arr[0])
    cur_hi = int(hi_arr[0])

    for lo, hi in zip(lo_arr[1:], hi_arr[1:]):
        lo, hi = int(lo), int(hi)
        # Adjacent ranges differ by 1 (contiguous); overlapping ranges have lo <= cur_hi.
        # Both cases collapse into a single wider range.
        if lo <= cur_hi + 1:        # contiguous or overlapping → merge
            cur_hi = max(cur_hi, hi)
        else:
            merged.append((cur_lo, cur_hi))
            cur_lo, cur_hi = lo, hi
    merged.append((cur_lo, cur_hi))   # flush the last open range

    return merged


# ---------------------------------------------------------------------------
# Main search function
# ---------------------------------------------------------------------------

def cone_to_pixel_ranges(
    ra_deg: float,
    dec_deg: float,
    radius_deg: float,
    algo: Algo = Algo.CONE,
    nside_mode: str = "conservative",
) -> PixelRanges:
    """
    Convert a cone search (ra, dec, radius) to pixel ranges at level 16.

    Parameters
    ----------
    ra_deg      : Right ascension in degrees [0, 360).
    dec_deg     : Declination in degrees [-90, 90].
    radius_deg  : Search radius in degrees > 0.
    algo        : Algo.CONE (fewer ranges) or Algo.NEIGHBOR (always <= 9).
    nside_mode  : NSide selection strategy:
                  "conservative" (default, Eran's preference) - coarser,
                      never miss anything.
                  "area"         - coarsest, matches cone area to pixel area.
                  "circumradius" - finest, tightest coverage.

    Returns
    -------
    PixelRanges object with .ranges list of (lo, hi) tuples.
    """
    if not (0.0 <= ra_deg < 360.0):
        raise ValueError(f"ra_deg must be in [0, 360), got {ra_deg}")
    if not (-90.0 <= dec_deg <= 90.0):
        raise ValueError(f"dec_deg must be in [-90, 90], got {dec_deg}")
    if radius_deg <= 0:
        raise ValueError(f"radius_deg must be > 0, got {radius_deg}")

    backend      = get_backend()
    # Choose a coarse NSide whose pixel size matches (or exceeds) the search radius.
    # Searching at this coarser resolution first dramatically reduces the number
    # of pixels we need to expand into fine-level ranges.
    nside_search = _best_nside_for_radius(radius_deg, mode=nside_mode)

    if algo == Algo.NEIGHBOR:
        # Identify the central coarse pixel, then grab its 8 neighbours.
        # The 3×3 block is guaranteed to contain the full cone when nside_mode
        # is "conservative" (pixel size >= cone radius).
        center_pix = backend.ang2pix_nested(nside_search, ra_deg, dec_deg)
        pix_list   = backend.neighbours_nested(nside_search, center_pix)

    elif algo == Algo.CONE:
        # Ask the backend to find all coarse pixels whose centres are inside
        # the search cone.  This is tighter than the 3×3 NEIGHBOR block,
        # especially for small radii near pixel boundaries.
        pix_list = backend.query_disc_nested(nside_search, ra_deg, dec_deg,
                                             radius_deg)
        if len(pix_list) == 0:
            # The cone is smaller than a single coarse pixel — no centres fall
            # inside it.  Fall back to the one pixel that contains the point.
            center_pix = backend.ang2pix_nested(nside_search, ra_deg, dec_deg)
            pix_list   = np.array([center_pix], dtype=np.int64)
    else:
        raise ValueError(f"Unknown algo: {algo}")

    # Expand each coarse pixel into a contiguous block of fine-level IDs,
    # then merge adjacent blocks to minimise the number of SQL BETWEEN clauses.
    ranges = _pixels_to_ranges(pix_list, nside_search)

    return PixelRanges(
        ranges=ranges,
        nside_search=nside_search,
        algo=algo,
        n_search_pixels=len(pix_list),
    )


# ---------------------------------------------------------------------------
# SQL generator
# ---------------------------------------------------------------------------

def _direction_cosines(ra_deg: float, dec_deg: float) -> Tuple[float, float, float]:
    # Unit vector from the sphere centre toward (RA, Dec).
    # Used in the dot-product post-filter: cos(angle) = cx*CX + cy*CY + cz*CZ.
    ra  = math.radians(ra_deg)
    dec = math.radians(dec_deg)
    cx  = math.cos(dec) * math.cos(ra)
    cy  = math.cos(dec) * math.sin(ra)
    cz  = math.sin(dec)
    return cx, cy, cz


def cone_search_sql(
    ra_deg: float,
    dec_deg: float,
    radius_deg: float,
    table: str,
    column: str,
    algo: Algo = Algo.CONE,
    extra_columns: str = "*",
    post_filter: bool = True,
    post_filter_mode: str = "cosine",   # "cosine" | "greatcircle"
    # cosine mode needs these column names in the table:
    cx_col: str = "cx",
    cy_col: str = "cy",
    cz_col: str = "cz",
    # greatcircle mode needs ra/dec column names:
    ra_col:  str = "ra",
    dec_col: str = "dec",
) -> Tuple[str, Optional[str]]:
    """
    Generate a ClickHouse SELECT statement for a cone search.

    Parameters
    ----------
    ra_deg, dec_deg, radius_deg : cone centre and radius (degrees).
    table, column               : ClickHouse table and healpix column name.
    algo                        : Algo.CONE or Algo.NEIGHBOR.
    extra_columns               : columns to select (default "*").
    post_filter                 : if True, also return a post-filter fragment.
    post_filter_mode            : "cosine"  → dot-product filter (fast, no trig)
                                  "greatcircle" → ClickHouse greatCircleAngle()
    cx_col,cy_col,cz_col        : direction-cosine column names (cosine mode).
    ra_col, dec_col             : RA/Dec column names (greatcircle mode).

    Returns
    -------
    (sql_ranges_only, post_filter_fragment_or_None)

    sql_ranges_only   : complete SELECT using only healpix range filters.
    post_filter_fragment : WHERE clause fragment for exact distance check.
                           Apply this to the result of sql_ranges_only to
                           get exact cone membership.
    """
    pr = cone_to_pixel_ranges(ra_deg, dec_deg, radius_deg, algo=algo)

    # Build one "col BETWEEN lo AND hi" clause per pixel range.
    # SQL BETWEEN is inclusive on both ends, matching our [lo, hi] convention.
    range_clauses = [
        f"({column} BETWEEN {lo} AND {hi})"
        for lo, hi in pr.ranges
    ]
    # Join multiple ranges with OR — ClickHouse will evaluate these efficiently
    # against any sorted / index-structured MergeTree table.
    where_healpix = "\n   OR ".join(range_clauses)

    sql = (
        f"SELECT {extra_columns}\n"
        f"FROM {table}\n"
        f"WHERE (\n"
        f"   {where_healpix}\n"
        f")"
    )

    # --- post-filter fragment ---
    # The healpix ranges over-approximate the cone (pixel granularity).
    # A post-filter removes the false positives and delivers an exact result.
    pf: Optional[str] = None
    if post_filter:
        r_rad = math.radians(radius_deg)

        if post_filter_mode == "cosine":
            # Dot-product test: point P is inside cone C if
            #   P · C >= cos(radius)
            # All three values are precomputed in Python; ClickHouse only does
            # arithmetic — no trig — which is faster than greatCircleAngle().
            cx, cy, cz  = _direction_cosines(ra_deg, dec_deg)
            cos_r        = math.cos(r_rad)
            pf = (
                f"-- Exact cone post-filter (dot product, no trig at query time)\n"
                f"-- Add this to the WHERE clause of the healpix range query:\n"
                f"AND ({cx_col} * {cx:.17g}"
                f" + {cy_col} * {cy:.17g}"
                f" + {cz_col} * {cz:.17g}"
                f" >= {cos_r:.17g})"
            )

        elif post_filter_mode == "greatcircle":
            # ClickHouse built-in great-circle distance function.
            # More readable than the cosine form but requires a trig call per row.
            pf = (
                f"-- Exact cone post-filter (ClickHouse greatCircleAngle)\n"
                f"-- Add this to the WHERE clause of the healpix range query:\n"
                f"AND (greatCircleAngle({ra_col}, {dec_col}, "
                f"{ra_deg:.10g}, {dec_deg:.10g}) <= {radius_deg:.10g})"
            )
        else:
            raise ValueError(f"Unknown post_filter_mode: {post_filter_mode}")

    return sql, pf


# ---------------------------------------------------------------------------
# Convenience: full SQL with post-filter embedded
# ---------------------------------------------------------------------------

def cone_search_sql_full(
    ra_deg: float,
    dec_deg: float,
    radius_deg: float,
    table: str,
    column: str,
    algo: Algo = Algo.CONE,
    extra_columns: str = "*",
    post_filter_mode: str = "cosine",
    cx_col: str = "cx", cy_col: str = "cy", cz_col: str = "cz",
    ra_col: str = "ra",  dec_col: str = "dec",
) -> str:
    """Return a single SQL string with both healpix ranges AND post-filter."""
    sql, pf = cone_search_sql(
        ra_deg, dec_deg, radius_deg, table, column,
        algo=algo, extra_columns=extra_columns,
        post_filter=True, post_filter_mode=post_filter_mode,
        cx_col=cx_col, cy_col=cy_col, cz_col=cz_col,
        ra_col=ra_col, dec_col=dec_col,
    )
    # Strip the human-readable comment lines from pf, keeping only the AND
    # clause so the result is a valid self-contained SQL statement.
    and_line = "\n".join(
        l for l in (pf or "").splitlines() if l.strip().startswith("AND")
    )
    return sql.rstrip() + "\n" + and_line
