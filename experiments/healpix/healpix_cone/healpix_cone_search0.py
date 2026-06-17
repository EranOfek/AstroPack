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
    return platform.system() == "Windows"


def _load_backend():
    """Return a namespace with ang2pix, query_disc, neighbours functions."""
    if _is_windows():
        return _AstropyBackend()
    else:
        try:
            return _HealpyBackend()
        except ImportError:
            return _AstropyBackend()


class _HealpyBackend:
    name = "healpy"

    def __init__(self):
        import healpy as hp  # noqa: F401
        self._hp = hp

    def ang2pix_nested(self, nside: int, ra_deg: float, dec_deg: float) -> int:
        """RA/Dec degrees → nested pixel index."""
        theta = math.radians(90.0 - dec_deg)   # colatitude
        phi   = math.radians(ra_deg)
        return int(self._hp.ang2pix(nside, theta, phi, nest=True))

    def query_disc_nested(self, nside: int, ra_deg: float, dec_deg: float,
                          radius_deg: float) -> np.ndarray:
        """Return nested pixel indices within radius_deg of (ra_deg, dec_deg)."""
        hp = self._hp
        vec = hp.ang2vec(math.radians(90.0 - dec_deg), math.radians(ra_deg))
        # inclusive=False → only pixels whose centres are inside the cone
        pix = hp.query_disc(nside, vec, math.radians(radius_deg),
                            nest=True, inclusive=False)
        return pix.astype(np.int64)

    def neighbours_nested(self, nside: int, pix: int) -> np.ndarray:
        """Return 8 neighbours + self (nested). -1 entries removed."""
        hp = self._hp
        neighb = hp.get_all_neighbours(nside, pix, nest=True)
        valid  = neighb[neighb >= 0]
        return np.unique(np.append(valid, pix)).astype(np.int64)

    def pix2ang_nested(self, nside: int, pix: int) -> Tuple[float, float]:
        """nested pixel → (ra_deg, dec_deg)."""
        theta, phi = self._hp.pix2ang(nside, pix, nest=True)
        return math.degrees(phi), 90.0 - math.degrees(theta)


class _AstropyBackend:
    name = "astropy_healpix"

    def __init__(self):
        import astropy_healpix as ah
        from astropy.coordinates import SkyCoord
        from astropy import units as u
        self._ah = ah
        self._SkyCoord = SkyCoord
        self._u = u

    def ang2pix_nested(self, nside: int, ra_deg: float, dec_deg: float) -> int:
        ah = self._ah
        return int(ah.lonlat_to_healpix(
            self._u.Quantity(ra_deg,  unit="deg"),
            self._u.Quantity(dec_deg, unit="deg"),
            nside, order="nested"))

    def query_disc_nested(self, nside: int, ra_deg: float, dec_deg: float,
                          radius_deg: float) -> np.ndarray:
        ah = self._ah
        u  = self._u
        sc = self._SkyCoord(ra=ra_deg * u.deg, dec=dec_deg * u.deg)
        pix = ah.cone_search_skycoord(sc, radius=radius_deg * u.deg,
                                       nside=nside, order="nested")
        return pix.astype(np.int64)

    def neighbours_nested(self, nside: int, pix: int) -> np.ndarray:
        ah = self._ah
        neighb = ah.neighbours(nside, np.array([pix], dtype=np.int64),
                               order="nested").flatten()
        valid  = neighb[neighb >= 0]
        return np.unique(np.append(valid, pix)).astype(np.int64)

    def pix2ang_nested(self, nside: int, pix: int) -> Tuple[float, float]:
        ah  = self._ah
        u   = self._u
        lon, lat = ah.healpix_to_lonlat(
            np.array([pix], dtype=np.int64), nside, order="nested")
        return float(lon.to(u.deg).value[0]), float(lat.to(u.deg).value[0])


# Singleton backend (loaded once)
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
NSIDE_CAT         = 2 ** HEALPIX_LEVEL_CAT   # 65 536
MAX_PIX_ID        = 12 * NSIDE_CAT * NSIDE_CAT - 1   # 51 539 607 551


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

def _best_nside_for_radius(radius_deg: float) -> int:
    """
    Choose the largest NSide (power of 2) whose pixel size ≈ radius.

    HEALPix pixel "radius" (centre-to-vertex) ≈ sqrt(3)/NSide  [radians].
    We want:  sqrt(3)/NSide ≈ radius_rad
              NSide ≈ sqrt(3)/radius_rad

    Take the largest power-of-2 that does NOT exceed that value,
    capped at NSIDE_CAT.
    """
    radius_rad = math.radians(radius_deg)
    if radius_rad <= 0:
        raise ValueError("radius_deg must be > 0")
    ideal     = math.sqrt(3.0) / radius_rad
    level     = int(math.floor(math.log2(ideal)))
    level     = max(level, 0)
    nside     = 2 ** level
    nside     = min(nside, NSIDE_CAT)
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
    factor = NSIDE_CAT // nside_search          # integer, power of 2
    nchild = factor * factor

    pixels = np.unique(pixels.astype(np.int64))

    lo_arr = pixels * nchild
    hi_arr = lo_arr + nchild - 1

    # Sort by lo, then merge overlapping/adjacent ranges
    order  = np.argsort(lo_arr)
    lo_arr = lo_arr[order]
    hi_arr = hi_arr[order]

    merged: List[Tuple[int, int]] = []
    cur_lo = int(lo_arr[0])
    cur_hi = int(hi_arr[0])

    for lo, hi in zip(lo_arr[1:], hi_arr[1:]):
        lo, hi = int(lo), int(hi)
        if lo <= cur_hi + 1:        # contiguous or overlapping → merge
            cur_hi = max(cur_hi, hi)
        else:
            merged.append((cur_lo, cur_hi))
            cur_lo, cur_hi = lo, hi
    merged.append((cur_lo, cur_hi))

    return merged


# ---------------------------------------------------------------------------
# Main search function
# ---------------------------------------------------------------------------

def cone_to_pixel_ranges(
    ra_deg: float,
    dec_deg: float,
    radius_deg: float,
    algo: Algo = Algo.CONE,
) -> PixelRanges:
    """
    Convert a cone search (ra, dec, radius) to pixel ranges at level 16.

    Parameters
    ----------
    ra_deg      : Right ascension in degrees [0, 360).
    dec_deg     : Declination in degrees [-90, 90].
    radius_deg  : Search radius in degrees > 0.
    algo        : Algo.CONE (fewer ranges) or Algo.NEIGHBOR (always ≤ 9).

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
    nside_search = _best_nside_for_radius(radius_deg)

    if algo == Algo.NEIGHBOR:
        center_pix = backend.ang2pix_nested(nside_search, ra_deg, dec_deg)
        pix_list   = backend.neighbours_nested(nside_search, center_pix)

    elif algo == Algo.CONE:
        pix_list = backend.query_disc_nested(nside_search, ra_deg, dec_deg,
                                             radius_deg)
        if len(pix_list) == 0:
            # Fallback: radius smaller than pixel size → use single pixel
            center_pix = backend.ang2pix_nested(nside_search, ra_deg, dec_deg)
            pix_list   = np.array([center_pix], dtype=np.int64)
    else:
        raise ValueError(f"Unknown algo: {algo}")

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

    # --- build BETWEEN ... OR ... clause ---
    range_clauses = [
        f"({column} BETWEEN {lo} AND {hi})"
        for lo, hi in pr.ranges
    ]
    where_healpix = "\n   OR ".join(range_clauses)

    sql = (
        f"SELECT {extra_columns}\n"
        f"FROM {table}\n"
        f"WHERE (\n"
        f"   {where_healpix}\n"
        f")"
    )

    # --- post-filter fragment ---
    pf: Optional[str] = None
    if post_filter:
        r_rad = math.radians(radius_deg)

        if post_filter_mode == "cosine":
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
    # strip comment lines from pf, keep only the AND clause
    and_line = "\n".join(
        l for l in (pf or "").splitlines() if l.strip().startswith("AND")
    )
    return sql.rstrip() + "\n" + and_line


# ---------------------------------------------------------------------------
# Debug helpers
# ---------------------------------------------------------------------------

def debug_best_nside():
    print("\n" + "=" * 60)
    print("DEBUG: _best_nside_for_radius()")
    print("=" * 60)
    cases = [
        ("tiny  0.001°",  0.001),
        ("small 0.1°",    0.1),
        ("1 arcmin",      1/60),
        ("typical 1°",    1.0),
        ("large 5°",      5.0),
        ("huge  45°",    45.0),
    ]
    for label, r in cases:
        ns = _best_nside_for_radius(r)
        pix_size_deg = math.degrees(math.sqrt(3) / ns)
        print(f"  radius={label:15s} → NSideSearch={ns:6d}  "
              f"(pixel size ≈ {pix_size_deg:.4f}°)")


def debug_pixel_ranges_neighbor():
    print("\n" + "=" * 60)
    print("DEBUG: NEIGHBOR algo — RA=254 Dec=64 R=1°")
    print("=" * 60)
    pr = cone_to_pixel_ranges(254.0, 64.0, 1.0, algo=Algo.NEIGHBOR)
    print(pr)


def debug_pixel_ranges_cone():
    print("\n" + "=" * 60)
    print("DEBUG: CONE algo — RA=254 Dec=64 R=1°")
    print("=" * 60)
    pr = cone_to_pixel_ranges(254.0, 64.0, 1.0, algo=Algo.CONE)
    print(pr)


def debug_compare_algos():
    print("\n" + "=" * 60)
    print("DEBUG: NEIGHBOR vs CONE comparison")
    print("=" * 60)
    test_cases = [
        (0.0,   0.0,  1.0,  "equator"),
        (254.0, 64.0, 1.0,  "Sasha example"),
        (180.0, 89.0, 0.5,  "near north pole"),
        (180.0,-89.0, 0.5,  "near south pole"),
        (0.0,   0.0,  0.01, "tiny radius"),
        (45.0,  30.0, 5.0,  "large radius"),
    ]
    print(f"  {'Case':<20} {'Algo':<10} {'NSideS':>7} {'#SearchPix':>10} "
          f"{'#Ranges':>8} {'TotalPix':>10}")
    print("  " + "-" * 70)
    for ra, dec, r, label in test_cases:
        for algo in (Algo.NEIGHBOR, Algo.CONE):
            pr = cone_to_pixel_ranges(ra, dec, r, algo=algo)
            total = sum(hi - lo + 1 for lo, hi in pr.ranges)
            print(f"  {label:<20} {algo.value:<10} {pr.nside_search:>7d} "
                  f"{pr.n_search_pixels:>10d} {pr.n_ranges:>8d} {total:>10d}")
        print()


def debug_sql_output():
    print("\n" + "=" * 60)
    print("DEBUG: SQL output — RA=254 Dec=64 R=1°")
    print("=" * 60)
    for algo in (Algo.CONE, Algo.NEIGHBOR):
        print(f"\n--- algo={algo.value} ---")
        sql, pf = cone_search_sql(
            254.0, 64.0, 1.0,
            table="proc_src", column="upix_high",
            algo=algo,
            post_filter=True, post_filter_mode="cosine",
        )
        print(sql)
        print(pf)

    print("\n--- greatcircle post-filter ---")
    sql, pf = cone_search_sql(
        254.0, 64.0, 1.0,
        table="proc_src", column="upix_high",
        post_filter=True, post_filter_mode="greatcircle",
    )
    print(sql)
    print(pf)


def debug_edge_cases():
    print("\n" + "=" * 60)
    print("DEBUG: edge cases")
    print("=" * 60)

    # RA wrap-around near 0/360
    print("\n  RA near 0/360 boundary:")
    pr1 = cone_to_pixel_ranges(0.5,  0.0, 1.0, algo=Algo.CONE)
    pr2 = cone_to_pixel_ranges(359.5, 0.0, 1.0, algo=Algo.CONE)
    print(f"    RA=0.5°  → {pr1.n_ranges} ranges")
    print(f"    RA=359.5°→ {pr2.n_ranges} ranges")

    # pole
    print("\n  North pole (Dec=90):")
    pr = cone_to_pixel_ranges(0.0, 90.0, 1.0, algo=Algo.CONE)
    print(f"    → {pr.n_ranges} ranges, {pr.n_search_pixels} search pixels")

    # very small radius (sub-pixel)
    print("\n  Sub-pixel radius (0.001°):")
    pr = cone_to_pixel_ranges(45.0, 30.0, 0.001, algo=Algo.CONE)
    print(f"    → {pr.n_ranges} ranges, nside_search={pr.nside_search}")

    # large radius
    print("\n  Large radius (10°):")
    pr = cone_to_pixel_ranges(45.0, 30.0, 10.0, algo=Algo.CONE)
    print(f"    → {pr.n_ranges} ranges, {pr.n_search_pixels} search pixels")


def debug_pixel_id_bounds():
    print("\n" + "=" * 60)
    print("DEBUG: pixel ID bounds check")
    print("=" * 60)
    print(f"  NSIDE_CAT    = {NSIDE_CAT}")
    print(f"  MAX_PIX_ID   = {MAX_PIX_ID:,}  ({MAX_PIX_ID:.3e})")
    print(f"  fits UInt32? : {MAX_PIX_ID <= 2**32 - 1}")
    print(f"  fits UInt64? : {MAX_PIX_ID <= 2**64 - 1}")
    pr = cone_to_pixel_ranges(0.0, 0.0, 1.0, algo=Algo.CONE)
    all_ids = [x for lo, hi in pr.ranges for x in (lo, hi)]
    print(f"  max id in sample ranges = {max(all_ids):,}")
    print(f"  all within bounds?       {all(0 <= x <= MAX_PIX_ID for x in all_ids)}")


def debug():
    """Master debug — calls all debug_* functions."""
    print(f"\nBackend: {get_backend().name}")
    debug_best_nside()
    debug_pixel_ranges_neighbor()
    debug_pixel_ranges_cone()
    debug_compare_algos()
    debug_sql_output()
    debug_edge_cases()
    debug_pixel_id_bounds()


if __name__ == "__main__":
    debug()
