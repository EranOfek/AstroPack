"""
debug_healpix_cone_search.py
============================
Debug helpers for healpix_cone_search.py

Run:
    python debug_healpix_cone_search.py

Usage:
    python debug_healpix_cone_search.py

Debug functions:
    debug_best_nside()                  # best nside for radius
    debug_pixel_ranges_neighbor()       # NEIGHBOR algo
    debug_pixel_ranges_cone()           # CONE algo
    debug_compare_algos()               # NEIGHBOR vs CONE comparison
    debug_sql_output()                  # SQL output
    debug_edge_cases()                  # edge cases
    debug_pixel_id_bounds()             # pixel ID bounds check

"""

import math
import platform
import sys
from dataclasses import dataclass, field
from enum import Enum
from typing import List, Optional, Tuple

import numpy as np

from healpix_cone_search import (
    Algo,
    NSIDE_CAT,
    MAX_PIX_ID,
    PixelRanges,
    _best_nside_for_radius,
    _direction_cosines,
    _pixels_to_ranges,
    cone_to_pixel_ranges,
    cone_search_sql,
    cone_search_sql_full,
    get_backend,
)

# ============================================================================
#                                 Debug Functions
# ============================================================================

def debug_best_nside():
    print("\n" + "=" * 60)
    print("DEBUG: _best_nside_for_radius() — all three modes")
    print("=" * 60)
    cases = [
        ("tiny  0.001°",  0.001),
        ("small 0.1°",    0.1),
        ("1 arcmin",      1/60),
        ("typical 1°",    1.0),
        ("large 5°",      5.0),
        ("huge  45°",    45.0),
    ]
    modes = ["conservative", "area", "circumradius"]
    header = f"  {'radius':<16}" + "".join(f"{'NSide_'+m:>20}" for m in modes)
    print(header)
    print("  " + "-" * (16 + 20 * len(modes)))
    for label, r in cases:
        row = f"  {label:<16}"
        for m in modes:
            ns = _best_nside_for_radius(r, mode=m)
            pix_deg = math.degrees(math.sqrt(3) / ns)
            row += f"  {ns:>6d} ({pix_deg:.3f}°)"
        print(row)
    print()
    print("  NOTE: 'conservative' = Eran's preferred formula (1/r)")
    print("        'area'         = area-matching (1/(sqrt(3)*r))  — coarsest")
    print("        'circumradius' = circumradius  (sqrt(3)/r)      — finest")


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
    print(f"    RA=0.5°  -> {pr1.n_ranges} ranges")
    print(f"    RA=359.5°-> {pr2.n_ranges} ranges")

    # pole
    print("\n  North pole (Dec=90):")
    pr = cone_to_pixel_ranges(0.0, 90.0, 1.0, algo=Algo.CONE)
    print(f"    ->{pr.n_ranges} ranges, {pr.n_search_pixels} search pixels")

    # very small radius (sub-pixel)
    print("\n  Sub-pixel radius (0.001°):")
    pr = cone_to_pixel_ranges(45.0, 30.0, 0.001, algo=Algo.CONE)
    print(f"    ->{pr.n_ranges} ranges, nside_search={pr.nside_search}")

    # large radius
    print("\n  Large radius (10°):")
    pr = cone_to_pixel_ranges(45.0, 30.0, 10.0, algo=Algo.CONE)
    print(f"    ->{pr.n_ranges} ranges, {pr.n_search_pixels} search pixels")


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
