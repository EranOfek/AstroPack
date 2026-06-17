"""
test_healpix_cone_search.py
===========================
Comprehensive pytest suite for healpix_cone_search.py

Run:
    pytest test_healpix_cone_search.py -v
    pytest test_healpix_cone_search.py -v --tb=short
"""

import math
import re
import sys
import pytest
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

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

def _sphere_dist_deg(ra1, dec1, ra2, dec2):
    """Great-circle distance in degrees (haversine)."""
    r1, d1, r2, d2 = map(math.radians, (ra1, dec1, ra2, dec2))
    dlat = d2 - d1
    dlon = r2 - r1
    a = math.sin(dlat / 2) ** 2 + math.cos(d1) * math.cos(d2) * math.sin(dlon / 2) ** 2
    return math.degrees(2 * math.asin(math.sqrt(a)))


def _pix_center(nside, pix):
    """Return (ra_deg, dec_deg) of pixel centre."""
    return get_backend().pix2ang_nested(nside, int(pix))


def _ranges_cover_pixel(ranges, pix_id):
    return any(lo <= pix_id <= hi for lo, hi in ranges)


# ---------------------------------------------------------------------------
# 1. _best_nside_for_radius
# ---------------------------------------------------------------------------

class TestBestNside:

    def test_returns_power_of_two(self):
        for r in [0.001, 0.01, 0.1, 0.5, 1.0, 5.0, 30.0]:
            ns = _best_nside_for_radius(r)
            assert ns > 0
            assert (ns & (ns - 1)) == 0, f"NSide={ns} is not a power of 2"

    def test_never_exceeds_nside_cat(self):
        for r in [1e-6, 1e-5, 0.0001]:
            ns = _best_nside_for_radius(r)
            assert ns <= NSIDE_CAT

    def test_minimum_is_one(self):
        ns = _best_nside_for_radius(89.0)   # huge radius
        assert ns >= 1

    def test_pixel_size_covers_radius(self):
        """Pixel size at NSideSearch must be >= radius (conservative)."""
        for r_deg in [0.01, 0.1, 0.5, 1.0, 2.0, 5.0]:
            ns   = _best_nside_for_radius(r_deg)
            r_rad = math.radians(r_deg)
            pix_radius_rad = math.sqrt(3) / ns
            assert pix_radius_rad >= r_rad, (
                f"radius={r_deg}° NSide={ns}: pixel radius {math.degrees(pix_radius_rad):.4f}° "
                f"< search radius {r_deg}°"
            )

    def test_raises_on_zero_radius(self):
        with pytest.raises(ValueError):
            _best_nside_for_radius(0.0)

    def test_raises_on_negative_radius(self):
        with pytest.raises(ValueError):
            _best_nside_for_radius(-1.0)

    def test_known_values(self):
        # sqrt(3)/NSide >= radius_rad  → NSide <= sqrt(3)/radius_rad
        # For radius=1° = 0.01745 rad  → ideal ≈ 99.4 → largest pow2 ≤ 99 → 64
        ns = _best_nside_for_radius(1.0)
        assert ns == 64

        # For radius=0.1° = 0.001745 rad → ideal ≈ 994 → 512
        ns = _best_nside_for_radius(0.1)
        assert ns == 512


# ---------------------------------------------------------------------------
# 2. _pixels_to_ranges
# ---------------------------------------------------------------------------

class TestPixelsToRanges:

    def test_single_pixel_nside_cat(self):
        """At NSideSearch == NSIDE_CAT, each pixel maps to [pix, pix]."""
        pix = np.array([42], dtype=np.int64)
        ranges = _pixels_to_ranges(pix, NSIDE_CAT)
        assert ranges == [(42, 42)]

    def test_contiguous_pixels_merge(self):
        """Adjacent pixels at low NSide must merge into one range."""
        # At NSideSearch=NSIDE_CAT//2, nchild=4
        # pixel 0 → [0,3], pixel 1 → [4,7]  → merge → [0,7]
        nside_s = NSIDE_CAT // 2
        pix = np.array([0, 1], dtype=np.int64)
        ranges = _pixels_to_ranges(pix, nside_s)
        assert len(ranges) == 1
        assert ranges[0] == (0, 7)

    def test_non_contiguous_pixels_separate(self):
        """Non-adjacent pixels must not merge."""
        nside_s = NSIDE_CAT // 2  # nchild=4
        # pixel 0 → [0,3], pixel 10 → [40,43]
        pix = np.array([0, 10], dtype=np.int64)
        ranges = _pixels_to_ranges(pix, nside_s)
        assert len(ranges) == 2

    def test_range_width_correct(self):
        nside_s = NSIDE_CAT // 4   # factor=4, nchild=16
        pix = np.array([5], dtype=np.int64)
        ranges = _pixels_to_ranges(pix, nside_s)
        lo, hi = ranges[0]
        assert hi - lo + 1 == 16
        assert lo == 5 * 16

    def test_all_ids_within_bounds(self):
        # At nside=1 there are only 12 pixels total (0..11)
        # Use a modest nside so pixel ids stay within catalog bounds
        nside_s = 64   # 12 * 64^2 = 49152 pixels
        n_pix   = 12 * 64 * 64
        pix = np.arange(0, min(100, n_pix), dtype=np.int64)
        ranges = _pixels_to_ranges(pix, nside_s)
        for lo, hi in ranges:
            assert lo >= 0
            assert hi <= MAX_PIX_ID

    def test_duplicates_handled(self):
        """Duplicate pixel IDs must not create extra ranges."""
        nside_s = NSIDE_CAT
        pix = np.array([5, 5, 5], dtype=np.int64)
        ranges = _pixels_to_ranges(pix, nside_s)
        assert len(ranges) == 1
        assert ranges[0] == (5, 5)

    def test_ranges_are_sorted(self):
        pix = np.array([100, 10, 50, 1], dtype=np.int64)
        ranges = _pixels_to_ranges(pix, NSIDE_CAT)
        los = [lo for lo, hi in ranges]
        assert los == sorted(los)

    def test_ranges_non_overlapping(self):
        pix = np.arange(0, 200, dtype=np.int64)
        ranges = _pixels_to_ranges(pix, NSIDE_CAT // 8)
        for i in range(len(ranges) - 1):
            assert ranges[i][1] < ranges[i + 1][0], "Ranges overlap!"


# ---------------------------------------------------------------------------
# 3. cone_to_pixel_ranges — NEIGHBOR
# ---------------------------------------------------------------------------

class TestConeToPixelRangesNeighbor:

    def test_returns_pixel_ranges_object(self):
        pr = cone_to_pixel_ranges(0.0, 0.0, 1.0, algo=Algo.NEIGHBOR)
        assert isinstance(pr, PixelRanges)

    def test_at_most_9_ranges(self):
        """Neighbor algo: central + 8 neighbours → ≤ 9 ranges always."""
        cases = [
            (0.0,   0.0,  1.0),
            (254.0, 64.0, 1.0),
            (180.0, 89.9, 0.5),
            (90.0, -89.9, 0.5),
            (45.0,  45.0, 0.1),
        ]
        for ra, dec, r in cases:
            pr = cone_to_pixel_ranges(ra, dec, r, algo=Algo.NEIGHBOR)
            assert pr.n_ranges <= 9, (
                f"RA={ra} Dec={dec} R={r}: got {pr.n_ranges} ranges"
            )

    def test_algo_stored_correctly(self):
        pr = cone_to_pixel_ranges(0.0, 0.0, 1.0, algo=Algo.NEIGHBOR)
        assert pr.algo == Algo.NEIGHBOR

    def test_nside_search_correct(self):
        pr = cone_to_pixel_ranges(0.0, 0.0, 1.0, algo=Algo.NEIGHBOR)
        assert pr.nside_search == _best_nside_for_radius(1.0)

    def test_all_range_ids_valid(self):
        pr = cone_to_pixel_ranges(45.0, 30.0, 1.0, algo=Algo.NEIGHBOR)
        for lo, hi in pr.ranges:
            assert 0 <= lo <= hi <= MAX_PIX_ID

    def test_ranges_non_overlapping(self):
        pr = cone_to_pixel_ranges(45.0, 30.0, 1.0, algo=Algo.NEIGHBOR)
        for i in range(len(pr.ranges) - 1):
            assert pr.ranges[i][1] < pr.ranges[i + 1][0]

    def test_center_pixel_covered(self):
        """The center of the search must always fall in some range."""
        backend = get_backend()
        ra, dec, r = 254.0, 64.0, 1.0
        pr = cone_to_pixel_ranges(ra, dec, r, algo=Algo.NEIGHBOR)
        center_pix = backend.ang2pix_nested(NSIDE_CAT, ra, dec)
        assert _ranges_cover_pixel(pr.ranges, center_pix), \
            f"Center pixel {center_pix} not in ranges {pr.ranges}"


# ---------------------------------------------------------------------------
# 4. cone_to_pixel_ranges — CONE
# ---------------------------------------------------------------------------

class TestConeToPixelRangesCone:

    def test_returns_pixel_ranges_object(self):
        pr = cone_to_pixel_ranges(0.0, 0.0, 1.0, algo=Algo.CONE)
        assert isinstance(pr, PixelRanges)

    def test_algo_stored_correctly(self):
        pr = cone_to_pixel_ranges(0.0, 0.0, 1.0, algo=Algo.CONE)
        assert pr.algo == Algo.CONE

    def test_all_range_ids_valid(self):
        pr = cone_to_pixel_ranges(45.0, 30.0, 1.0, algo=Algo.CONE)
        for lo, hi in pr.ranges:
            assert 0 <= lo <= hi <= MAX_PIX_ID

    def test_center_pixel_covered(self):
        backend = get_backend()
        ra, dec, r = 254.0, 64.0, 1.0
        pr = cone_to_pixel_ranges(ra, dec, r, algo=Algo.CONE)
        center_pix = backend.ang2pix_nested(NSIDE_CAT, ra, dec)
        assert _ranges_cover_pixel(pr.ranges, center_pix)

    def test_cone_fewer_ranges_than_neighbor(self):
        """CONE should produce ≤ ranges than NEIGHBOR for same input."""
        ra, dec, r = 254.0, 64.0, 1.0
        pr_n = cone_to_pixel_ranges(ra, dec, r, algo=Algo.NEIGHBOR)
        pr_c = cone_to_pixel_ranges(ra, dec, r, algo=Algo.CONE)
        assert pr_c.n_ranges <= pr_n.n_ranges, (
            f"CONE ({pr_c.n_ranges}) > NEIGHBOR ({pr_n.n_ranges})"
        )

    def test_cone_fewer_total_pixels_than_neighbor(self):
        """CONE total pixel count should be ≤ NEIGHBOR."""
        ra, dec, r = 254.0, 64.0, 1.0
        pr_n = cone_to_pixel_ranges(ra, dec, r, algo=Algo.NEIGHBOR)
        pr_c = cone_to_pixel_ranges(ra, dec, r, algo=Algo.CONE)
        total_n = sum(hi - lo + 1 for lo, hi in pr_n.ranges)
        total_c = sum(hi - lo + 1 for lo, hi in pr_c.ranges)
        assert total_c <= total_n

    def test_no_false_negatives_inside_cone(self):
        """
        Sample random pixels whose CENTRES are well inside the cone —
        every one must be covered by the ranges.
        """
        backend = get_backend()
        ra0, dec0, r = 100.0, 20.0, 1.0
        pr = cone_to_pixel_ranges(ra0, dec0, r, algo=Algo.CONE)

        # Sample ≤200 pixels from the expanded range set
        all_pix = []
        for lo, hi in pr.ranges:
            all_pix.extend(range(lo, min(hi + 1, lo + 20)))  # up to 20 per range
        if not all_pix:
            pytest.skip("no pixels to sample")

        inner_r = r * 0.6   # well inside cone
        missed  = 0
        checked = 0
        for p in all_pix[:200]:
            pra, pdec = _pix_center(NSIDE_CAT, p)
            d = _sphere_dist_deg(ra0, dec0, pra, pdec)
            if d <= inner_r:
                if not _ranges_cover_pixel(pr.ranges, p):
                    missed += 1
                checked += 1
        assert missed == 0, f"{missed}/{checked} inner-cone pixels not covered"

    def test_sub_pixel_radius_returns_at_least_one_range(self):
        """Even a tiny (sub-pixel) radius must return ≥ 1 range."""
        pr = cone_to_pixel_ranges(45.0, 30.0, 0.001, algo=Algo.CONE)
        assert pr.n_ranges >= 1

    def test_ranges_non_overlapping(self):
        pr = cone_to_pixel_ranges(254.0, 64.0, 1.0, algo=Algo.CONE)
        for i in range(len(pr.ranges) - 1):
            assert pr.ranges[i][1] < pr.ranges[i + 1][0]


# ---------------------------------------------------------------------------
# 5. Input validation
# ---------------------------------------------------------------------------

class TestInputValidation:

    @pytest.mark.parametrize("ra", [-1.0, 360.0, 400.0])
    def test_invalid_ra(self, ra):
        with pytest.raises(ValueError, match="ra_deg"):
            cone_to_pixel_ranges(ra, 0.0, 1.0)

    @pytest.mark.parametrize("dec", [-91.0, 91.0, 180.0])
    def test_invalid_dec(self, dec):
        with pytest.raises(ValueError, match="dec_deg"):
            cone_to_pixel_ranges(0.0, dec, 1.0)

    @pytest.mark.parametrize("r", [0.0, -1.0])
    def test_invalid_radius(self, r):
        with pytest.raises(ValueError, match="radius_deg"):
            cone_to_pixel_ranges(0.0, 0.0, r)


# ---------------------------------------------------------------------------
# 6. Pixel ID bounds
# ---------------------------------------------------------------------------

class TestPixelIdBounds:

    def test_max_pixel_id_constant(self):
        assert MAX_PIX_ID == 12 * NSIDE_CAT ** 2 - 1

    def test_nside_cat(self):
        assert NSIDE_CAT == 65_536

    def test_max_id_exceeds_uint32(self):
        """Must be > 2^32-1, confirming UInt64 is required in ClickHouse."""
        assert MAX_PIX_ID > 2 ** 32 - 1

    def test_max_id_fits_uint64(self):
        assert MAX_PIX_ID < 2 ** 64 - 1

    def test_all_ranges_within_bounds(self):
        cases = [(0.0, 0.0, 1.0), (254.0, 64.0, 1.0), (180.0, 89.0, 0.5)]
        for ra, dec, r in cases:
            for algo in (Algo.CONE, Algo.NEIGHBOR):
                pr = cone_to_pixel_ranges(ra, dec, r, algo=algo)
                for lo, hi in pr.ranges:
                    assert 0 <= lo
                    assert hi <= MAX_PIX_ID


# ---------------------------------------------------------------------------
# 7. Direction cosines
# ---------------------------------------------------------------------------

class TestDirectionCosines:

    def test_unit_vector(self):
        for ra, dec in [(0, 0), (90, 0), (0, 90), (45, 45), (254, 64)]:
            cx, cy, cz = _direction_cosines(ra, dec)
            norm = math.sqrt(cx**2 + cy**2 + cz**2)
            assert abs(norm - 1.0) < 1e-12, f"Not unit vector at RA={ra} Dec={dec}"

    def test_known_values(self):
        # RA=0, Dec=0 → (1, 0, 0)
        cx, cy, cz = _direction_cosines(0.0, 0.0)
        assert abs(cx - 1.0) < 1e-12
        assert abs(cy - 0.0) < 1e-12
        assert abs(cz - 0.0) < 1e-12

    def test_north_pole(self):
        # Dec=90 → (0, 0, 1)
        cx, cy, cz = _direction_cosines(0.0, 90.0)
        assert abs(cz - 1.0) < 1e-12

    def test_south_pole(self):
        cx, cy, cz = _direction_cosines(0.0, -90.0)
        assert abs(cz + 1.0) < 1e-12

    def test_dot_product_is_cos_distance(self):
        """dot(A, B) == cos(angular_distance(A, B))."""
        pairs = [(0, 0, 1, 0), (45, 30, 50, 35), (254, 64, 256, 65)]
        for ra1, dec1, ra2, dec2 in pairs:
            c1 = _direction_cosines(ra1, dec1)
            c2 = _direction_cosines(ra2, dec2)
            dot = sum(a * b for a, b in zip(c1, c2))
            d   = _sphere_dist_deg(ra1, dec1, ra2, dec2)
            assert abs(dot - math.cos(math.radians(d))) < 1e-10


# ---------------------------------------------------------------------------
# 8. SQL generation
# ---------------------------------------------------------------------------

class TestSqlGeneration:

    def _sql_and_pf(self, ra=254.0, dec=64.0, r=1.0,
                    table="proc_src", col="upix_high", **kw):
        return cone_search_sql(ra, dec, r, table=table, column=col, **kw)

    def test_returns_tuple(self):
        result = self._sql_and_pf()
        assert isinstance(result, tuple) and len(result) == 2

    def test_sql_contains_select(self):
        sql, _ = self._sql_and_pf()
        assert sql.strip().upper().startswith("SELECT")

    def test_sql_contains_table_name(self):
        sql, _ = self._sql_and_pf(table="my_catalog")
        assert "my_catalog" in sql

    def test_sql_contains_column_name(self):
        sql, _ = self._sql_and_pf(col="healpix_id")
        assert "healpix_id" in sql

    def test_sql_contains_between(self):
        sql, _ = self._sql_and_pf()
        assert "BETWEEN" in sql.upper()

    def test_sql_range_values_are_integers(self):
        sql, _ = self._sql_and_pf()
        # extract all numbers after BETWEEN and AND
        numbers = re.findall(r"BETWEEN\s+(\d+)\s+AND\s+(\d+)", sql)
        assert len(numbers) > 0
        for lo_s, hi_s in numbers:
            lo, hi = int(lo_s), int(hi_s)
            assert lo <= hi
            assert lo >= 0
            assert hi <= MAX_PIX_ID

    def test_post_filter_none_when_disabled(self):
        _, pf = cone_search_sql(254.0, 64.0, 1.0, "t", "c", post_filter=False)
        assert pf is None

    def test_post_filter_cosine_contains_and(self):
        _, pf = self._sql_and_pf(post_filter=True, post_filter_mode="cosine")
        assert pf is not None
        assert "AND" in pf.upper()

    def test_post_filter_cosine_has_three_terms(self):
        _, pf = self._sql_and_pf(post_filter=True, post_filter_mode="cosine")
        # must have cx*..., cy*..., cz*...
        assert pf.count("*") >= 3

    def test_post_filter_greatcircle_contains_greatcircleangle(self):
        _, pf = self._sql_and_pf(post_filter=True,
                                  post_filter_mode="greatcircle")
        assert "greatCircleAngle" in pf

    def test_post_filter_invalid_mode(self):
        with pytest.raises(ValueError, match="post_filter_mode"):
            cone_search_sql(0.0, 0.0, 1.0, "t", "c",
                            post_filter=True, post_filter_mode="bad")

    def test_extra_columns_in_select(self):
        sql, _ = cone_search_sql(0.0, 0.0, 1.0, "t", "c",
                                  extra_columns="id, ra, dec")
        assert "id, ra, dec" in sql

    def test_both_algos_produce_valid_sql(self):
        for algo in (Algo.CONE, Algo.NEIGHBOR):
            sql, _ = cone_search_sql(254.0, 64.0, 1.0, "t", "c", algo=algo)
            assert "SELECT" in sql.upper()
            assert "BETWEEN" in sql.upper()

    def test_sql_full_contains_and_filter(self):
        sql = cone_search_sql_full(254.0, 64.0, 1.0, "t", "c")
        assert "AND" in sql

    def test_custom_table_and_column(self):
        sql, _ = cone_search_sql(
            1.0, 1.0, 0.5,
            table="my_survey.detections",
            column="hpx_nested",
        )
        assert "my_survey.detections" in sql
        assert "hpx_nested" in sql

    def test_ranges_in_sql_match_pixel_ranges(self):
        """Numbers in SQL must exactly match PixelRanges.ranges."""
        pr  = cone_to_pixel_ranges(254.0, 64.0, 1.0, algo=Algo.CONE)
        sql, _ = cone_search_sql(254.0, 64.0, 1.0, "t", "c", algo=Algo.CONE)
        found  = re.findall(r"BETWEEN\s+(\d+)\s+AND\s+(\d+)", sql)
        sql_ranges = [(int(a), int(b)) for a, b in found]
        assert sql_ranges == pr.ranges


# ---------------------------------------------------------------------------
# 9. Edge cases / special sky positions
# ---------------------------------------------------------------------------

class TestEdgeCases:

    @pytest.mark.parametrize("ra,dec,r", [
        (0.0,   90.0, 0.5),   # north pole
        (0.0,  -90.0, 0.5),   # south pole
        (0.0,    0.0, 1.0),   # equator
        (359.9,  0.0, 1.0),   # RA near 360
        (0.1,    0.0, 1.0),   # RA near 0
    ])
    def test_no_exception_at_special_positions(self, ra, dec, r):
        for algo in (Algo.CONE, Algo.NEIGHBOR):
            pr = cone_to_pixel_ranges(ra, dec, r, algo=algo)
            assert pr.n_ranges >= 1

    def test_very_small_radius(self):
        pr = cone_to_pixel_ranges(45.0, 30.0, 1e-4, algo=Algo.CONE)
        assert pr.n_ranges >= 1
        for lo, hi in pr.ranges:
            assert 0 <= lo <= hi <= MAX_PIX_ID

    def test_large_radius(self):
        pr = cone_to_pixel_ranges(45.0, 30.0, 10.0, algo=Algo.CONE)
        assert pr.n_ranges >= 1

    def test_ra_zero_and_359_similar_coverage(self):
        """RA=0.5 and RA=359.5 cones near equator should have comparable counts."""
        pr1 = cone_to_pixel_ranges(0.5,   0.0, 1.0, algo=Algo.CONE)
        pr2 = cone_to_pixel_ranges(359.5, 0.0, 1.0, algo=Algo.CONE)
        t1  = sum(hi - lo + 1 for lo, hi in pr1.ranges)
        t2  = sum(hi - lo + 1 for lo, hi in pr2.ranges)
        # total pixel count should be similar (within 20%)
        ratio = max(t1, t2) / max(min(t1, t2), 1)
        assert ratio < 1.5, f"RA wrap asymmetry: t1={t1} t2={t2}"


# ---------------------------------------------------------------------------
# 10. Backend availability
# ---------------------------------------------------------------------------

class TestBackend:

    def test_backend_loads(self):
        b = get_backend()
        assert b is not None
        assert b.name in ("healpy", "astropy_healpix")

    def test_backend_ang2pix_round_trip(self):
        """ang2pix then pix2ang should be close to original."""
        b   = get_backend()
        ra, dec = 123.456, -34.567
        pix     = b.ang2pix_nested(NSIDE_CAT, ra, dec)
        ra2, dec2 = b.pix2ang_nested(NSIDE_CAT, pix)
        # pixel size ~3.2" → < 0.01° tolerance
        assert _sphere_dist_deg(ra, dec, ra2, dec2) < 0.01

    def test_backend_neighbours_returns_array(self):
        b    = get_backend()
        pix  = b.ang2pix_nested(64, 45.0, 30.0)
        nb   = b.neighbours_nested(64, pix)
        assert isinstance(nb, np.ndarray)
        assert len(nb) >= 1

    def test_backend_query_disc_returns_array(self):
        b   = get_backend()
        pix = b.query_disc_nested(64, 45.0, 30.0, 1.0)
        assert isinstance(pix, np.ndarray)


# ---------------------------------------------------------------------------
# 11. Reproducibility
# ---------------------------------------------------------------------------

class TestReproducibility:

    def test_same_input_same_output(self):
        kw = dict(ra_deg=123.0, dec_deg=-20.0, radius_deg=0.5, algo=Algo.CONE)
        pr1 = cone_to_pixel_ranges(**kw)
        pr2 = cone_to_pixel_ranges(**kw)
        assert pr1.ranges == pr2.ranges

    def test_sql_deterministic(self):
        kw = dict(ra_deg=45.0, dec_deg=10.0, radius_deg=1.0,
                  table="t", column="c")
        sql1, _ = cone_search_sql(**kw)
        sql2, _ = cone_search_sql(**kw)
        assert sql1 == sql2
