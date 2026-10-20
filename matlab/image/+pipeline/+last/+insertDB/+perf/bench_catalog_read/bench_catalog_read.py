#!/usr/bin/env python3
"""Benchmark: load ~480 LAST ``*proc_Cat_1.fits`` tables into memory as fast as possible.

Cache states (the dominant factor on this storage -- a spinning RAID, ~6 ms/file cold):
  warm   same visit, page cache hot            -> pure CPU / parse cost
  evict  same visit, page cache dropped (posix_fadvise DONTNEED) before each run
         (the RAID controller cache may still be warm -> optimistic "cold")
  cold   every trial reads a *fresh* visit never read before (from --cold-pool, a proc/ dir)
         -> the real first-touch cost. Visits already used are recorded in --cold-state
         so MATLAB and Python runs never reuse one.

Examples:
  bench_catalog_read.py --visit .../proc/214135v0 --states warm,evict
  bench_catalog_read.py --visit .../proc/214135v0 --cold-pool .../LAST.01.10.04/2026/06/04/proc --states cold
"""

from __future__ import annotations

import argparse
import csv
import itertools
import random
import os
import statistics
import sys
import time
from concurrent.futures import ProcessPoolExecutor
from datetime import datetime
from pathlib import Path
from typing import Dict, List, Sequence, Tuple

import numpy as np

sys.path.insert(0, str(Path(__file__).resolve().parent))
import catread  # noqa: E402

TEMPLATE = "LAST*proc_Cat_1.fits"


def visit_files(visit: Path, template: str = TEMPLATE) -> List[str]:
    return sorted(str(p) for p in visit.glob(template))


def list_pool(pool: Path, state_file: Path) -> List[Path]:
    used = set(state_file.read_text().split()) if state_file.exists() else set()
    visits = [d for d in sorted(pool.iterdir()) if d.is_dir() and d.name.endswith("v0")]
    return [d for d in visits if str(d) not in used and len(visit_files(d)) == 480]


def mark_used(state_file: Path, visit: Path) -> None:
    with state_file.open("a") as f:
        f.write(str(visit) + "\n")


def digest(arrs: Sequence[np.ndarray]) -> Tuple[int, float]:
    """(nFiles, checksum). Non-finite values (the catalogs contain inf/nan) are counted, not summed."""
    tot = 0.0
    for a in arrs:
        fin = np.isfinite(a)
        tot += float(a[fin].sum()) + 1e3 * a.shape[0] + 7.0 * float((~fin).sum())
    return len(arrs), tot


def _run_proc_one(args):
    name, path = args
    return catread.READERS[name](path)


def _run_proc_chunk(args):
    name, chunk = args
    fn = catread.READERS[name]
    return [fn(p) for p in chunk]


def run_config(method: str, driver: str, n: int, paths: List[str]):
    fn = catread.READERS[method]
    if driver == "serial":
        return catread.run_serial(fn, paths)
    if driver == "threads":
        return catread.run_threads(fn, paths, n)
    if driver == "prefetch":
        return catread.run_prefetch_threads(fn, paths, n)
    if driver == "procs":
        # chunked so each worker pickles one big result instead of 480 small ones
        chunks = [paths[i::n] for i in range(n)]
        with ProcessPoolExecutor(max_workers=n) as ex:
            parts = list(ex.map(_run_proc_chunk, [(method, c) for c in chunks]))
        out: List = [None] * len(paths)
        for i, part in enumerate(parts):
            out[i::n] = part
        return out
    raise ValueError(driver)


def build_configs(methods: List[str], drivers: List[str], threads: List[int]) -> List[Tuple[str, str, int]]:
    cfgs = []
    for m, d in itertools.product(methods, drivers):
        if m == "raw" and d == "procs":
            continue
        if d == "serial":
            cfgs.append((m, d, 1))
        else:
            cfgs += [(m, d, n) for n in threads if n > 1]
    return cfgs


def main(argv: Sequence[str] | None = None) -> int:
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--visit", type=Path, required=True, help="visit dir used for warm/evict states")
    ap.add_argument("--states", default="warm,evict")
    ap.add_argument("--methods", default="raw,numpy,fitsio,astropy")
    ap.add_argument("--drivers", default="serial,threads")
    ap.add_argument("--threads", default="4,16,32")
    ap.add_argument("--reps", type=int, default=3, help="repetitions per config (warm/evict) or rounds (cold)")
    ap.add_argument("--cold-pool", type=Path, nargs="+", help="proc/ dir(s) whose *v0 visits are used one-per-trial for state=cold")
    ap.add_argument("--seed", type=int, default=1)
    ap.add_argument("--cold-state", type=Path, default=Path(__file__).with_name("cold_used_visits.txt"))
    ap.add_argument("--csv", type=Path, default=None, help="append results here (default bench_py_<ts>.csv)")
    ap.add_argument("--no-verify", action="store_true")
    args = ap.parse_args(argv)

    methods = args.methods.split(",")
    for mod, meth in (("fitsio", "fitsio"), ("astropy", "astropy")):
        if meth in methods:
            try:
                __import__(mod)
            except ImportError:
                print(f"skip method {meth}: {mod} not installed", file=sys.stderr)
                methods.remove(meth)
    drivers = args.drivers.split(",")
    threads = [int(x) for x in args.threads.split(",")]
    states = args.states.split(",")
    cfgs = build_configs(methods, drivers, threads)
    csv_path = args.csv or Path.cwd() / f"bench_py_{datetime.now():%Y%m%d_%H%M%S}.csv"
    rows: List[Dict] = []

    def record(state, cfg, visit, files, sec, ok):
        mb = sum(os.stat(p).st_size for p in files) / 2**20
        r = dict(lang="python", state=state, method=cfg[0], driver=cfg[1], threads=cfg[2], visit=str(visit),
                 n=len(files), sizeMB=round(mb, 2), sec=round(sec, 4), MBps=round(mb / sec, 1), ok=ok)
        rows.append(r)
        print(f"{state:5s} {cfg[0]:12s} {cfg[1]:8s} thr={cfg[2]:3d}  {sec:7.3f}s  {mb/sec:8.1f} MB/s  ok={ok}  {Path(visit).name}", flush=True)

    ref_cache: Dict[str, Tuple[int, float]] = {}

    def reference(files, visit):
        if str(visit) not in ref_cache:
            try:
                import astropy  # noqa: F401
                ref_fn = catread.read_astropy          # independent reference
            except ImportError:
                ref_fn = catread.read_numpy            # was verified identical to astropy on all 480 files
            ref_cache[str(visit)] = digest(catread.run_serial(ref_fn, files))
        return ref_cache[str(visit)]

    def check(res, files, visit):
        if args.no_verify or res[0] is None or isinstance(res[0], (bytes, bytearray)):
            return "-"
        d, r = digest(res), reference(files, visit)
        return "OK" if (d[0] == r[0] and abs(d[1] - r[1]) <= 1e-9 * max(1.0, abs(r[1]))) else f"MISMATCH {d} vs {r}"

    files = visit_files(args.visit)
    if len(files) == 0:
        print("no files in", args.visit, file=sys.stderr)
        return 2
    print(f"visit {args.visit}: {len(files)} files; {len(cfgs)} configs; states={states}")

    for state in [s for s in states if s in ("warm", "evict")]:
        for cfg in cfgs:
            times = []
            for rep in range(args.reps + (1 if state == "warm" else 0)):  # +1 untimed warm-up
                if state == "evict":
                    catread.evict(files)
                t = time.perf_counter()
                res = run_config(*cfg[:2], cfg[2], files)
                sec = time.perf_counter() - t
                if state == "warm" and rep == 0:
                    continue
                times.append(sec)
                if rep == 0 or (state == "warm" and rep == 1):
                    ok = check(res, files, args.visit)
                del res
            for sec in times:
                record(state, cfg, args.visit, files, sec, ok)

    if "cold" in states:
        if not args.cold_pool:
            print("--cold-pool required for state=cold", file=sys.stderr)
            return 2
        pool = [v for p in args.cold_pool for v in list_pool(p, args.cold_state)]
        need = len(cfgs) * args.reps
        if len(pool) < need:
            print(f"cold pool has {len(pool)} unused visits, need {need}", file=sys.stderr)
            return 2
        rng = random.Random(args.seed)
        rng.shuffle(pool)      # spread trials over nights / times of day
        it = iter(pool)
        for rnd in range(args.reps):
            order = cfgs[:]
            rng.shuffle(order)
            for cfg in order:
                visit = next(it)
                vf = visit_files(visit)
                mark_used(args.cold_state, visit)
                catread.evict(vf)
                t = time.perf_counter()
                res = run_config(*cfg[:2], cfg[2], vf)
                sec = time.perf_counter() - t
                ok = check(res, vf, visit)
                record("cold", cfg, visit, vf, sec, ok)

    new = not csv_path.exists()
    with csv_path.open("a", newline="") as f:
        w = csv.DictWriter(f, fieldnames=list(rows[0].keys()))
        if new:
            w.writeheader()
        w.writerows(rows)
    print("csv:", csv_path)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
