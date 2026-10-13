#!/usr/bin/env python3
"""Thread-sweep benchmark for proc Cat FITS read I/O (raw / astropy table / header)."""

from __future__ import annotations

import argparse
import os
import sys
import time
from concurrent.futures import ProcessPoolExecutor, ThreadPoolExecutor
from datetime import datetime
from pathlib import Path
from typing import List, Sequence, Tuple


def discover_visits(parent: Path, max_visits: int) -> List[Path]:
    visits: List[Path] = []
    for entry in sorted(parent.iterdir()):
        if not entry.is_dir():
            continue
        if entry.name in (".", ".."):
            continue
        if "v0" not in entry.name:
            continue
        if "re" in str(entry):
            continue
        visits.append(entry)
        if len(visits) >= max_visits:
            break
    return visits


def list_catalog_files(visit_dir: Path, template: str) -> List[Path]:
    # Non-recursive glob in visit dir (matches io.files.filelist spirit for simple wildcards).
    if "*" in template:
        return sorted(visit_dir.glob(template))
    return sorted([visit_dir / template] if (visit_dir / template).exists() else [])


def file_stats(paths: Sequence[Path]) -> Tuple[int, float]:
    total = sum(p.stat().st_size for p in paths if p.is_file())
    return len(paths), total / (1024**2)


def read_file_bytes(path: str) -> None:
    with open(path, "rb") as fid:
        while True:
            chunk = fid.read(1024 * 1024)
            if not chunk:
                break


def read_fits_table(path: str) -> None:
    from astropy.io import fits  # lazy import

    with fits.open(path, memmap=False) as hdul:
        for hdu in hdul:
            if hasattr(hdu, "columns") and hdu.columns is not None:
                _ = hdu.data
                return
        raise RuntimeError(f"No table HDU in {path}")


def read_fits_header_hdu3(path: str) -> None:
    from astropy.io import fits

    with fits.open(path, memmap=False) as hdul:
        if len(hdul) <= 3:
            _ = hdul[0].header
        else:
            _ = hdul[3].header


def run_pool(mode: str, paths: List[Path], threads: int) -> float:
    n = len(paths)
    str_paths = [str(p) for p in paths]
    t0 = time.perf_counter()
    if threads <= 1:
        fn = {"raw": read_file_bytes, "astro": read_fits_table, "header": read_fits_header_hdu3}[mode]
        for p in str_paths:
            fn(p)
    else:
        if mode == "raw":
            with ThreadPoolExecutor(max_workers=threads) as ex:
                list(ex.map(read_file_bytes, str_paths))
        else:
            fn = read_fits_table if mode == "astro" else read_fits_header_hdu3
            with ProcessPoolExecutor(max_workers=threads) as ex:
                list(ex.map(fn, str_paths))
    return time.perf_counter() - t0


def mbps(size_mb: float, sec: float) -> float:
    if sec <= 0 or size_mb <= 0:
        return float("nan")
    return size_mb / sec


def log_line(fid, msg: str) -> None:
    fid.write(f"{datetime.now().strftime('%Y-%m-%d %H:%M:%S')} {msg}\n")
    fid.flush()


def parse_threads(s: str) -> List[int]:
    out = [int(x.strip()) for x in s.split(",") if x.strip()]
    if not out:
        raise ValueError("empty --threads")
    return out


def resolve_modes(mode: str) -> List[str]:
    if mode == "all":
        return ["raw", "astro", "header"]
    return [mode]


def main(argv: Sequence[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description="Proc catalog FITS read benchmark")
    parser.add_argument("parent", type=Path, help="proc/ directory with *v0 visit subdirs")
    parser.add_argument("--threads", default="1", help="Comma list, e.g. 1,2,4,8,16")
    parser.add_argument("--mode", default="all", choices=["raw", "astro", "header", "all"])
    parser.add_argument("--max-visits", type=int, default=1)
    parser.add_argument("--template", default="LAST*proc_Cat_1.fits*")
    parser.add_argument("--log-file", default="")
    args = parser.parse_args(argv)

    parent = args.parent.resolve()
    if not parent.is_dir():
        print(f"parent not found: {parent}", file=sys.stderr)
        return 2

    threads_list = parse_threads(args.threads)
    modes = resolve_modes(args.mode)
    visits = discover_visits(parent, args.max_visits)
    if not visits:
        print(f"no visits under {parent}", file=sys.stderr)
        return 2

    log_path = Path(args.log_file) if args.log_file else Path.cwd() / f"bench_catalog_read_{datetime.now():%Y%m%d_%H%M%S}.log"
    results: List[dict] = []

    with log_path.open("a", encoding="utf-8") as fid:
        log_line(fid, f"START python Parent={parent} Modes={','.join(modes)} Threads={threads_list} MaxVisits={args.max_visits}")
        log_line(fid, f"HOST {os.uname().nodename}")

        for visit in visits:
            files = list_catalog_files(visit, args.template)
            if not files:
                log_line(fid, f"SKIP visit={visit} reason=noFiles")
                continue
            nfiles, size_mb = file_stats(files)
            log_line(fid, f"visit={visit} nFiles={nfiles} sizeMB={size_mb:.2f}")

            for mode in modes:
                if mode in ("astro", "header"):
                    try:
                        import astropy  # noqa: F401
                    except ImportError:
                        log_line(fid, f"SKIP mode={mode} reason=noAstropy")
                        print(f"bench_catalog_read.py: skip {mode} (install astropy)", file=sys.stderr)
                        continue
                for nth in threads_list:
                    try:
                        sec = run_pool(mode, files, nth)
                    except Exception as exc:  # noqa: BLE001
                        log_line(fid, f"ERROR visit={visit} mode={mode} threads={nth} {exc}")
                        print(f"ERROR {mode} threads={nth}: {exc}", file=sys.stderr)
                        continue
                    rate = mbps(size_mb, sec)
                    results.append(
                        {"visit": str(visit), "mode": mode, "threads": nth, "n": nfiles, "sizeMB": size_mb, "sec": sec, "MBps": rate}
                    )
                    log_line(
                        fid,
                        f"RESULT visit={visit} mode={mode} threads={nth} n={nfiles} sizeMB={size_mb:.2f} sec={sec:.3f} MBps={rate:.2f}",
                    )
                    print(
                        f"bench_catalog_read {mode} threads={nth} n={nfiles} sizeMB={size_mb:.2f} sec={sec:.3f} MBps={rate:.2f} {visit}"
                    )

        # summary
        for mode in modes:
            for nth in threads_list:
                sub = [r for r in results if r["mode"] == mode and r["threads"] == nth]
                if not sub:
                    continue
                rates = [r["MBps"] for r in sub]
                import statistics

                log_line(
                    fid,
                    f"SUMMARY mode={mode} threads={nth} nRuns={len(sub)} MBps_mean={statistics.fmean(rates):.2f} MBps_median={statistics.median(rates):.2f}",
                )
        log_line(fid, "END")

    print(f"log: {log_path}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
