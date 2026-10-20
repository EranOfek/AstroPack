#!/usr/bin/env python3
"""Summarise bench csv files (from bench_catalog_read.py / .m): median/min/max per config and state.

usage: summarize_bench.py a.csv b.csv ...
Cold trials read different visits (sizes differ a little) so MB/s = median of per-trial MB/s.
"""
import csv
import statistics as st
import sys
from collections import defaultdict

rows = []
for f in sys.argv[1:]:
    with open(f, newline="") as fh:
        rows += list(csv.DictReader(fh))

g = defaultdict(list)
for r in rows:
    drv = r.get("driver", "")
    key = (r["state"], r["lang"], r["method"] + ("/" + drv if drv else ""), int(r["threads"]))
    g[key].append((float(r["sec"]), float(r["MBps"]), r.get("ok", "")))

order = {"warm": 0, "evict": 1, "cold": 2}
print(f"{'state':6s} {'lang':7s} {'method':26s} {'thr':>4s} {'n':>3s} {'median_s':>9s} {'min_s':>8s} {'max_s':>8s} {'med_MB/s':>9s}  verified")
for key in sorted(g, key=lambda k: (order[k[0]], k[1], st.median(x[0] for x in g[k]))):
    v = g[key]
    secs = [x[0] for x in v]
    ok = "OK" if all(x[2] in ("OK", "-") for x in v) else "MISMATCH"
    print(f"{key[0]:6s} {key[1]:7s} {key[2]:26s} {key[3]:4d} {len(v):3d} {st.median(secs):9.3f} {min(secs):8.3f} {max(secs):8.3f} {st.median(x[1] for x in v):9.1f}  {ok}")
