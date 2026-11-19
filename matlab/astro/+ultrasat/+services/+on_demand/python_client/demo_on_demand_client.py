#!/usr/bin/env python3
# ***************************************************************************
# Project     : AstroPack / on_demand
# Filename    : +on_demand/python_client/demo_on_demand_client.py
# Author      : Chen Tishler
# Created     : 19/11/2026
# Modified    : 19/11/2026
# Description : Demo CLI for OnDemandClient (health + dry_run job).
# ***************************************************************************

from __future__ import annotations

import argparse
import os
import sys
import tempfile
from pathlib import Path

SCRIPT_DIR = Path(__file__).resolve().parent
if str(SCRIPT_DIR) not in sys.path:
    sys.path.insert(0, str(SCRIPT_DIR))

from on_demand_client import OnDemandClient, allocate_request_id


def main() -> int:
    parser = argparse.ArgumentParser(description="On-demand file-IPC client demo")
    parser.add_argument("--root", type=Path, default=None, help="ON_DEMAND_PATH override")
    parser.add_argument("--type", default="forced_photometry", choices=["forced_photometry", "custom_coadd"])
    parser.add_argument("--health-only", action="store_true")
    parser.add_argument("--timeout", type=float, default=120.0)
    parser.add_argument("--chlog", action="store_true", help="Enable ON_DEMAND_CHLOG for this run")
    args = parser.parse_args()

    if args.root is None:
        if not os.getenv("ON_DEMAND_PATH"):
            args.root = Path(tempfile.mkdtemp(prefix="on_demand_demo_"))
            os.environ["ON_DEMAND_PATH"] = str(args.root)
            print(f"ON_DEMAND_PATH unset; using {args.root}")
    if args.chlog:
        os.environ["ON_DEMAND_CHLOG"] = "1"

    client = OnDemandClient(on_demand_root=args.root, enable_chlog=args.chlog)

    try:
        health = client.health_check(args.type, timeout_sec=min(args.timeout, 30))
        print(f"health: status={health.status} message={health.message}")

        if args.health_only:
            return 0 if health.status == "ok" else 1

        work = {
            "action": args.type,
            "request_id": allocate_request_id(),
            "dry_run": True,
            "image_file": "//euclid/share/demo/reference.fits",
            "sources": [{"name": "demo", "ra_deg": 1.0, "dec_deg": 2.0}],
        }
        if args.type == "custom_coadd":
            work = {
                "action": "custom_coadd",
                "request_id": allocate_request_id(),
                "dry_run": True,
                "image_files": ["//euclid/share/demo/a.fits", "//euclid/share/demo/b.fits"],
            }

        handle = client.submit_job(args.type, work, timeout_sec=int(args.timeout))
        print(f"submitted: {handle.work_folder}")
        print("Start MATLAB: ultrasat.services.on_demand.on_demand_service(...) then re-run with --wait")

        result = client.wait_for_job(handle, timeout_sec=args.timeout)
        print(f"result: status={result.status} message={result.message}")
        return 0 if result.status == "ok" else 1
    finally:
        client.close()


if __name__ == "__main__":
    raise SystemExit(main())