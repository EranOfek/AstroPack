#!/usr/bin/env python3
# ***************************************************************************
# Project     : AstroPack / on_demand
# Filename    : +on_demand/python_client/test_on_demand_client.py
# Author      : Chen Tishler
# Created     : 19/11/2026
# Modified    : 19/11/2026
# Description : pytest for on_demand_client (no MATLAB required).
# ***************************************************************************

from __future__ import annotations

import json
import threading
import time
from pathlib import Path

import pytest

SCRIPT_DIR = Path(__file__).resolve().parent
import sys

sys.path.insert(0, str(SCRIPT_DIR))

from on_demand_client import OnDemandClient, JobHandle, local_path, matlab_path


def test_matlab_path_forward_slash(tmp_path: Path) -> None:
    p = tmp_path / "a" / "b.json"
    p.parent.mkdir(parents=True, exist_ok=True)
    p.touch()
    out = matlab_path(p)
    assert "/" in out
    assert "\\" not in out


def test_local_path_round_trip(tmp_path: Path) -> None:
    rel = tmp_path / "work" / "req"
    rel.mkdir(parents=True)
    fwd = matlab_path(rel)
    back = local_path(fwd)
    assert back.resolve() == rel.resolve()


def _fake_worker(inbox: Path, stop: threading.Event) -> None:
    while not stop.is_set():
        for trigger in inbox.glob("*.json"):
            if ".out." in trigger.name or ".tmp" in trigger.name:
                continue
            if trigger.name.endswith(".out.json"):
                continue
            out = trigger.with_name(trigger.name.replace(".json", ".out.json"))
            if out.exists():
                continue
            payload = json.loads(trigger.read_text(encoding="utf-8"))
            if payload.get("action") == "health":
                body = {"status": "ok", "message": "health: OK"}
            else:
                body = {
                    "status": "ok",
                    "message": "mock",
                    "request_id": payload.get("request_id", ""),
                }
            tmp = out.with_suffix(".json.tmp")
            tmp.write_text(json.dumps(body), encoding="utf-8")
            tmp.replace(out)
            return
        time.sleep(0.05)


def test_submit_and_wait_with_mock_worker(tmp_path: Path) -> None:
    client = OnDemandClient(on_demand_root=tmp_path, poll_sec=0.05, enable_chlog=False)
    inbox = client.inbox_dir("forced_photometry")
    inbox.mkdir(parents=True, exist_ok=True)
    stop = threading.Event()
    worker = threading.Thread(target=_fake_worker, args=(inbox, stop), daemon=True)
    worker.start()

    work = {
        "action": "forced_photometry",
        "request_id": "12-00-00-0001",
        "dry_run": True,
        "image_file": "//host/share/img.fits",
        "sources": [{"ra_deg": 0, "dec_deg": 0}],
    }
    handle = client.submit_job("forced_photometry", work, timeout_sec=60)
    result = client.wait_for_job(handle, timeout_sec=5.0)
    stop.set()
    assert result.status == "ok"
    assert isinstance(handle, JobHandle)