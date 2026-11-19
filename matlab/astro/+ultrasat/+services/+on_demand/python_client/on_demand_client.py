#!/usr/bin/env python3
# ***************************************************************************
# Project     : AstroPack / on_demand
# Filename    : +on_demand/python_client/on_demand_client.py
# Author      : Chen Tishler
# Created     : 19/11/2026
# Modified    : 19/11/2026
# Description : Single-file Python client for ON_DEMAND_PATH file-IPC (portal / LAST).
#               Creates workdir + inbox trigger, polls .out.json and optional workdir status.
# ***************************************************************************
"""
On-demand pipeline client — drop-in for Science Portal developers.

Requires ON_DEMAND_PATH (or pass on_demand_root to OnDemandClient).

Optional ClickHouse logging: set ON_DEMAND_CHLOG=1 and ensure `chlog` is importable
(astropy-clickhouse utils/logging on PYTHONPATH).

Example::

    from on_demand_client import OnDemandClient

    client = OnDemandClient()
  result = client.health_check("forced_photometry")
"""

from __future__ import annotations

import json
import os
import sys
import time
import uuid
from dataclasses import dataclass, field
from datetime import datetime, timezone
from pathlib import Path
from typing import Any, Literal

DEFAULT_POLL_SEC = 0.2
SUPPORTED_TYPES = frozenset({"forced_photometry", "custom_coadd"})
STATUS_TERMINAL = frozenset({"ok", "error"})


# ---------------------------------------------------------------------------
# Path helpers (GW experiment_share_layout compatible)
# ---------------------------------------------------------------------------

def utc_now_iso() -> str:
    """Return current UTC time as ISO-8601 string."""
    return datetime.now(timezone.utc).isoformat()


def _unc_forward_slash(unc_path: str) -> str:
    text = unc_path.replace("\\", "/")
    if text.startswith("//"):
        return text
    if text.startswith("\\\\"):
        return "//" + text.lstrip("\\/")
    return text


def matlab_path(path: Path) -> str:
    """Forward-slash path for JSON (UNC as //host/share/...)."""
    resolved = path.resolve()
    text = str(resolved).replace("\\", "/")
    if text.startswith("//") or text.startswith("\\\\"):
        return _unc_forward_slash(text)
    return text


def local_path(path_str: str) -> Path:
    """Resolve a JSON path string to a local Path for Python I/O."""
    text = (path_str or "").strip()
    if not text:
        return Path(text)
    normalized = text.replace("\\", "/")
    if normalized.startswith("//"):
        return Path(normalized.replace("/", os.sep))
    if normalized.startswith("\\\\"):
        return Path(text)
    candidate = Path(text.replace("/", os.sep))
    return candidate


def default_on_demand_root() -> Path:
    """Read ON_DEMAND_PATH from environment."""
    env = os.getenv("ON_DEMAND_PATH")
    if not env:
        raise RuntimeError("ON_DEMAND_PATH environment variable is not set")
    return Path(env)


def allocate_request_id(seq: int | None = None) -> str:
    """UTC HH-MM-SS-NNNN request id (SPEC §3)."""
    now = datetime.now(timezone.utc)
    if seq is None:
        seq = int(now.microsecond // 100) % 10000
    return f"{now:%H-%M-%S}-{seq:04d}"


# ---------------------------------------------------------------------------
# Optional chlog (astro-clickhouse utils/logging/chlog)
# ---------------------------------------------------------------------------

class _OptionalChLog:
    """Thin wrapper; never raises if chlog is absent."""

    def __init__(
        self,
        *,
        enabled: bool,
        config_path: str | None,
        process: str,
    ) -> None:
        self._log = None
        if not enabled:
            return
        try:
            import chlog  # type: ignore

            self._log = chlog.init(config_path, process=process)
        except Exception:
            self._log = None

    def inf(self, message: str, **attrs: Any) -> None:
        if self._log is None:
            return
        try:
            self._log.inf(message, **attrs)
        except Exception:
            pass

    def err(self, message: str, **attrs: Any) -> None:
        if self._log is None:
            return
        try:
            self._log.err(message, **attrs)
        except Exception:
            pass

    def close(self) -> None:
        if self._log is None:
            return
        try:
            self._log.close()
        except Exception:
            pass


def chlog_enabled_from_env(explicit: bool | None) -> bool:
    if explicit is not None:
        return explicit
    val = (os.getenv("ON_DEMAND_CHLOG") or "").strip().lower()
    return val in {"1", "true", "yes", "on"}


# ---------------------------------------------------------------------------
# Data classes
# ---------------------------------------------------------------------------

@dataclass
class JobHandle:
    """Submitted job paths and correlation id."""

    processing_type: str
    request_id: str
    work_folder: Path
    request_file: Path
    trigger_basename: str
    corr_id: str = field(default="")


@dataclass
class JobResult:
    """Outcome after wait_for_job."""

    status: str
    message: str
    out_payload: dict[str, Any] | None = None
    status_doc: dict[str, Any] | None = None
    result_doc: dict[str, Any] | None = None
    completed_at: str = ""


# ---------------------------------------------------------------------------
# Client
# ---------------------------------------------------------------------------

class OnDemandClient:
    """
    Producer client for on-demand MATLAB workers (file IPC).

    Input:
        on_demand_root - ON_DEMAND_PATH; default from environment
        poll_sec       - inbox poll interval
        enable_chlog   - optional ClickHouse client logging
        chlog_config   - chlog.json path (optional)
        process        - chlog process name
    """

    def __init__(
        self,
        on_demand_root: Path | str | None = None,
        *,
        poll_sec: float = DEFAULT_POLL_SEC,
        enable_chlog: bool | None = None,
        chlog_config: str | None = None,
        process: str = "on_demand_client",
    ) -> None:
        if on_demand_root is None:
            self.root = default_on_demand_root()
        else:
            self.root = Path(on_demand_root)
        self.poll_sec = poll_sec
        ch_cfg = chlog_config or os.getenv("CHLOG_CONFIG")
        self._chlog = _OptionalChLog(
            enabled=chlog_enabled_from_env(enable_chlog),
            config_path=ch_cfg,
            process=process,
        )

    def close(self) -> None:
        """Release optional chlog."""
        self._chlog.close()

    def inbox_dir(self, processing_type: str) -> Path:
        self._check_type(processing_type)
        return self.root / processing_type / "inbox"

    def work_root(self, processing_type: str) -> Path:
        self._check_type(processing_type)
        return self.root / processing_type / "work"

    def create_workdir(
        self,
        processing_type: str,
        work_request: dict[str, Any],
        *,
        request_id: str | None = None,
        write_queued_status: bool = True,
    ) -> tuple[Path, Path]:
        """
        Create work/yyyy/mm/dd/<request_id>/ with request.json (+ optional status.json).
        Returns (work_folder, request_file) as local Paths.
        """
        self._check_type(processing_type)
        if request_id is None:
            request_id = str(work_request.get("request_id") or allocate_request_id())
        work_request = dict(work_request)
        work_request["action"] = work_request.get("action") or processing_type
        work_request["request_id"] = request_id

        now = datetime.now(timezone.utc)
        work_folder = (
            self.work_root(processing_type)
            / f"{now:%Y}"
            / f"{now:%m}"
            / f"{now:%d}"
            / request_id
        )
        work_folder.mkdir(parents=True, exist_ok=True)
        request_file = work_folder / "request.json"
        self._atomic_write_json(request_file, work_request)

        if write_queued_status:
            status = {
                "state": "queued",
                "request_id": request_id,
                "action": work_request["action"],
                "created_at": utc_now_iso(),
                "claimed_at": "",
                "started_at": "",
                "completed_at": "",
                "worker_id": "",
                "reclaim_count": 0,
                "heartbeat_at": "",
                "message": "",
            }
            self._atomic_write_json(work_folder / "status.json", status)

        return work_folder, request_file

    def build_trigger(
        self,
        processing_type: str,
        request_id: str,
        request_file: Path | str,
        work_folder: Path | str,
        *,
        timeout_sec: int | None = None,
    ) -> dict[str, Any]:
        """Short inbox trigger dict (paths as matlab_path strings)."""
        trigger: dict[str, Any] = {
            "action": processing_type,
            "request_id": request_id,
            "request_file": matlab_path(Path(request_file)),
            "work_folder": matlab_path(Path(work_folder)),
        }
        if timeout_sec is not None:
            trigger["timeout_sec"] = int(timeout_sec)
        return trigger

    def submit_trigger(
        self,
        trigger: dict[str, Any],
        *,
        trigger_basename: str | None = None,
        inbox: Path | None = None,
    ) -> str:
        """
        Atomically publish trigger JSON into inbox. Returns trigger_basename (e.g. HH-MM-SS-0001.json).
        Clears stale .out.json for that basename.
        """
        action = str(trigger.get("action") or "")
        if action == "health":
            processing_type = trigger.get("_inbox_type", "forced_photometry")
        else:
            processing_type = action
        if inbox is None:
            inbox = self.inbox_dir(str(processing_type))
        inbox.mkdir(parents=True, exist_ok=True)

        if trigger_basename is None:
            rid = trigger.get("request_id")
            if action == "health":
                trigger_basename = f"health-{uuid.uuid4().hex[:8]}.json"
            elif rid:
                trigger_basename = f"{rid}.json"
            else:
                trigger_basename = f"{allocate_request_id()}.json"

        ipc_request = inbox / trigger_basename
        ipc_out = inbox / trigger_basename.replace(".json", ".out.json")
        tmp_path = inbox / f"{trigger_basename}.tmp"

        if ipc_out.exists():
            ipc_out.unlink()

        tmp_path.write_text(json.dumps(trigger, indent=2), encoding="utf-8")
        tmp_path.replace(ipc_request)

        self._chlog.inf(
            "job.submit",
            corr_id=self._corr_id(processing_type, trigger.get("request_id", "")),
            action=action,
            request_id=trigger.get("request_id", ""),
            trigger=matlab_path(ipc_request),
        )
        return trigger_basename

    def submit_job(
        self,
        processing_type: str,
        work_request: dict[str, Any],
        *,
        timeout_sec: int | None = None,
        request_id: str | None = None,
    ) -> JobHandle:
        """Create workdir, build trigger, submit to inbox."""
        work_folder, request_file = self.create_workdir(
            processing_type, work_request, request_id=request_id
        )
        rid = str(work_request.get("request_id") or request_id)
        if not rid:
            rid = json.loads(request_file.read_text(encoding="utf-8"))["request_id"]
        trigger = self.build_trigger(
            processing_type, rid, request_file, work_folder, timeout_sec=timeout_sec
        )
        basename = self.submit_trigger(trigger)
        corr = self._corr_id(processing_type, rid)
        return JobHandle(
            processing_type=processing_type,
            request_id=rid,
            work_folder=work_folder,
            request_file=request_file,
            trigger_basename=basename,
            corr_id=corr,
        )

    def poll_out_json(
        self,
        trigger_basename: str,
        inbox: Path,
        *,
        timeout_sec: float,
    ) -> tuple[str, str, dict[str, Any] | None]:
        """Wait for inbox/<basename>.out.json (like dispatch_too_planner)."""
        out_path = inbox / trigger_basename.replace(".json", ".out.json")
        start = time.time()
        while time.time() - start < timeout_sec:
            if out_path.is_file():
                try:
                    payload = json.loads(out_path.read_text(encoding="utf-8"))
                except (json.JSONDecodeError, OSError) as exc:
                    return "error", f"invalid_out_json: {exc}", None
                status = str(payload.get("status") or "error")
                message = str(payload.get("message") or "")
                return status, message, payload
            time.sleep(self.poll_sec)
        return "timeout", f"no .out.json within {timeout_sec}s", None

    def poll_workdir(
        self,
        work_folder: Path | str,
        *,
        timeout_sec: float,
    ) -> tuple[str, str, dict[str, Any] | None, dict[str, Any] | None]:
        """Poll work_folder/status.json until terminal state."""
        folder = Path(work_folder)
        status_path = folder / "status.json"
        start = time.time()
        last_state = ""
        while time.time() - start < timeout_sec:
            if status_path.is_file():
                try:
                    status_doc = json.loads(status_path.read_text(encoding="utf-8"))
                except (json.JSONDecodeError, OSError):
                    time.sleep(self.poll_sec)
                    continue
                state = str(status_doc.get("state") or "")
                last_state = state
                if state in STATUS_TERMINAL:
                    result_doc = None
                    result_path = folder / "result.json"
                    if result_path.is_file():
                        try:
                            result_doc = json.loads(result_path.read_text(encoding="utf-8"))
                        except (json.JSONDecodeError, OSError):
                            result_doc = None
                    msg = str(status_doc.get("message") or "")
                    return state, msg, status_doc, result_doc
            time.sleep(self.poll_sec)
        return "timeout", f"workdir not terminal (last={last_state})", None, None

    def wait_for_job(
        self,
        handle: JobHandle,
        *,
        timeout_sec: float = 600.0,
        prefer: Literal["out", "workdir", "both"] = "out",
    ) -> JobResult:
        """Wait for completion via .out.json and/or workdir status."""
        inbox = self.inbox_dir(handle.processing_type)
        self._chlog.inf(
            "job.wait",
            corr_id=handle.corr_id,
            request_id=handle.request_id,
            timeout_sec=timeout_sec,
        )

        out_status, out_msg, out_payload = "timeout", "", None
        wd_status, wd_msg, status_doc, result_doc = "timeout", "", None, None

        if prefer in ("out", "both"):
            out_status, out_msg, out_payload = self.poll_out_json(
                handle.trigger_basename, inbox, timeout_sec=timeout_sec
            )

        if prefer in ("workdir", "both") or (
            prefer == "out" and out_status == "timeout"
        ):
            wd_status, wd_msg, status_doc, result_doc = self.poll_workdir(
                handle.work_folder, timeout_sec=timeout_sec
            )

        if out_status != "timeout":
            status = out_status
            message = out_msg
        else:
            status = wd_status
            message = wd_msg

        self._chlog.inf(
            "job.done",
            corr_id=handle.corr_id,
            request_id=handle.request_id,
            job_status=status,
            detail=message,
        )

        return JobResult(
            status=status,
            message=message,
            out_payload=out_payload,
            status_doc=status_doc,
            result_doc=result_doc,
            completed_at=utc_now_iso(),
        )

    def health_check(
        self,
        processing_type: str = "forced_photometry",
        *,
        timeout_sec: float = 30.0,
    ) -> JobResult:
        """Drop health trigger into type inbox and wait for .out.json."""
        trigger = {"action": "health", "_inbox_type": processing_type}
        basename = self.submit_trigger(trigger, inbox=self.inbox_dir(processing_type))
        inbox = self.inbox_dir(processing_type)
        status, message, payload = self.poll_out_json(
            basename, inbox, timeout_sec=timeout_sec
        )
        return JobResult(
            status=status,
            message=message,
            out_payload=payload,
            completed_at=utc_now_iso(),
        )

    @staticmethod
    def _atomic_write_json(path: Path, data: dict[str, Any]) -> None:
        tmp = path.with_suffix(path.suffix + ".tmp")
        tmp.write_text(json.dumps(data, indent=2), encoding="utf-8")
        tmp.replace(path)

    @staticmethod
    def _check_type(processing_type: str) -> None:
        if processing_type not in SUPPORTED_TYPES and processing_type != "health":
            raise ValueError(
                f"unsupported processing_type: {processing_type!r}; "
                f"expected one of {sorted(SUPPORTED_TYPES)}"
            )

    @staticmethod
    def _corr_id(processing_type: str, request_id: str) -> str:
        rid = request_id or "none"
        return f"{processing_type}:{rid}"


__all__ = [
    "OnDemandClient",
    "JobHandle",
    "JobResult",
    "matlab_path",
    "local_path",
    "allocate_request_id",
    "utc_now_iso",
]