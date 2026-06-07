"""CSV/JSON I/O for the LCS CP-SAT solver."""

from __future__ import annotations

import json
from pathlib import Path
from typing import Optional, Tuple

import pandas as pd

from .models import SolverConfig, SolverResult, WindowAssignment


REQUIRED_FIELDS_COLUMNS = {"field_id", "ra", "dec", "A_U"}
REQUIRED_WINDOWS_COLUMNS = {
    "field_id",
    "vis_start_day",
    "vis_end_day",
    "window_len_days",
}
REQUIRED_ELIGIBILITY_COLUMNS = {
    "field_id",
    "eligible_abc",
    "eligible_long_window",
    "eligible_d",
}


class InputValidationError(ValueError):
    """Raised when an input file is missing required columns."""


def _validate_columns(df: pd.DataFrame, required: set, path: Path) -> None:
    missing = required - set(df.columns)
    if missing:
        raise InputValidationError(
            f"{path} is missing required columns: {sorted(missing)}"
        )


def _normalize_fields_df(df: pd.DataFrame) -> pd.DataFrame:
    rename = {}
    if "Field" in df.columns and "field_id" not in df.columns:
        rename["Field"] = "field_id"
    if "RA" in df.columns and "ra" not in df.columns:
        rename["RA"] = "ra"
    if "Dec" in df.columns and "dec" not in df.columns:
        rename["Dec"] = "dec"
    if "AU" in df.columns and "A_U" not in df.columns:
        rename["AU"] = "A_U"
    out = df.rename(columns=rename)
    _validate_columns(out, REQUIRED_FIELDS_COLUMNS, Path("fields"))
    return out


def _normalize_windows_df(df: pd.DataFrame) -> pd.DataFrame:
    rename = {}
    if "window_length" in df.columns and "window_len_days" not in df.columns:
        rename["window_length"] = "window_len_days"
    out = df.rename(columns=rename)
    _validate_columns(out, REQUIRED_WINDOWS_COLUMNS, Path("visibility_windows"))
    return out


def load_config(config_path: Optional[Path]) -> SolverConfig:
    if config_path is None or not config_path.exists():
        return SolverConfig()
    with open(config_path, encoding="utf-8") as fh:
        data = json.load(fh)
    return SolverConfig.from_dict(data)


def load_inputs(
    fields_path: Path,
    windows_path: Path,
    eligibility_path: Path,
    config_path: Optional[Path] = None,
    daily_visibility_path: Optional[Path] = None,
) -> Tuple[pd.DataFrame, pd.DataFrame, pd.DataFrame, SolverConfig, Optional[pd.DataFrame]]:
    """Load and validate solver input files."""
    fields_df = _normalize_fields_df(pd.read_csv(fields_path))
    windows_df = _normalize_windows_df(pd.read_csv(windows_path))
    eligibility_df = pd.read_csv(eligibility_path)
    _validate_columns(eligibility_df, REQUIRED_ELIGIBILITY_COLUMNS, eligibility_path)

    config = load_config(config_path)
    daily_df = None
    if daily_visibility_path is not None and daily_visibility_path.exists():
        daily_df = pd.read_csv(daily_visibility_path)

    return fields_df, windows_df, eligibility_df, config, daily_df


def write_schedule_windows(result: SolverResult, out_dir: Path) -> Path:
    rows = []
    for item in result.window_assignments:
        rows.append(
            {
                "category": item.category,
                "field_id": item.field_id,
                "cadence": item.cadence,
                "start_day": item.start_day,
                "end_day": item.end_day,
                "window_index": item.window_index,
                "group_id": item.group_id if item.group_id is not None else "",
                "notes": item.notes,
            }
        )
    out_path = out_dir / "schedule_windows.csv"
    pd.DataFrame(rows).to_csv(out_path, index=False)
    return out_path


def write_daily_schedule(result: SolverResult, out_dir: Path) -> Path:
    rows = [
        {
            "day": obs.day,
            "slot_index": obs.slot_index,
            "field_id": obs.field_id,
            "category": obs.category,
            "cadence": obs.cadence,
        }
        for obs in result.daily_observations
    ]
    out_path = out_dir / "daily_schedule.csv"
    pd.DataFrame(rows).to_csv(out_path, index=False)
    return out_path


def write_validation_report(report_df: pd.DataFrame, out_dir: Path) -> Path:
    out_path = out_dir / "validation_report.csv"
    report_df.to_csv(out_path, index=False)
    return out_path


def write_solver_summary(summary: dict, out_dir: Path) -> Path:
    out_path = out_dir / "solver_summary.json"
    with open(out_path, "w", encoding="utf-8") as fh:
        json.dump(summary, fh, indent=2)
    return out_path


def default_input_paths(base_dir: Path) -> dict:
    """Return default paths relative to lcs_solver directory."""
    data_dir = base_dir.parent / "data" / "lcs_solver_inputs"
    return {
        "fields": data_dir / "lcs_fields.csv",
        "windows": data_dir / "lcs_visibility_windows.csv",
        "eligibility": data_dir / "lcs_field_eligibility.csv",
        "config": data_dir / "lcs_params.json",
        "daily_visibility": data_dir / "lcs_daily_visibility.csv",
    }
