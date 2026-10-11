#!/usr/bin/env python3
"""Clone ClickHouse table structures from last.* to last_perf.*_perf (no data copy)."""

from __future__ import annotations

import os
import re
import subprocess
import sys
from pathlib import Path

SCRIPT_DIR = Path(__file__).resolve().parent

TABLES = (
    ("proc_src", "proc_src_perf"),
    ("visit_src", "visit_src_perf"),
    ("visit_asteroids", "visit_asteroids_perf"),
    ("visit_images", "visit_images_perf"),
)


def load_dotenv(path: Path) -> None:
    if not path.is_file():
        return
    for line in path.read_text(encoding="utf-8").splitlines():
        line = line.strip()
        if not line or line.startswith("#") or "=" not in line:
            continue
        key, _, val = line.partition("=")
        key = key.strip()
        val = val.strip().strip('"').strip("'")
        if key and key not in os.environ:
            os.environ[key] = val


def ch_query(client_cmd: list[str], sql: str) -> str:
    proc = subprocess.run(
        client_cmd + ["--query", sql],
        check=True,
        capture_output=True,
        text=True,
    )
    return proc.stdout.strip()


def table_exists(client_cmd: list[str], db: str, table: str) -> bool:
    out = ch_query(
        client_cmd,
        f"EXISTS TABLE {db}.{table}",
    )
    return out == "1"


def rewrite_create_statement(create_sql: str, src_db: str, src_table: str, dst_db: str, dst_table: str) -> str:
    # Replace first occurrence of `db`.`table` or db.table in CREATE statement
    pattern = re.compile(
        rf"`?{re.escape(src_db)}`?\.`?{re.escape(src_table)}`?",
        re.IGNORECASE,
    )
    new_sql, n = pattern.subn(f"`{dst_db}`.`{dst_table}`", create_sql, count=1)
    if n != 1:
        # Fallback: replace table name only after ON CLUSTER or CREATE TABLE
        new_sql = create_sql.replace(f"{src_db}.{src_table}", f"{dst_db}.{dst_table}", 1)
        new_sql = new_sql.replace(f"`{src_db}`.`{src_table}`", f"`{dst_db}`.`{dst_table}`", 1)
    return new_sql


def main() -> int:
    load_dotenv(SCRIPT_DIR / ".env")

    host = os.environ.get("CLICKHOUSE_HOST", "localhost")
    port = os.environ.get("CLICKHOUSE_PORT", "9000")
    user = os.environ.get("CLICKHOUSE_USER", "default")
    password = os.environ.get("CLICKHOUSE_PASSWORD", "")

    if not password:
        print("CLICKHOUSE_PASSWORD is not set (use .env next to this script)", file=sys.stderr)
        return 1

    client_cmd = [
        "clickhouse-client",
        "--host",
        host,
        "--port",
        str(port),
        "--user",
        user,
        "--password",
        password,
    ]

    src_db = "last"
    dst_db = "last_perf"

    print(f"CREATE DATABASE IF NOT EXISTS {dst_db}")
    ch_query(client_cmd, f"CREATE DATABASE IF NOT EXISTS {dst_db}")

    for src_table, dst_table in TABLES:
        if table_exists(client_cmd, dst_db, dst_table):
            print(f"skip {dst_db}.{dst_table} (already exists)")
            continue
        create_sql = ch_query(client_cmd, f"SHOW CREATE TABLE {src_db}.{src_table}")
        new_sql = rewrite_create_statement(create_sql, src_db, src_table, dst_db, dst_table)
        print(f"creating {dst_db}.{dst_table}")
        ch_query(client_cmd, new_sql)

    print("done")
    return 0


if __name__ == "__main__":
    sys.exit(main())
