# 08 — Database Patterns

## ClickHouse

Used for analytical and pipeline queries. Not for transactional workloads.

- Bulk inserts preferred; no row-by-row operations inside loops
- Read-heavy analytical queries on pipeline results
- Access isolated in dedicated classes

## PostgreSQL

Used for transactional data (primarily LAST mission).

## Access Layer

Database access classes live in `matlab/util/+db/`.

Connection profiles defined in `config/Database.DbConnections.*` configuration files.

## Schema Definitions

SQL schemas stored in `database/` directory at repository root.

## Rules

- Database access must be isolated in dedicated classes — no inline SQL in core logic or GUI code
- Bulk operations over row-by-row iteration
- Connection configuration via config files, never hardcoded
