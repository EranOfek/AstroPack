clickhouse-client --user default --password --query "
SELECT
    database,
    table,
    mutation_id,
    command,
    parts_to_do,
    latest_fail_reason
FROM system.mutations
WHERE NOT is_done
FORMAT PrettyCompact
"


clickhouse-client --user default --password --query "
SELECT
    database,
    table,
    elapsed,
    progress,
    num_parts
FROM system.merges
FORMAT PrettyCompact
"


clickhouse-client --user default --password  --multiquery <<'SQL'
SYSTEM STOP MERGES;
SYSTEM STOP TTL MERGES;
SYSTEM STOP MOVES;
SYSTEM FLUSH LOGS;
SQL


