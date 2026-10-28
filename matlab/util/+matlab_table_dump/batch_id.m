%% SYNCED from astro-clickhouse — edit SoT, then re-run sync
function id = batch_id(prefix, index)
%MATLAB_TABLE_DUMP.BATCH_ID  Deterministic, sortable batch id: <prefix>_<index as 9 digits>.
%
%   matlab_table_dump.batch_id('ultrasat_20261013_field0412', 17)  -> 'ultrasat_20261013_field0412_000000017'
%
%   Characters outside [A-Za-z0-9._-] in the prefix become '_'. Re-running the
%   same catalog file gives the same ids, so replays are recognized as
%   duplicates (queue) and deduplicated (ClickHouse insert_deduplication_token).
prefix = regexprep(char(prefix), '[^A-Za-z0-9._-]', '_');
id = sprintf('%s_%09d', prefix, index);
id = matlab_table_dump.internal.check_id(id, 'batch');
end
