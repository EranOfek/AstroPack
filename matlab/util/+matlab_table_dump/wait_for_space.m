%% SYNCED from astro-clickhouse — edit SoT, then re-run sync
function waited = wait_for_space(root, maxDepth, timeout)
%MATLAB_TABLE_DUMP.WAIT_FOR_SPACE  Block while the queue holds maxDepth or more batches (docs/QUEUE.md section 4).
%
%   waited = matlab_table_dump.wait_for_space(root, maxDepth)          % wait forever
%   waited = matlab_table_dump.wait_for_space(root, maxDepth, timeout) % error matlab_table_dump:queueFull after timeout s
%
%   Sleeps with back-off 1 ms -> 50 ms. Returns the seconds spent waiting.
if nargin < 3, timeout = Inf; end
t0 = tic;
delay = 0.001;
while matlab_table_dump.depth(root) >= maxDepth
    if toc(t0) >= timeout
        error('matlab_table_dump:queueFull', 'Queue %s stayed full for %g s (MaxDepth=%d).', root, timeout, maxDepth);
    end
    pause(delay);
    delay = min(delay * 2, 0.05);
end
waited = toc(t0);
end
