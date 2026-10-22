%% SYNCED from astro-clickhouse — edit SoT, then re-run sync
function waited = wait_for_space(root, maxDepth, timeout)
%COLBATCH.WAIT_FOR_SPACE  Block while the queue holds maxDepth or more batches (docs/QUEUE.md section 4).
%
%   waited = colbatch.wait_for_space(root, maxDepth)          % wait forever
%   waited = colbatch.wait_for_space(root, maxDepth, timeout) % error colbatch:queueFull after timeout s
%
%   Sleeps with back-off 1 ms -> 50 ms. Returns the seconds spent waiting.
if nargin < 3, timeout = Inf; end
t0 = tic;
delay = 0.001;
while colbatch.depth(root) >= maxDepth
    if toc(t0) >= timeout
        error('colbatch:queueFull', 'Queue %s stayed full for %g s (MaxDepth=%d).', root, timeout, maxDepth);
    end
    pause(delay);
    delay = min(delay * 2, 0.05);
end
waited = toc(t0);
end
