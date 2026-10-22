%% SYNCED from astro-clickhouse — edit SoT, then re-run sync
function ok = selftest(folder)
%MATLAB_TABLE_DUMP.SELFTEST  Quick check of the writers on this MATLAB/Octave installation.
%
%   ok = matlab_table_dump.selftest              % uses tempdir
%   ok = matlab_table_dump.selftest('/dev/shm')
%
%   Writes and reads back 64-bit integer extremes, NaN/Inf/-0 bit patterns and
%   logicals with the M engine (and the MEX engine when built), and checks that
%   both engines produce identical bytes. Run it once after installing or
%   upgrading MATLAB: if 'native' int64 fails, use 'Int64Mode','typecast'.
if nargin < 1, folder = tempdir; end
ok = true;
s.i64 = [intmin('int64'); intmax('int64'); int64(9007199254740993); int64(-9007199254740993)];
s.u64 = [uint64(0); intmax('uint64'); uint64(9223372036854775809); uint64(9007199254740993)];
s.f64 = [NaN; Inf; -Inf; -0];
s.flag = [true; false; true; false];
p = fullfile(folder, sprintf('matlab_table_dump_selftest_%d.mtd', feature_pid()));
engines = {'m'};
if exist('matlab_table_dump_mex', 'file') == 3, engines{end + 1} = 'mex'; end
bytes = {};
for e = 1:numel(engines)
    for mode = {'native', 'typecast'}
        matlab_table_dump.write(p, s, 'BatchId', 'selftest', 'Engine', engines{e}, 'Int64Mode', mode{1}, 'CreatedUtc', '2026-01-01T00:00:00.000Z', 'Producer', 'selftest');
        [d, ~] = matlab_table_dump.read(p, 'Output', 'struct');
        good = isequal(d.i64, s.i64) && isequal(d.u64, s.u64) && isequal(d.flag, s.flag) && ...
            isequal(typecast(d.f64, 'uint64'), typecast(s.f64, 'uint64'));
        if strcmp(engines{e}, 'mex') && strcmp(mode{1}, 'typecast'), continue; end   % MEX ignores Int64Mode
        fprintf('  %-4s %-8s : %s\n', engines{e}, mode{1}, pass(good));
        ok = ok && good;
        fid = fopen(p, 'r'); bytes{end + 1} = fread(fid, Inf, '*uint8'); fclose(fid); %#ok<AGROW>
    end
end
same = all(cellfun(@(b) isequal(b, bytes{1}), bytes));
fprintf('  identical bytes from every engine/mode : %s\n', pass(same));
ok = ok && same;
delete(p);
end

function s = pass(tf)
if tf, s = 'ok'; else, s = 'FAILED'; end
end

function pid = feature_pid()
if exist('OCTAVE_VERSION', 'builtin') ~= 0
    pid = getpid();
else
    pid = feature('getpid');
end
end
