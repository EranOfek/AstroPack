function [ok, errMsg] = test_adapter_spy(rows, token, cfg) %#ok<INUSD>
%TEST_ADAPTER_SPY  Test adapter: records the class and size of what it was given, in the global TEST_SPY.
global TEST_SPY
n = numel(rows);
if isa(rows, 'table'), n = height(rows); end   % numel(table) is rows x variables in Matlab
TEST_SPY = struct('class', class(rows), 'n', n, 'token', token, 'rows', {rows});
ok = true; errMsg = '';
end
