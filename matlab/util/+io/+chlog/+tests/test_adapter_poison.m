function [ok, errMsg, reject] = test_adapter_poison(rows, token) %#ok<INUSD>
%TEST_ADAPTER_POISON  Test adapter: rejects any batch containing the word POISON, accepts the rest.
%   Records what it accepted in the global TEST_ACCEPTED (cell array of rows' messages).
global TEST_ACCEPTED
ok = true; errMsg = ''; reject = false;
if ischar(rows)
    msgs = {rows};
elseif istable(rows)
    msgs = rows.message;
elseif isstruct(rows)
    msgs = arrayfun(@(R) R.message, rows, 'UniformOutput', false);
else
    msgs = {rows.message};
end
if any(cellfun(@(m) ~isempty(strfind(m, 'POISON')), msgs)) %#ok<STREMP>
    ok = false; errMsg = 'bad data'; reject = true;
    return;
end
TEST_ACCEPTED = [TEST_ACCEPTED msgs];
end
