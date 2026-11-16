function [Ok, ErrMsg] = test_adapter_mock(Rows, Token, Cfg)
%TEST_ADAPTER_MOCK  Offline test insert adapter (spool_dir/mock_inserted.jsonl, mock_fail.flag).
%   Used only by matlab/tests/run_tests.m — not part of the production +chlog package.
Ok = false; ErrMsg = '';
Spool = Cfg.client.spool_dir;
if isempty(regexp(Spool, '^([A-Za-z]:[\\/]|[\\/])', 'once')), Spool = fullfile(pwd, Spool); end
if exist(fullfile(Spool, 'mock_fail.flag'), 'file')
    ErrMsg = 'mock outage';
    return;
end
TokenFile = fullfile(Spool, 'mock_tokens.txt');
if ~isempty(Token)
    if exist(TokenFile, 'file')
        Seen = strsplit(strtrim(fileread(TokenFile)), char(10));
        if any(strcmp(Seen, Token)), Ok = true; return; end
    end
    Fid = fopen(TokenFile, 'ab'); fwrite(Fid, uint8([Token char(10)])); fclose(Fid);
end
if ischar(Rows)
    Text = Rows;
else
    if isa(Rows, 'table'), Rows = table2struct(Rows); end
    Parts = cell(1, numel(Rows));
    for K = 1:numel(Rows)
        Parts{K} = io.chlog.ChLogger.rowLine(Rows(K));
    end
    Text = [strjoin(Parts, char(10)) char(10)];
end
if ~exist(Spool, 'dir'), mkdir(Spool); end
Fid = fopen(fullfile(Spool, 'mock_inserted.jsonl'), 'ab');
fwrite(Fid, uint8(Text));
fclose(Fid);
Ok = true;
end
