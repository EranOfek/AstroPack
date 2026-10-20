classdef MockPgConn < handle
%MOCKPGCONN  Test double for the Database Toolbox PostgreSQL connection.
%   Used ONLY to run the StatusDB tests in GNU Octave (no Database Toolbox),
%   against a real PostgreSQL server through the psql command-line client.
%   Implements execute, fetch (returns a struct of cellstr columns), close, isopen.
%   Never put this folder on the MATLAB path on a machine with the toolbox.
    properties
        Server = 'localhost'
        Port = 5432
        Database = ''
        User = ''
        Password = ''
        Open = true
    end
    methods
        function execute(obj, sql)
            run_psql(obj, sql, false);
        end
        function R = fetch(obj, sql)
            out = run_psql(obj, sql, true);
            R = parse_unaligned(out);
        end
        function close(obj)
            obj.Open = false;
        end
        function tf = isopen(obj)
            tf = obj.Open;
        end
    end
end

function out = run_psql(obj, sql, tabular)
if ~obj.Open
    error('MockPgConn:closed', 'Connection is closed.');
end
f = [tempname '.sql'];
o = [tempname '.out'];
e = [tempname '.err'];
fid = fopen(f, 'w');
if tabular
    fprintf(fid, '\\pset format unaligned\n\\pset fieldsep_zero\n\\pset recordsep ''\\036''\n\\pset footer off\n');
end
fwrite(fid, sql);
fprintf(fid, ';\n');
fclose(fid);
cmd = sprintf(['PGPASSWORD=%s PGCLIENTENCODING=UTF8 psql -X -q -v ON_ERROR_STOP=1 ' ...
               '-h %s -p %d -U %s -d %s -f %s > %s 2> %s'], ...
              sh(obj.Password), sh(obj.Server), obj.Port, sh(obj.User), sh(obj.Database), ...
              sh(f), sh(o), sh(e));
st = system(cmd);
out = readall(o);
err = readall(e);
delete(f); delete(o); delete(e);
if st ~= 0
    error('MockPgConn:sql', '%s', strtrim(err));
end
end

function R = parse_unaligned(out)
out = regexprep(out, '[\x1e\n]+$', '');
recs = strsplit(out, char(30), 'CollapseDelimiters', false);
hdr = strsplit(recs{1}, char(0), 'CollapseDelimiters', false);
R = struct();
for j = 1:numel(hdr)
    R.(hdr{j}) = cell(numel(recs) - 1, 1);
end
for i = 2:numel(recs)
    r = recs{i};
    if ~isempty(r) && r(1) == sprintf('\n')
        r = r(2:end);
    end
    v = strsplit(r, char(0), 'CollapseDelimiters', false);
    for j = 1:numel(hdr)
        R.(hdr{j}){i-1} = v{j};
    end
end
end

function s = readall(f)
fid = fopen(f, 'r');
s = fread(fid, Inf, '*char')';
fclose(fid);
end

function q = sh(s)
if isnumeric(s), s = num2str(s); end
q = ['''' strrep(s, '''', '''\''''') ''''];
end
