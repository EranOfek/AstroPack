function q = sq_shq(s)
%SQ_SHQ  Single-quote a path for a POSIX shell (private helper).
q = ['''' strrep(s, '''', '''\''''') ''''];
end
