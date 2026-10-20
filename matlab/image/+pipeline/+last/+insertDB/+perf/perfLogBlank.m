function perfLogBlank()
% perfLogBlank Append one empty line to insertDB_perf.log (no timestamp).

    pipeline.last.insertDB.perf.perfLogAppendLines({''});
end
