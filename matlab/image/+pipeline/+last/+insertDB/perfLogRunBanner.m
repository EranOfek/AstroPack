function perfLogRunBanner(FnName)
% perfLogRunBanner Blank line + run-start banner (no timestamp on banner).
% Input:  FnName - clone name e.g. insertArchiveCatalogs2DB_perf

    Ts = datestr(now, 'yyyy-mm-dd HH:MM:SS.FFF');
    Banner = sprintf('============== Started: %s %s', FnName, Ts);
    pipeline.last.insertDB.perfLogAppendLines({'', Banner});
end
