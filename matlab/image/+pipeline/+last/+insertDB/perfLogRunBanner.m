function perfLogRunBanner(FnName)
% perfLogRunBanner Blank line + run-start banner + ENV line (file and console).
% Input:  FnName - clone name e.g. insertArchiveCatalogs2DB_perf

    Ts = datestr(now, 'yyyy-mm-dd HH:MM:SS.FFF');
    Banner = sprintf('============== Started: %s %s', FnName, Ts);

    Host = strtrim(getenv('INSERTDB_RUN_HOST'));
    if isempty(Host)
        try
            Host = getHostName();
        catch
            Host = 'unknown';
        end
    end

    Rel = version('-release');
    V = ver('MATLAB');
    if isempty(V)
        VerStr = 'unknown';
    else
        VerStr = V(1).Version;
    end

    EnvTs = datestr(now, 'yyyy-mm-dd HH:MM:SS.FFF');
    EnvLine = sprintf('%s %s ENV host=%s matlabRelease=%s matlabVersion=%s', ...
        EnvTs, FnName, Host, Rel, VerStr);

    pipeline.last.insertDB.perfLogAppendLines({'', Banner, EnvLine}, 'Echo', true);
end
