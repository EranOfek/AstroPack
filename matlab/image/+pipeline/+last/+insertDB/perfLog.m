function perfLog(Message, Seconds)
% perfLog Append one performance log line for LAST insertArchive *_perf clones.
% Input:  Message - text (include clone name in Message)
%         Seconds - optional; appended as duration e.g. 0.1s
%
% LogFile alternative (disabled):
%   % L = LogFile('insertDB_perf', 'SubFolder', 'last/insertArchive', 'DayFolder', true);
%   % L.write(Message);

    Enabled = true;
    LogDir = '~/matlab/log/last/insertArchive/';
    LogBaseName = 'insertDB_perf.log';

    if ~Enabled
        return
    end

    LogDir = expandHome_(LogDir);
    if ~isfolder(LogDir)
        mkdir(LogDir);
    end
    LogPath = fullfile(LogDir, LogBaseName);

    Ts = datestr(now, 'yyyy-mm-dd HH:MM:SS.FFF');
    if nargin >= 2 && ~isempty(Seconds)
        Line = sprintf('%s %s %.1fs', Ts, Message, Seconds);
    else
        Line = sprintf('%s %s', Ts, Message);
    end

    Fid = fopen(LogPath, 'a');
    if Fid < 0
        return
    end
    fprintf(Fid, '%s\n', Line);
    fflush(Fid);
    fclose(Fid);
end

function Path = expandHome_(Path)
    if startsWith(Path, '~')
        if ispc
            Home = getenv('USERPROFILE');
        else
            Home = getenv('HOME');
        end
        if isempty(Home)
            Home = char(java.lang.System.getProperty('user.home'));
        end
        if startsWith(Path, '~/') || strcmp(Path, '~')
            Path = fullfile(Home, Path(3:end));
        else
            Path = fullfile(Home, Path(2:end));
        end
    end
end
