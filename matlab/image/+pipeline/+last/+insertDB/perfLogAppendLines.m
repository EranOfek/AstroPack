function perfLogAppendLines(Lines, Args)
% perfLogAppendLines Append raw lines to insertDB_perf.log (used by perfLog helpers).
% Input:  Lines - cellstr or char; use '' for a blank line (no timestamp).
%         'Echo' - if true, also fprintf each non-empty line to the command window.

    arguments
        Lines
        Args.Echo(1,1) logical = false
    end

    Enabled = true;
    LogDir = '~/matlab/log/last/insertArchive/';
    LogBaseName = 'insertDB_perf.log';

    if ~Enabled
        return
    end
    if ~iscell(Lines)
        Lines = {Lines};
    end

    LogDir = expandHome_(LogDir);
    if ~isfolder(LogDir)
        mkdir(LogDir);
    end
    LogPath = fullfile(LogDir, LogBaseName);

    persistent Announced
    if isempty(Announced)
        fprintf(1, 'insertDB_perf log: %s\n', LogPath);
        Announced = true;
    end

    Fid = fopen(LogPath, 'a');
    if Fid < 0
        return
    end
    for I = 1:numel(Lines)
        if isempty(Lines{I})
            fprintf(Fid, '\n');
        else
            fprintf(Fid, '%s\n', Lines{I});
            if Args.Echo
                fprintf(1, '%s\n', Lines{I});
            end
        end
    end
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
