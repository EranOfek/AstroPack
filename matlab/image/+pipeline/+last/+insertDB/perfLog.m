function perfLog(Message, Seconds)
% perfLog Append one performance log line for LAST insertArchive *_perf clones.
% Input:  Message - text (include clone name in Message)
%         Seconds - optional; appended as duration e.g. 0.1s
%
% LogFile alternative (disabled):
%   % L = LogFile('insertDB_perf', 'SubFolder', 'last/insertArchive', 'DayFolder', true);
%   % L.write(Message);

    Ts = datestr(now, 'yyyy-mm-dd HH:MM:SS.FFF');
    if nargin >= 2 && ~isempty(Seconds)
        Line = sprintf('%s %s %.1fs', Ts, Message, Seconds);
    else
        Line = sprintf('%s %s', Ts, Message);
    end
    pipeline.last.insertDB.perfLogAppendLines(Line);
end
