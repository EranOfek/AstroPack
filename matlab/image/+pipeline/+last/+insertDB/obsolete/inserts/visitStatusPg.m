function varargout = visitStatusPg(action, FnName, varargin)
%VISITSTATUSPG  Postgres visit status helpers for insertArchive*_pg scripts.
%   [Injected, StatusText, Outcome] = visitStatusPg('read', FnName, St, DataDir, Phrases, IgnoreStatus)
%   visitStatusPg('append', FnName, St, DataDir, Line, UpdateStatus)

switch lower(action)
    case 'read'
        [St, DataDir, Phrases, IgnoreStatus] = deal(varargin{:});
        if IgnoreStatus
            varargout = {false, "", ""};
            return
        end
        TicStatus = tic;
        try
            StatusText = St.read(DataDir);
        catch ME
            rethrow(ME);
        end
        perfLog(sprintf('%s readStatusPg %s', FnName, DataDir), toc(TicStatus));
        if strlength(string(StatusText)) == 0
            varargout = {false, StatusText, 'noStatus'};
            return
        end
        Injected = false;
        for k = 1:numel(Phrases)
            if contains(StatusText, Phrases(k))
                Injected = true;
                break
            end
        end
        varargout = {Injected, StatusText, ""};
    case 'append'
        [St, DataDir, Line, UpdateStatus] = deal(varargin{:});
        if ~UpdateStatus
            return
        end
        TicSu = tic;
        St.write(DataDir, Line, 'Mode', 'append');
        perfLog(sprintf('%s updateStatusPg %s', FnName, DataDir), toc(TicSu));
    otherwise
        error('visitStatusPg:badAction', 'Unknown action %s', action);
end
end
