%==========================================================================
% Project     : ULTRASAT SOC
% Filename    : +ultrasat/+services/+on_demand/logOnDemand.m
% Author      : Chen Tishler
% Created     : 19/11/2026
% Modified    : 19/11/2026
% Description : Log to optional ChLogger with MsgLogger fallback (never throws).
%==========================================================================

function logOnDemand(Level, Message, varargin)
    % logOnDemand Write one line to ChLogger and/or MsgLogger.
    %
    % Input  : Level   - chlog level: DBG, INF, WRN, ERR, CRT
    %          Message - text message
    %          varargin - Name-value pairs for chlog (e.g. 'CorrId', 'id-1')
    % Output : -
    % Example: logOnDemand('INF', 'claim ok', 'CorrId', 'fp:14-32-01-0007');

    L = [];
    try
        L = io.chlog.ChLogger.getSingleton();
    catch
    end

    if io.chlog.ChLogger.isValidHandle(L)
        try
            L.log(Level, Message, varargin{:});
        catch Ex
            io.msgLog(LogLevel.Debug, 'logOnDemand chlog: %s', Ex.message);
        end
    end

    LogLvl = chLevelToLogLevel(Level);
    if ~isempty(varargin)
        Corr = '';
        for K = 1:2:numel(varargin)
            if K + 1 > numel(varargin)
                break;
            end
            if strcmpi(varargin{K}, 'CorrId')
                Corr = char(string(varargin{K + 1}));
                break;
            end
        end
        if ~isempty(Corr)
            io.msgLog(LogLvl, '[%s] %s', Corr, Message);
            return;
        end
    end
    io.msgLog(LogLvl, '%s', Message);
end

% -------------------------------------------------------------------------

function Ll = chLevelToLogLevel(Level)
    switch upper(char(Level))
        case {'DBG', 'DEBUG'}
            Ll = LogLevel.Debug;
        case {'WRN', 'WARN', 'WARNING'}
            Ll = LogLevel.Warning;
        case {'ERR', 'ERROR', 'CRT', 'CRITICAL'}
            Ll = LogLevel.Error;
        otherwise
            Ll = LogLevel.Info;
    end
end