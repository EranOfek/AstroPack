%==========================================================================
% Project     : ULTRASAT SOC - Pipeline Monitor
% Filename    : soc.monitor.utc_now_str.m
% Author      : Chen Tishler
% Created     : 04/06/2026
% Updated     : 04/06/2026
% Description : Current UTC timestamp in ISO-8601 format with Z suffix
%==========================================================================

function Str = utc_now_str()
    % utc_now_str  Current UTC timestamp in ISO-8601 format with Z suffix.
    %
    % Output : Str - e.g. '2026-06-04T12:30:01.123Z'
    Dt = datetime('now', 'TimeZone', 'UTC');
    Str = char(Dt, 'yyyy-MM-dd''T''HH:mm:ss.SSS''Z''');
end
