%==========================================================================
% Project     : ULTRASAT SOC
% Filename    : ultrasat.services.on_demand.onDemandUtcNow
% Author      : Chen Tishler
% Created     : 19/11/2026
% Modified    : 19/11/2026
% Description : ISO-8601 UTC timestamp string for on-demand status files.
%==========================================================================

function S = onDemandUtcNow()
    S = char(datetime('now', 'TimeZone', 'UTC', 'Format', "yyyy-MM-dd'T'HH:mm:ss'Z'"));
end