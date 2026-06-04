%==========================================================================
% Project     : ULTRASAT SOC - Pipeline Monitor
% Filename    : soc.monitor.reset.m
% Author      : Chen Tishler
% Created     : 04/06/2026
% Updated     : 04/06/2026
% Description : Clear monitor singleton for tests and debugging
%==========================================================================

function reset()
    % reset  Clear soc.monitor singleton (for tests and debugging).
    soc.monitor.get_client([]);
end
