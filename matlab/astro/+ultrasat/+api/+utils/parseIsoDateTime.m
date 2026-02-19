%==========================================================================
% Project     : ULTRASAT Planner
% File        : +api/+utils/parseIsoDateTime.m
% Author      : Chen Tishler
% Created     : 05/10/2025
% Updated     : 05/11/2025
% Description : Parse ISO 8601 datetime strings - thin wrapper for DateTimeUtils
%==========================================================================

function dt = parseIsoDateTime(str)
    dt = ultrasat.api.utils.DateTimeUtils.parseIsoDateTime(str);
end
