%==========================================================================
% Project     : ULTRASAT SOC - Pipeline Monitor
% Filename    : soc.monitor.normalize_info.m
% Author      : Chen Tishler
% Created     : 04/06/2026
% Updated     : 04/06/2026
% Description : Normalize info argument to struct for record data field
%==========================================================================

function Info = normalize_info(Info)
    % normalize_info  Ensure monitoring info argument is a struct for data field.
    if nargin < 1 || isempty(Info)
        Info = struct();
    elseif ~isstruct(Info)
        Info = struct('value', Info);
    end
end
