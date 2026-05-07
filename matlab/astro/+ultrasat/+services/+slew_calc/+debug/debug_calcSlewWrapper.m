%==========================================================================
% Project     : ULTRASAT SOC
% Filename    : ultrasat/+services/+slew_calc/debug/debug_calcSlewWrapper.m
% Author      : Chen Tishler
% Created     : 02/11/2025
% Modified    : 10/02/2026
% Description : Debug function for calcSlewWrapper
%==========================================================================

function debug_calcSlewWrapper()
    % Debug function for calcSlewWrapper

    % Direct calll to calcSlew
    jd = celestial.time.julday('2029-01-01T00:00:00Z');
    [T_sec,DirectSlewBool] = ultrasat.tools.calcSlew(0,0, -pi,0, 'JD', jd);
    fprintf('T_sec: %f, DirectSlewBool: %d\n', T_sec, DirectSlewBool);

    Result = ultrasat.services.slew_calc.calcSlewWrapper(0,0, -pi,0, '2029-01-01T00:00:00Z');
    fprintf('T_sec: %f, DirectSlewBool: %d\n', Result.slew, Result.direct);
end
