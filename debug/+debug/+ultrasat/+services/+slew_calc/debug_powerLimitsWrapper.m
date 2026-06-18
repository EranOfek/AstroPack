%==========================================================================
% Project     : ULTRASAT SOC
% Filename    : +debug/+ultrasat/+services/+slew_calc/debug_powerLimitsWrapper.m
% Author      : Chen Tishler
% Created     : 31/05/2026
% Modified    : 31/05/2026
% Description : Debug function for powerLimitsWrapper and processRequest power_limits
%
% Run by      : debug.ultrasat.services.slew_calc.debug_powerLimitsWrapper()
%==========================================================================

function debug_powerLimitsWrapper()
    debug_powerLimitsWrapperDirect();
    debug_processRequestPowerLimits();
end


function debug_powerLimitsWrapperDirect()
    RAD = pi / 180;
    N1 = [220 * RAD, 0 * RAD];
    S1 = [42 * RAD, -66 * RAD];
    Coo = [N1; S1];

    Input = struct( ...
        'times', {{'2031-03-20T05:03:02Z', '2039-12-15T05:25:02Z'}}, ...
        'dod', 0, ...
        'coo', Coo);

    fprintf('=== powerLimitsWrapper direct ===\n');
    Result = ultrasat.services.slew_calc.powerLimitsWrapper(Input);
    disp(Result);

    InputNoCoo = struct('times', {{'2031-03-20T05:03:02Z'}});
    ResultNoCoo = ultrasat.services.slew_calc.powerLimitsWrapper(InputNoCoo);
    fprintf('Without coo: soft_max_sun_ang_dist = [');
    fprintf(' %.3f', ResultNoCoo.soft_max_sun_ang_dist);
    fprintf(' ]\n');
end


function debug_processRequestPowerLimits()
    RAD = pi / 180;
    Coo = [[220 * RAD, 0 * RAD]; [42 * RAD, -66 * RAD]];

    item = struct( ...
        'action', 'power_limits', ...
        'times', {{'2031-03-20T05:03:02Z'}}, ...
        'dod', 0, ...
        'coo', Coo);

    fprintf('\n=== processRequest power_limits ===\n');
    out = ultrasat.services.slew_calc.processRequest(item);
    fprintf('Status  : %s\n', out.status);
    fprintf('Message : %s\n', out.message);
    if isfield(out, 'soft_max_sun_ang_dist')
        fprintf('soft_max_sun_ang_dist: ');
        fprintf('%.3f ', out.soft_max_sun_ang_dist);
        fprintf('\n');
    end
    if isfield(out, 'is_hard')
        fprintf('is_hard: %s\n', mat2str(out.is_hard));
    end
    if isfield(out, 'hard_dur_min')
        fprintf('hard_dur_min: ');
        fprintf('%.3f ', out.hard_dur_min);
        fprintf('\n');
    end

    if ~strcmp(out.status, 'ok')
        error('processRequest power_limits returned non-ok status: %s', out.status);
    end
    fprintf('Test PASSED.\n');
end
