%==========================================================================
% Project     : ULTRASAT SOC
% Filename    : +ultrasat/+services/+slew_calc/processRequest.m
% Author      : Chen Tishler
% Created     : 02/11/2025
% Modified    : 10/05/2026
% Description : MATLAB service to calculate slew time between targets (RA/Dec per calcSlew; roll accepted but ignored)
%==========================================================================

function Output = processRequest(Input)
    % Process request: dispatch on action (flat JSON, no inner json_text).
    %
    % Input  : item struct with .action ('health'|'slew'|'slew_batch'|'power_limits') and action-specific fields.
    %            Attitude objects may include .roll (Python Attitude model); slew time is RA/Dec-only — roll is not used.
    % Output : ApiBaseResponse-style: .status ('ok'|'error'), .message; for 'slew' also .slew, .direct;
    %            for 'slew_batch' .results; for 'power_limits' also .soft_max_sun_ang_dist, .is_hard, .hard_dur_min
    %
    % Author : Chen Tishler (2021), refactored for flat API (2026)

    Output = struct;
    Output.status = '?';    
    Output.message = 'MATLAB: Exception in processRequest';

    try
        if strcmp(Input.action, 'health')
            Output.status  = 'ok';            
            Output.message = 'health: OK';
        elseif strcmp(Input.action, 'slew')
            Output = processSlew(Input);
        elseif strcmp(Input.action, 'slew_batch')
            Output = processSlewBatch(Input);
        elseif strcmp(Input.action, 'power_limits')
            Output = processPowerLimits(Input);
        else
            Output.status  = 'error';            
            Output.message = 'unknown action';
        end
    catch Ex
        Output.status  = 'error';            
        Output.message = sprintf('exception: %s', Ex.message);
    end
end

% ===========================================================================

function Output = processSlew(Input)
    % Process slew request: calculate slew time between two points
    % Input:   Input - input struct with .from.ra, .from.dec, .to.ra, .to.dec, optional .from/.to.roll, .time
    % Output:  Output - output struct with .status, .message, .slew, .direct
    % Example: Output = processSlew(Input);

    try

        % Get input parameters
        ra1 = Input.from.ra;
        dec1 = Input.from.dec;
        ra2 = Input.to.ra;
        dec2 = Input.to.dec;
        timeIso = '';
        if isfield(Input, 'time') && ~isempty(Input.time)
            timeIso = Input.time;
        end

        logRollIgnoredIfSignificant(Input.from, Input.to, 'slew');

        % Calculate
        res = ultrasat.services.slew_calc.calcSlewWrapper(ra1, dec1, ra2, dec2, timeIso);

        % Prepare output struct
        Output.status  = 'ok';
        Output.message = '';
        Output.slew    = res.slew;
        Output.direct  = res.direct;
    catch ex
        Output.status  = 'error';        
        Output.message = sprintf('MATLAB: processSlew exception: %s', ex.message);
        io.msgLog(LogLevel.Error, Output.message);
    end
end

% ===========================================================================

function Output = processSlewBatch(Input)
    % Process slew batch request: calculate slew time between multiple pairs of points
    % Input:   Input - input struct with .pairs, .time
    % Output:  Output - output struct with .status, .message, .results
    % Example: Output = processSlewBatch(Input);

    try
        if ~isfield(Input, 'pairs')
            Output.status = 'error';
            Output.message = 'processSlewBatch: Missing field "pairs"';
            Output.results = [];
            return;
        end
        pairs = Input.pairs;
        timeIso = '';
        if isfield(Input, 'time') && ~isempty(Input.time)
            timeIso = Input.time;
        end
        N = numel(pairs);
        results = repmat(struct('slew', [], 'direct', []), 1, N);

        % Calculate pair at a time
        for i = 1:N
            p = pairs(i);
            ra1 = p.from.ra;
            dec1 = p.from.dec;
            ra2 = p.to.ra;
            dec2 = p.to.dec;

            logRollIgnoredIfSignificant(p.from, p.to, sprintf('slew_batch pair %d', i));

            % Calculate
            res = ultrasat.services.slew_calc.calcSlewWrapper(ra1, dec1, ra2, dec2, timeIso);
            results(i).slew   = res.slew;
            results(i).direct = res.direct;
        end

        % Prepare output struct
        Output.status   = 'ok';        
        Output.message  = sprintf('MATLAB: processSlewBatch completed (%d items)', N);
        Output.results  = results;
    catch ex
        Output.message = sprintf('MATLAB: processSlewBatch exception: %s', ex.message);
        Output.status  = 'error';
        io.msgLog(LogLevel.Error, Output.message);
    end
end

% ===========================================================================

function Output = processPowerLimits(Input)
    % Process power_limits request: calculate solar-panel soft/hard limits.
    % Input:   Input - struct with .times, optional .dod, .coo, .max_ang, .min_ang,
    %                  .base_dur_min, .max_dod
    % Output:  Output - struct with .status, .message, .soft_max_sun_ang_dist,
    %                    .is_hard, .hard_dur_min

    try
        if ~isfield(Input, 'times') || isempty(Input.times)
            Output.status = 'error';
            Output.message = 'processPowerLimits: Missing field "times"';
            return;
        end

        res = ultrasat.services.slew_calc.powerLimitsWrapper(Input);

        Output.status = 'ok';
        Output.message = '';
        Output.soft_max_sun_ang_dist = res.soft_max_sun_ang_dist;
        Output.is_hard = res.is_hard;
        Output.hard_dur_min = res.hard_dur_min;
    catch ex
        Output.status = 'error';
        Output.message = sprintf('MATLAB: processPowerLimits exception: %s', ex.message);
        io.msgLog(LogLevel.Error, Output.message);
    end
end

% ===========================================================================

function r = attitudeRollDeg(S)
    % Default roll 0 when field absent (JSON attitude may omit roll)
    if isstruct(S) && isfield(S, 'roll') && ~isempty(S.roll)
        r = double(S.roll);
    else
        r = 0;
    end
end


function logRollIgnoredIfSignificant(FromS, ToS, ContextTag)
    % Log once at Debug when roll delta is non-zero; slew time remains RA/Dec-only.
    arguments
        FromS
        ToS
        ContextTag (1, :) char = ''
    end
    dRoll = attitudeRollDeg(ToS) - attitudeRollDeg(FromS);
    if abs(dRoll) > 1e-6
        io.msgLog(LogLevel.Debug, ...
            'slew_calc: ignoring roll delta (%.6g deg) for %s; result is RA/Dec slew only', ...
            dRoll, ContextTag);
    end
end

