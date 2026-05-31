%==========================================================================
% Project     : ULTRASAT SOC
% Filename    : ultrasat/+services/+slew_calc/powerLimitsWrapper.m
% Author      : Chen Tishler
% Created     : 31/05/2026
% Modified    : 31/05/2026
% Description : Wrapper for PowerLimits to calculate solar-panel power limits
%==========================================================================

function Result = powerLimitsWrapper(Input)
    % Wrapper for ultrasat.PowerLimits; returns flat struct for JSON IPC.
    %
    % Input  : struct with .times (ISO strings), optional .dod, .coo, .max_ang,
    %          .min_ang, .base_dur_min, .max_dod
    % Output : struct with .soft_max_sun_ang_dist, .is_hard, .hard_dur_min
    %
    % Example:
    %   Input = struct('times', {{'2031-03-20T05:03:02Z'}}, 'dod', 0, ...
    %       'coo', [220/180*pi, 0; 42/180*pi, -66/180*pi]);
    %   Result = powerLimitsWrapper(Input);

    t = parseTimesIso(Input.times);

    dod = getFieldOrDefault(Input, 'dod', 0);
    maxAng = getFieldOrDefault(Input, 'max_ang', 71 + 66);
    minAng = getFieldOrDefault(Input, 'min_ang', 71 + 60);
    baseDurMin = getFieldOrDefault(Input, 'base_dur_min', 180);
    maxDod = getFieldOrDefault(Input, 'max_dod', 0.8);

    plArgs = struct( ...
        'DOD', double(dod), ...
        'maxAng', double(maxAng), ...
        'minAng', double(minAng), ...
        'base_dur_min', double(baseDurMin), ...
        'maxDOD', double(maxDod));

    Coo = parseCoo(Input);
    if ~isempty(Coo)
        [SoftMaxSunAngDist, isHard, Hard_dur_min] = ultrasat.PowerLimits(t, ...
            'DOD', plArgs.DOD, ...
            'Coo', Coo, ...
            'maxAng', plArgs.maxAng, ...
            'minAng', plArgs.minAng, ...
            'base_dur_min', plArgs.base_dur_min, ...
            'maxDOD', plArgs.maxDOD);
    else
        [SoftMaxSunAngDist, isHard, Hard_dur_min] = ultrasat.PowerLimits(t, ...
            'DOD', plArgs.DOD, ...
            'maxAng', plArgs.maxAng, ...
            'minAng', plArgs.minAng, ...
            'base_dur_min', plArgs.base_dur_min, ...
            'maxDOD', plArgs.maxDOD);
    end

    Result = struct();
    Result.soft_max_sun_ang_dist = double(SoftMaxSunAngDist(:))';

    if isempty(Coo)
        Result.is_hard = [];
        Result.hard_dur_min = [];
    else
        Result.is_hard = logical(isHard(:))';
        hardDur = double(Hard_dur_min(:))';
        hardDur(isinf(hardDur)) = nan;
        Result.hard_dur_min = hardDur;
    end

    io.msgLog(LogLevel.Info, sprintf( ...
        'PowerLimits(%d times, coo=%s) -> soft_max=[%s]', ...
        numel(t), mat2str(~isempty(Coo)), ...
        strjoin(arrayfun(@(x) sprintf('%.3f', x), Result.soft_max_sun_ang_dist, 'UniformOutput', false), ', ')));
end

% ===========================================================================

function t = parseTimesIso(TimesIso)
    if ischar(TimesIso) || isstring(TimesIso)
        TimesIso = {char(TimesIso)};
    elseif isnumeric(TimesIso) || isdatetime(TimesIso)
        t = TimesIso;
        if ~isdatetime(t)
            error('powerLimitsWrapper: invalid times type');
        end
        return;
    end

    if iscell(TimesIso)
        n = numel(TimesIso);
        t = NaT(1, n);
        for i = 1:n
            dt = ultrasat.api.utils.DateTimeUtils.parseIsoDateTime(TimesIso{i});
            if isnat(dt)
                error('powerLimitsWrapper: invalid ISO time at index %d', i);
            end
            t(i) = dt;
        end
        return;
    end

    error('powerLimitsWrapper: times must be ISO strings, cell array, or datetime');
end

% ===========================================================================

function Coo = parseCoo(Input)
    Coo = [];
    if ~isfield(Input, 'coo') || isempty(Input.coo)
        return;
    end
    raw = Input.coo;
    if iscell(raw)
        n = numel(raw);
        Coo = zeros(n, 2);
        for i = 1:n
            row = raw{i};
            if numel(row) < 2
                error('powerLimitsWrapper: coo row %d must have [ra_rad, dec_rad]', i);
            end
            Coo(i, :) = double(row(1:2));
        end
    else
        Coo = double(raw);
        if size(Coo, 2) ~= 2
            error('powerLimitsWrapper: coo must be Nx2 [ra_rad, dec_rad]');
        end
    end
end

% ===========================================================================

function val = getFieldOrDefault(S, fieldName, defaultVal)
    if isstruct(S) && isfield(S, fieldName) && ~isempty(S.(fieldName))
        val = S.(fieldName);
    else
        val = defaultVal;
    end
end
