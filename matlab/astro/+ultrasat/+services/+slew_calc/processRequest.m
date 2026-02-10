%==========================================================================
% Project     : ULTRASAT SOC
% Filename    : ultrasat/+services/+slew_calc/processRequest.m
% Author      : Chen Tishler
% Created     : 02/11/2025
% Modified    : 10/02/2026
% Description : MATLAB service to calculate slew time between targets
%==========================================================================

function Output = processRequest(Input)
% Process request: dispatch on action (flat JSON, no inner json_text).
%
% Input  : item struct with .action ('health'|'slew'|'slew_batch') and action-specific fields
% Output : flat struct: .message, .result; for slew also .slew, .direct; for batch .results
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
    try
        if ~isfield(Input, 'pairs')
            Output.message = 'processSlewBatch: Missing field "pairs"';
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

