function Result = processRequest(item)
% Process request: dispatch on action (flat JSON, no inner json_text).
%
% Input  : item struct with .action ('health'|'slew'|'slew_batch') and action-specific fields
% Output : flat struct: .message, .result; for slew also .slew, .direct; for batch .results
%
% Author : Chen Tishler (2021), refactored for flat API (2026)

    out = struct;
    out.message = 'MATLAB: Exception in processRequest';
    out.result  = -1;

    try
        action = '';
        if isfield(item, 'action')
            action = item.action;
        end
        if isempty(action) && isfield(item, 'op')
            action = item.op;
        end

        if strcmp(action, 'health')
            out.message = 'ok';
            out.result  = 0;
        elseif strcmp(action, 'slew')
            out = ultrasat.services.slew_calc_service.processSlew(item);
        elseif strcmp(action, 'slew_batch')
            out = ultrasat.services.slew_calc_service.processSlewBatch(item);
        else
            out.message = 'MATLAB: unknown action';
            out.result  = -1;
        end
    catch Ex
        out.message = sprintf('MATLAB: exception: %s', Ex.message);
        out.result  = -1;
    end

    Result = out;
end
