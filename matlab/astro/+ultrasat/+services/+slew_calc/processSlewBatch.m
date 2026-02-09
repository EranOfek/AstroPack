function Result = processSlewBatch(item)
% Process batch slew: flat input struct (pairs, optional time), flat output.
%
% Input  : item struct with .pairs (array of {from, to}), optional .time (ISO)
% Output : struct with .message, .result, .results (array of .slew, .direct)

    out = struct;
    out.message  = 'MATLAB: processSlewBatch started';
    out.result   = -1;
    out.results  = [];

    try
        if ~isfield(item, 'pairs')
            error('processSlewBatch:MissingField', 'Input must contain "pairs" array.');
        end
        pairs = item.pairs;
        timeIso = '';
        if isfield(item, 'time') && ~isempty(item.time)
            timeIso = item.time;
        end
        N = numel(pairs);
        results = repmat(struct('slew', [], 'direct', []), 1, N);
        for i = 1:N
            p = pairs(i);
            ra1 = p.from.ra;
            dec1 = p.from.dec;
            ra2 = p.to.ra;
            dec2 = p.to.dec;
            [res, ~] = ultrasat.services.slew_calc_service.doProcessSlew(ra1, dec1, ra2, dec2, timeIso);
            results(i).slew   = res.slew;
            results(i).direct = res.direct;
        end
        out.message  = sprintf('MATLAB: processSlewBatch completed (%d items)', N);
        out.result   = 0;
        out.results   = results;
    catch ex
        out.message = sprintf('MATLAB: processSlewBatch exception: %s', ex.message);
        out.result  = -1;
        io.msgLog(LogLevel.Error, out.message);
    end

    Result = out;
end
