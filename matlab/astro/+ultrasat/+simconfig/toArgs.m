function C = toArgs(S, Extra)
    % The key,val cell of one stage of a simulation configuration, for the function of that stage
    %   The field names of S are the argument names of the receiving
    %   function, so C goes to it as is: Fun(C{:}). Extra is appended
    %   after S, so a key in Extra overrides the same key of S at the
    %   receiving end (the last value wins in an arguments block).
    % Input  : - S: a structure, one stage of a configuration (e.g. Cfg.Usim
    %               of ultrasat.dut.config).
    %          - Extra: a cell of key,val appended after S. Default is {}.
    % Output : - C: a 1 x 2N cell of key,val.
    % Author : D. Kovaleva (Oct 2026)
    % Example: Cfg = ultrasat.dut.config;
    %          C = ultrasat.simconfig.toArgs(Cfg.Usim, {'Seed', 7});
    %          % ultrasat.usim('Cat', Cat, C{:});

    arguments
        S (1,1) struct
        Extra cell = {}
    end

    if mod(numel(Extra), 2) ~= 0
        error('simconfig:toArgs:Extra', 'Extra must be a cell of key,val pairs');
    end
    C = [namedargs2cell(S), Extra(:)'];
end
