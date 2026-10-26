function D = funDefaults(FunName)
    % The default expressions of the name-value arguments of a function, read from its arguments block
    %   Used to record, next to a configuration, the defaults of the
    %   arguments it does not set (e.g. of ultrasat.usim), without copying
    %   them: a later change of a default then shows up as a difference
    %   between two saved records. The values are the source expressions
    %   as text (e.g. '[2 2 10]', 'sqrt(6)'), not evaluated.
    %   Needs metafunction (MATLAB R2024a or later); on an older release D
    %   is an empty structure, with a warning.
    % Input  : - FunName: the function name, e.g. 'ultrasat.usim'.
    % Output : - D: a structure, one field per name-value argument (the
    %               part after 'Args.'), holding its default expression as
    %               a char; '' for an argument without a default.
    % Author : D. Kovaleva (Oct 2026)
    % Example: D = ultrasat.simconfig.funDefaults('ultrasat.usim');
    %          D.JitterPar      % '[2 2 10]'

    arguments
        FunName (1,:) char
    end

    D = struct();
    if exist('metafunction', 'builtin') == 0 && exist('metafunction', 'file') == 0
        warning('simconfig:funDefaults:NoMetafunction', ...
                'metafunction is not available (MATLAB R2024a or later): the defaults of %s are not recorded', FunName);
    else
        In = metafunction(FunName).Signature.Inputs;
        for I = 1:numel(In)
            if In(I).NameValue
                Name = char(In(I).Identifier.Name);
                Expr = '';
                if ~isempty(In(I).DefaultValue)
                    Expr = char(In(I).DefaultValue.Expression);
                end
                D.(Name) = Expr;
            end
        end
    end
end
