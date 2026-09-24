function Result = normalization(Lambda, ParamMatrix, Args)
    % Constant normalization (scaling) factor for transmission curves
    % Input  : - Lambda (double array): Wavelength array in Angstrom.
    %            If GetArgNames flag is true, returns ArgNames structure for parameters.
    %          - ParamMatrix (double matrix): Parameter matrix where each row is [Norm].
    %            Accepts column vector [NumParamSets x 1] or scalar.
    %          * ...,key,val,...
    %            'Return' - Pre-computed results for caching. Default is [].
    %            'GetArgNames' - Return ArgNames structure instead of calculating.
    %                            Default is false.
    % Output : - Result (double matrix): Normalization matrix
    %            [NumWavelengths x NumParamSets], each column equal to the
    %            corresponding Norm value broadcast across all wavelengths.
    %            OR ArgNames structure if GetArgNames is true.
    % Author : D. Kovaleva (Apr 2026)
    % Example:
    %   % Basic usage:
    %   Lambda = linspace(3000, 11000, 401)';
    %   ParamMatrix = [0.5; 0.7; 1.0];  % Three parameter sets
    %   Result = astro.transmission.normalization(Lambda, ParamMatrix);
    %
    %   % Get parameter information:
    %   ArgNames = astro.transmission.normalization('GetArgNames', true);
    %
    %   % Usage with CompositeFun:
    %   Model = tools.math.fun.CompositeFun();
    %   Model.addFun('Normalization', @astro.transmission.normalization, [], ...
    %                'Par', 0.5, 'FitPar', true);

    arguments
        Lambda      = linspace(3000, 11000, 401)'
        ParamMatrix = 0.5                          % [Norm]
        Args.Return = []
        Args.GetArgNames logical = false
    end

    % Return ArgNames structure if requested
    if Args.GetArgNames
        Result = struct('Name', {1}, ...
                       'Description', {'Norm'}, ...
                       'Min', {0.001}, ...
                       'Max', {1.0});
        return;
    end

    % Check for pre-computed results (external cache)
    if ~isempty(Args.Return)
        Result = Args.Return;
        return;
    end

    Norm = ParamMatrix(:);                                % column vector
    Result = ones(numel(Lambda), 1) * Norm.';             % [Nwave x NumParamSets]
end
