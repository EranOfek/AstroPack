function [Back, Var] = backVarScalar(Image, Args)
    % Calculate the background and variance (scalars) of an image.
    %   See also: imUtil.background.backVar
    % Input  : - A 2D image.
    %          * ...,key,val,...
    %            'Method' - A method for Back & Var calculation.
    %                   This can be a cell array of two elements
    %                   for the Back and Var methods or a single element
    %                   for a method that calculates them together (e.g.,
    %                   modeVar_Hist).
    %                   The methods may be function handles, or string of
    %                   pre defined methods.
    %                   For predefined method see:
    %                   imUtil.background.backgroundOption
    %                   Examples:
    %                   @imUtil.background.modeVar_LogHist | @imUtil.background.modeVar_Hist - will use a single function to
    %                           calculate both Back and Var.
    %                   {@median, 'poiss'} | {@median, 'rvar_mex'} | ...
    %                   Default is @imUtil.background.modeVar_Hist
    %            'MethodArgs' - A cell array of additional arguments to pass to:
    %                   imUtil.background.backgroundOption
    %                   Default is {}.
    %            'RN2' - Readout noise ^2 (required by 'poiss').
    %                   Note thta modeVar_LogHist has its own RN2 argument.
    %                   Default is 12.
    %            'Dilute' - Dilute the array by this factor. If empty, do
    %                   not dilute. Note some functions (e.g., @modeVar_LogHist)
    %                   has internal dilution arguments. Default is {}.
    % Output : - Background scalar.
    %          - Variance scalar.
    % Author : Eran Ofek (2025 Oct) 
    % Example: R=poissrnd(ones(1726,1726).*100);                                            
    %          [B,V]=imUtil.background.backVarScalar(R);
    %          [B,V]=imUtil.background.backVarScalar(R, 'Method',@imUtil.background.modeVar_LogHist, 'MethodArgs',{{'MinVal',50,'MaxVal',5000}});
    %          [B,V]=imUtil.background.backVarScalar(R, 'Method', {@median, 'rvar_mex'});
    %          [B,V]=imUtil.background.backVarScalar(R, 'Method', {'quantile', @var}, 'MethodArgs',{{0.4},{}});
    %          [B,V]=imUtil.background.backVarScalar(R, 'Method', {@median, 'poiss'}, 'RN2',3.^2); % poisson noise + RN^2


    arguments
        Image

        Args.Method     = @imUtil.background.modeVar_Hist; %{@median,@poiss}; %or @modeVar_Hist; or string of predefined...
        Args.MethodArgs = {{},{}};
        Args.RN2        = 12;  % RN^2 - required by 'poiss'.
        Args.Dilute     = [];   % be careful of double diluting
    end

    if iscell(Args.Method)
        Nmethod     = numel(Args.Method);
        if Nmethod==1
            Args.Method = Args.Method{1};
        end
    else
        Args.Method = Args.Method;
        Nmethod     = 1;
    end
    % When Nmethod=1, Method is a single element, otherwise cell array

    if Nmethod==1
        if isa(Args.Method, 'function_handle')
            % call function that returns two outputs
            if isempty(Args.Dilute)
                [Back, Var] = Args.Method(Image(:), Args.MethodArgs{1}{:});
            else
                % note that at this stage: tools.array.mex.diluteArray is
                % not fast enough
                [Back, Var] = Args.Method(Image(1:Args.Dilute:end), Args.MethodArgs{1}{:});
            end
        else
            % assuming pre-defined strings
            switch lower(Args.Method)
                case ''

                otherwise
                    error('Unknown Method option: %s', Args.Method);
            end
        end
    else % if Nmethod==1

        % Background calculation:
        IndM = 1; % Method index
        if isa(Args.Method{IndM}, 'function_handle')
            % call function that return single output
            if isempty(Args.Dilute)
                [Back] = Args.Method{IndM}(Image(:), Args.MethodArgs{IndM}{:});
            else
                % note that at this stage: tools.array.mex.diluteArray is
                % not fast enough
                [Back] = Args.Method{IndM}(Image(1:Args.Dilute:end), Args.MethodArgs{IndM}{:});
            end
        else
            % assuming pre-defined strings
            if isempty(Args.Dilute)
                Back = imUtil.background.backgroundOption(Image, Args.Method{IndM}, Args.MethodArgs{IndM}); % Note MethodArgs{IndM} is a cell
            else
                Back = imUtil.background.backgroundOption(Image(1:Args.Dilute:end), Args.Method{IndM}, Args.MethodArgs{IndM});
            end
        end

        % Variance calculation:
        IndM = 2; % Method index
        if isa(Args.Method{IndM}, 'function_handle')
            % call function that return single output
            if isempty(Args.Dilute)
                [Var] = Args.Method{IndM}(Image(:), Args.MethodArgs{IndM}{:});
            else
                % note that at this stage: tools.array.mex.diluteArray is
                % not fast enough
                [Var] = Args.Method{IndM}(Image(1:Args.Dilute:end), Args.MethodArgs{IndM}{:});
            end
        else
            % special treatment for 'poiss' option:
            if strcmpi(Args.Method{IndM}, 'poiss')
                Args.MethodArgs{IndM} = {Back, Args.RN2};
            end

            % assuming pre-defined strings
            if isempty(Args.Dilute)
                Var = imUtil.background.backgroundOption(Image, Args.Method{IndM}, Args.MethodArgs{IndM}); % Note MethodArgs{IndM} is a cell
            else
                Var = imUtil.background.backgroundOption(Image(1:Args.Dilute:end), Args.Method{IndM}, Args.MethodArgs{IndM});
            end
        end
    end % if Nmethod==1

end
