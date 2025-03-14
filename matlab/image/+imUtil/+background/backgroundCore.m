function [Back, Var] = backgroundCore(Image, Args)
    % Estimate background and variance in a single matrix.
    %     This is the core function that is used for background and
    %     variance estimations.
    % Input  : - Image.
    %          * ...,key,val,... 
    %            'Back' - Background method. This is either a function
    %                   handle or string (e.g., 'median', 'rvar').
    %                   See imUtil.background.backgroundOption to see full
    %                   list of string options.
    %                   Default is @imUtil.background.modeVar_LogHist
    %            'BackArgs' - A cell array of additional arguments to pass to the
    %                   background function or string method.
    %                   For example, for 'quantile', this is the quantile
    %                   to choose.
    %                   Default is {}.
    %            'Var' - Like 'Back', but for the variance estimation.
    %                   If the same function handle is given for both
    %                   'Back' and 'Var', then assume that the function is
    %                   of the form: [Back, Var]=fun(Vec, BackArgs{:}).
    %                   In this case the VarArgs is ignored.
    %                   Default is @imUtil.background.modeVar_LogHist
    %            'VarArgs' - Like 'BackArgs', but for the variance
    %                   function arguments.
    %                   Default is {}.
    %            'Mask' - An optional indices or logical mask indicating
    %                   which pixels to use in the background/variance estimation.
    %                   If empty, then use all pixels.
    %                   Default is [].
    %            'Dilute' - Dilute the matrix by choosing each N element.
    %                   If 1, then no dilution. Default is 1.
    %            'Min' - Remove pixels below this vale.
    %                   If empty, do not remove. Default is [].
    %            'Max' - Remove pixels above this vale.
    %                   If empty, do not remove. Default is [].
    % Output : - Backgound estimator.
    %          - Variance estimator.
    % Author : Eran Ofek (2025 Mar) 
    % Example: [Back,Var]=imUtil.background.backgroundCore(randn(1000,1000).*10+100)
    %          [Back,Var]=imUtil.background.backgroundCore(randn(1000,1000).*10+100, 'Back', 'median', 'Var','rvar');

    arguments
        Image
        Args.Back              = @imUtil.background.modeVar_LogHist;
        Args.BackArgs          = {};
        Args.Var               = @imUtil.background.modeVar_LogHist;
        Args.VarArgs           = {};
        Args.Mask              = [];   % pixels to use
        Args.Dilute            = 1;
        Args.Min               = [];
        Args.Max               = [];
    end

    % Apply mask - remove bad pixels
    if ~isempty(Args.Mask)
        Vec = Image(Args.Mask);
    else
        Vec = Image(:);
    end
        
    % Dilute vector
    if Args.Dilute~=1
        Vec = Vec(1:Args.Dilute:end);
    end
    
    % remove values below Min
    if ~isempty(Args.Min)
        Vec = Vec(Vec>Args.Min);
    end
    % remove values above Max
    if ~isempty(Args.Max)
        Vec = Vec(Vec<Args.Max);
    end
    
        
    if isa(Args.Back, 'function_handle') && isa(Args.Var, 'function_handle')
        if strcmp(func2str(Args.Back),func2str(Args.Var))
            % BackMethod and VaeMethod are the same function
            % In this case assume function of the form: [Back,Var] = fun(Vec, args{:})
            % In this case use only BackArgs and ignore VarArgs
            
            [Back, Var] = Args.Back(Vec, Args.BackArgs{:});
        else
            % use different functions to estimate Back and Var
            Back = Args.Back(Vec, Args.BackArgs{:});
            Var  = Args.Back(Vec, Args.VarArgs{:});
        end
    else
        % assume that both Args.Back and Args.Var are strings
        
        Back = imUtil.background.backgroundOption(Vec, Args.Back, Args.BackArgs);
        Var  = imUtil.background.backgroundOption(Vec, Args.Var, Args.VarArgs);
    end
    
end
