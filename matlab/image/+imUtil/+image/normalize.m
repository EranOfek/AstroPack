function [Image] = normalize(Image, Args)
    % Additive and multiplicative normalization of an image
    %   Normalize (by addition and multiplication) the image such that some statistical properties of the
    %   image (e.g., std) will be equal some value.
    % Input  : - An array.
    %          * ...,key,val,... 
    %            'AddFun' - Function handle of a ststistical property.
    %                   The array will be normalize, additively, such that
    %                   this statistical property will be equal to
    %                   'AddVal'.
    %                   If empty, then skip the additive normalization.
    %                   Default is @fast_median.
    %            'AddFunArgs' - A cell array of additional arguments to
    %                   pass to AddFun. Default is {}.
    %            'AddVal' - The addive normzlization will be force to equal
    %                   this scalar. Default is 0.
    %            'MultFun' - Like 'AddFun', but for the multiplicative
    %                   normalization.
    %                   Default is @tools.math.stat.std_mad.
    %            'MultFunArgs' - A cell array of additional arguments to
    %                   pass to MultFun. Default is {0, 'all'}.
    %            'MultVal' - The multiplicative normzlization will be force to equal
    %                   this scalar. Default is 1.
    %            'PreDef' - A char array indicating a pre defined method.
    %                   If not empty, then will override all the other
    %                   arguments.
    %                   The following options sets the values of:
    %                   AddFun, AddFunArgs, AddVal, MultFun, MultFunArgs, MultVal:
    %                   [] - Use the other input arguments.
    %                   'norm_robust' - @fast_median, {}, 0,   @tools.math.stat.std_mad, {0,'all'}, 1
    %                   'norm' - @mean, {}, 0,   @std, {0,'all'}, 1
    %                   'chi2_mean' - Normalize to the mean of \chi^2 with K degrees of freedoms.
    %                   'chi2_median' - Normalize to the median of \chi^2 with K degrees of freedoms.
    %                   'chi2_var'    - Normalize to the variance of \chi^2 with K degrees of freedoms.
    %            'K' - d.o.f. for the \chi^2 distribution in the PreDef
    %                   options, or the second input argument of Fun2Prob argument.
    %                   Default is 1.
    %            'Fun2Prob' - If not empty, then will convert the Image to
    %                   one-sided probability units using the function
    %                   handle supplied in this argument
    %                   (e.g., @chi2cdf)
    %                   For example, if you use PreDef=chi2_median, then
    %                   set it to @chi2cdf, in order to convert from \chi^2
    %                   distribution to probability.
    %                   Default is [].
    %            'Prob2Sig' - A logical indicating if to convert the image
    %                   in units of probability to Gaussian significance.
    %                   This will work only if Fun2prob is not empty.
    %                   Default is true.
    % Output : - A normalized array.
    % Author : Eran Ofek (2024 Jan) 
    % Example: %Set mean to zero and ribust std to 1:
    %          Im=imUtil.image.normalize(100 + 2.*randn(1000,1000));
    %          %Set mean to 2 (e.g., chi^2 dist. with dof=2)
    %          Im=imUtil.image.normalize(Im, 'AddFun',[], 'MultFun',@mean, 'MultFunArgs',{}, 'MultVal',2);

    arguments
        Image
        Args.AddFun             = @fast_median;
        Args.AddFunArgs cell    = {};
        Args.AddVal             = 0;
        
        Args.MultFun            = @tools.math.stat.std_mad;
        Args.MultFunArgs cell   = {0,'all'};
        Args.MultVal            = 1;
        
        Args.PreDef             = [];
        Args.K                  = 1;
        Args.Fun2Prob           = [];     % @chi2cdf
        Args.Prob2sig logical   = true;
    end
    
    if ~isempty(Args.PreDef)
        % overrid input argumnets
        switch Args.PreDef
            case 'norm_robust'
                Args.AddFun      = @fast_median;
                Args.AddFunArgs  = {};
                Args.AddVal      = 0;
                Args.MultFun     = @tools.math.stat.std_mad;
                Args.MultFunArgs = {0,'all'};
                Args.MultVal     = 1;
            case 'norm'
                Args.AddFun      = @mean;
                Args.AddFunArgs  = {};
                Args.AddVal      = 0;
                Args.MultFun     = @std;
                Args.MultFunArgs = {0,'all'};
                Args.MultVal     = 1;
            case 'chi2_mean'
                Args.AddFun      = [];
                Args.MultFun     = @mean;
                Args.MultFunArgs = {0,'all'};
                Args.MultVal     = Args.K;
            case 'chi2_median'
                Args.AddFun      = [];
                Args.MultFun     = @median;
                Args.MultFunArgs = {0,'all'};
                Args.MultVal     = Args.K.*(1 - (2./(9.*Args.K))).^3;
            case 'chi2_var'
                Args.AddFun      = [];
                Args.MultFun     = @var;
                Args.MultFunArgs = {0,'all'};
                Args.MultVal     = 2.*Args.K;
            otherwise
                error('Unknown PreDef option');
        end
    end
    
    if ~isempty(Args.AddFun)
        AddFunVal = Args.AddFun(Image(:), Args.AddFunArgs{:});
        Image     = Image - AddFunVal + Args.AddVal;
    end
    
    if ~isempty(Args.MultFun)
        MultFunVal = Args.MultFun(Image(:), Args.MultFunArgs{:});
        Image      = Args.MultVal .* Image./MultFunVal;
    end
    
    if ~isempty(Args.Fun2Prob)
        % assume Image is now like \chi^2 distribution
        % Convert from \chi^2 distribution to probability
    
        Image = Args.Fun2Prob(Image, Args.K);
        % one sided
        Image = 1 - (1-Image).*2;
        if Args.Prob2Sig
            % convert to Gaussian significance
            Image = norminv(Image, 0, 1);
        end
    end
end
