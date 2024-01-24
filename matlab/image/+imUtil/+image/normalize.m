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
    end
    
    if ~isempty(Args.AddFun)
        AddFunVal = Args.AddFun(Image(:), Args.AddFunArgs{:});
        Image     = Image - AddFunVal + Args.AddVal;
    end
    
    if ~isempty(Args.MultFun)
        MultFunVal = Args.MultFun(Image(:), Args.MultFunArgs{:});
        Image      = Args.MultVal .* Image./MultFunVal;
    end
    

end
