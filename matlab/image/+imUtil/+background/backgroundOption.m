function [Back] = backgroundOption(Vec, Method, MethodArgs)
    % Estimate background/variance of an array using pre-defined functions.
    % Input  : - A vector.
    %          - Method:
    %            'median'
    %            'mean'
    %            'rmean'
    %            'std'
    %            'rstd'
    %            'var'
    %            'rvar'
    %            'quantile' - Default quantile is 0.25.
    %          - Method extra arguments
    %            Default is [].
    % Output : - Requested property of vector
    % Author : Eran Ofek (2025 Mar) 
    % Example: R=imUtil.background.backgroundOption(randn(1000,1),'median')

    arguments
        Vec
        Method
        MethodArgs    = [];
    end
    
    Vec = Vec(:);

    switch Method
        case 'median'
            Back = median(Vec, 1, 'omitnan');
        case 'mean'
            Back = mean(Vec, 1, 'omitnan');
        case 'rmean'
            Back = median(Vec, 1, 'omitnan');
            if isempty(MethodArgs)
                Back = tools.math.stat.rmean(Vec, 1);
            else
                Back = tools.math.stat.rmean(Vec, 1, MethodArgs{:});
            end
        case 'std'
            Back = std(Vec, [], 1, 'omitnan');
        case 'var'
            Back = var(Vec, [], 1, 'omitnan');
        case 'rstd'
            if isempty(MethodArgs)
                Back = tools.math.stat.rstd(Vec,1,1);
            else
                Back = tools.math.stat.rstd(Vec,1,MethodArgs{:});
            end
        case 'rvar'
            if isempty(MethodArgs)
                Back = tools.math.stat.rstd(Vec,1,1).^2;
            else
                Back = tools.math.stat.rstd(Vec,1,MethodArgs{:}).^2;
            end    
        case 'quantile'
            if isempty(MethodArgs)
                Back = quantile(Vec, 0.25);
            else
                Back = quantile(Vec, MethodArgs{:});
            end
        otherwise
            error('Unknown Back option');
    end


end
