function [Result] = xcorrLineWidth(Data, Template, FilterWidth, Filter, Args)
    % Correlation coef. between data and template when the template is convolved with a filter
    %       as a function of the filter width.
    %       For each filter width the correlation betweem the data and
    %       template convolved with the filter is returned.
    % Input  : - A column vector with data.
    %          - A colume vector with template.
    %          - Vector of filter width to test.
    %            Default is (0.1:0.1:2)'.
    %          - Filter function. Default is @(X, Sigma) normpdf(X,0,Sigma);
    %          * ...,key,val,... 
    %            'X' - The locations in which to evaluate the filter.
    %                   Default is (-10:1:10).
    % Output : - A structure containing:
    %            .Corr - corr coef. between the orginal data and template
    %                   vectors.
    %            .Width - Vector of filter width tested.
    %            .CorrWidth - Corr coef. for each filter width.
    % Author : Eran Ofek (2023 Dec) 
    % Example: S=AstroSpec.specGalQSO('Gal_E');
    %          Template = [S.Wave, S.Flux];
    %          X = (-10:1:10)';
    %          Data = Template;
    %          Data(:,2)  = conv(Template(:,2), normpdf(X,0,0.5),'same');
    %          Result = imUtil.spec.lines.xcorrLineWidth(Data(:,2), Template(:,2));
    %          plot(Result.Width, Result.CorrWidth)

    arguments
        Data
        Template
        FilterWidth            = (0.1:0.1:2)';
        Filter                 = @(X, Sigma) normpdf(X,0,Sigma);
        
        Args.X                 = (-10:1:10);
    end

    Nw = numel(FilterWidth);
    C = corrcoef(Data, Template);
    Result.Corr      = C(1,2);
    Result.Width     = FilterWidth;
    Result.CorrWidth = zeros(size(FilterWidth));
    
    for Iw=1:1:Nw
        TemplateConv = conv(Template, Filter(Args.X, FilterWidth(Iw)), 'same');
        C = corrcoef(Data, TemplateConv);
        Result.CorrWidth(Iw) = C(1,2);
        
    end
    
end
