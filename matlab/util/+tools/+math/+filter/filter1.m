function [S, DataStd]=filter1(Data, Template, Dim, Args)
    % Apply a 1D filter to data and return statistics in std units.
    % Input  : - A 1D Data, along the Dim dimension (same at the templates
    %            Dimension).
    %          - A vector of a template, or a matrix of template banks.
    %            The template ust be along the same dimension as the data.
    %            The templates will be padded, and corerized.
    %          - Dimension along which the data and templates are provided.
    %            Default is 1.
    %          * ...,key,val,...
    %            'Back' - Back value to subtract from the data.
    %                   If empty, then do not subtract background.
    %                   Default is [].
    %            'Std' - An std of the data scalar, vector, or matrix.
    %                   Alternatively, this can be a function handle that
    %                   will be used to calculate the std.
    %                   The second argument of the function must be dim.
    %                   Default is @tools.math.stat.rstd
    %            'PadVal' - Padding value. Default is 0.
    %            'IsTemplateFFT' - Is template is FFTed and complex
    %                   conjugate of template. Default is false.
    % Output : - The filtered statistics in units of std.
    %          - The Data std.
    % Author : Eran Ofek (Feb 2022)
    % Example: Data = randn(20,1).*0.1; T = [1 2 3 2 1].';
    %          Data(1:5) = T + randn(5,1).*0.1;
    %          [S, DS] = tools.math.filter.filter1(Data, T);
    
    arguments
        Data
        Template
        Dim                         = 1;
        Args.Back                   = [];
        Args.Std                    = @tools.math.stat.rstd; % additional argument must be Dim
        Args.PadVal                 = 0;
        Args.IsTemplateFFT logical  = false; % template is corenerized, ffted, complex-conj and with correct size an
    end
    
    Length   = size(Data, Dim);
    
    if Args.IsTemplateFFT
        TemplateFFT = Args.Template;
    else
        Template    = tools.math.filter.cornerize1(Template, Length, Dim, true, Args.PadVal);
        TemplateFFT = conj(fft(Template, [], Dim));
    end
    
    % subtracted background from Data
    if ~isempty(Args.Back)
        Data = Data - Args.Back;
    end
    
    if isnumeric(Args.Std)
        DataStd = Args.Std;
    else
        % assume Std is a function handle
        DataStd = Args.Std(Data, Dim);
    end
    Norm = sqrt(sum(Template.^2, Dim));
    
    S = ifft(fft(Data, [], Dim) .* TemplateFFT, [], Dim)./(DataStd.*Norm);
    
end