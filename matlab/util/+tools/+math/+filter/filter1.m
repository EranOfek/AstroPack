function [S, DataStd]=filter1(Data, Template, Dim, Args)
    % Apply a 1D filter to data and return statistics in std units.
    % Input  : - A vector or matrix with 1D Data, along the Dim dimension (same at the templates
    %            Dimension).
    %            Either the data or template are matrices, but not both.
    %          - A vector or a matrix of template bank.
    %            If the first input is a matrix then this must be a vector.
    %            The template ust be along the same dimension as the data.
    %            The templates will be padded, and corerized.
    %          - Dimension along which the data and templates are provided.
    %            Default is 1.
    %          * ...,key,val,...
    %            'Back' - Back value to subtract from the data.
    %                   Alternatively, a function_handle for calculating
    %                   the background.
    %                   If empty, then do not subtract background.
    %                   Default is [].
    %            'BackArgs' - A cell array of additional arguments to pass
    %                   to the background calc. function. Default is {}.
    %            'Std' - An std of the data scalar, vector, or matrix.
    %                   Alternatively, this can be a function handle that
    %                   will be used to calculate the std.
    %                   The second argument of the function must be dim.
    %                   Default is @tools.math.stat.rstd
    %            'StdArgs' - A cell array of additional arguments to pass
    %                   to the background std calc. function after the data
    %                   and dim parameters. Default is {}.
    %            'PadVal' - Padding value. Default is 0.
    %            'IsTemplateFFT' - Is template is FFTed and complex
    %                   conjugate of template. Default is false.
    % Output : - The filtered statistics in units of std.
    %          - The Data std.
    % Author : Eran Ofek (Feb 2022)
    % Example: Data = randn(200,2).*0.1; T = [1 2 1 2 1].';
    %          Data(1:5,1) = T + randn(5,1).*0.1;
    %          [S, DS] = tools.math.filter.filter1(Data, T);
    
    arguments
        Data
        Template
        Dim                         = 1;
        Args.Back                   = [];
        Args.BackArgs cell          = {};
        Args.Std                    = @tools.math.stat.rstd; % additional argument must be Dim
        Args.StdArgs cell           = {};
        Args.PadVal                 = 0;
        Args.IsTemplateFFT logical  = false; % template is corenerized, ffted, complex-conj and with correct size an
    end
    
    Length   = size(Data, Dim);
    
    % make sure Template is normalized to unity
    Template = Template./sum(Template, Dim);
    
    if Args.IsTemplateFFT
        TemplateFFT = Args.Template;
    else
        Template    = tools.math.filter.cornerize1(Template, Length, Dim, true, Args.PadVal);
        TemplateFFT = conj(fft(Template, [], Dim));
    end
    
    % subtracted background from Data
    if ~isempty(Args.Back)
        if isa(Args.Back, 'function_handle')
            Args.Back = Args.Back(Data, Args.BackArgs{:});
        end
        Data = Data - Args.Back;
    end
    
    if isnumeric(Args.Std)
        DataStd = Args.Std;
    elseif isa(Args.Std, 'function_handle')
        % assume Std is a function handle
        DataStd = Args.Std(Data, Dim, Args.StdArgs{:});
    else
        error('Unknown Std option');
    end
    Norm = 1./sqrt(sum(Template.^2, Dim));
    
    S = ifft(fft(Data, [], Dim) .* TemplateFFT, [], Dim);
    %S = S./imUtil.background.rstd(S);   % empirical test
    S = Norm .* S./DataStd;
    
end