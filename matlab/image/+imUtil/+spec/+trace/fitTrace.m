function [Result] = fitTrace(X, Y, Args)
    % Fit a trace position as a function of wavelength position.
    %       The function can work on multiple traces simoltanosly.
    %       Given the X (wavelength) and Y (spatial position),
    %       the fit is done in 3 (optional) steps:
    %       1. Median filter Y.
    %       2. SGolay filter the previous step Y.
    %       3. Fit a polynomial to the previous step Y.
    % Input  : - An X vector. If empty, then will be constructed in the
    %            range of 1 to the last pixel.
    %          - A vector or matrix of Y (spatial positions).
    %            By default (DimWave=1), each column correspond to a trace.
    %          * ...,key,val,... 
    %            'DimWave' - Dimension of the wavelength direction.
    %                   Default is 1.
    %            'StdFilterHalfSize' - The half size of the std filter.
    %                   Default is 20.
    %            'MaxStdFilter' - Pixels with StdFilter value above this
    %                   threshold will not participate in the polynomial
    %                   fitting.
    %                   Default is 0.3.
    %            'MedFilt' - Apply median filtering. Default is true.
    %            'MedFiltOrder' - Order of median filtering (see medfilt1).
    %                   Default is 21.
    %            'SGolay' - Apply SGolay filter. Default is true.
    %            'SGolayOrder' - SGolay filter order. Default is 2.
    %            'SGolayLen' - Length (window size) of the SGolay filter.
    %                   Default is 101.
    %            'PolyFit' - Apply a polynomial fitting. Default is true.
    %            'FitOrder' - Vector of polynomials order to fit.
    %                   Default is [0 1 2 3 4 5].
    % Output : - A structure with the following fields:
    %            .FitY - The best fit Y values.
    %            .StdY - Std filter on Y values (in columns).
    %            .ResidY - Y-FitY residuals (in columns).
    % Author : Eran Ofek (2023 Dec) 
    % Example: R=imUtil.spec.trace.fitTrace([],Y)

    arguments
        X
        Y
        Args.DimWave           = 1;
        Args.StdFilterHalfSize = 20;
        Args.MaxStdFilter      = 0.3;   % StdY above this value will not be participating in the trace fitting
        
        % mefilt
        Args.MedFilt logical   = true;
        Args.MedFiltOrder      = 21;
        
        % spgolay
        Args.SGolay logical    = true;
        Args.SGolayOrder       = 2;
        Args.SGolayLen         = 101;
        
        % polyfit
        Args.PolyFit logical   = true;
        Args.FitOrder          = [0 1 2 3 4 5];
    end
    
    if Args.DimWave==2
        Y = Y.';
    end
    
    [Npix, Ntrace] = size(Y);
    if isempty(X)
        X = (1:1:Npix).';
    end
    X = X(:);
    
    % fit X vs Y, for each column of Y:
    FitY = Y;
    
    % std filter 
    StdY = timeSeries.filter.filterStd(Y, Args.StdFilterHalfSize, 'Dim',1);
    FlagGoodPix = StdY<Args.MaxStdFilter;
    
    if Args.MedFilt
        FitY = medfilt1(FitY, Args.MedFiltOrder, [], 1);
    end
    
    if Args.SGolay
        FitY = sgolayfilt(FitY, Args.SGolayOrder, Args.SGolayLen, [], 1);
    end
    
    if Args.PolyFit
        MidX = 0.5.*(X(1) + X(end));
        HalfRange = (X(end) - X(2)).*0.5;
        Xm = (X-MidX)./HalfRange;
        Hm  = Xm.^Args.FitOrder;
        % since FlagGoodPix is not the same for each column - do it loop
        for Itrace=1:1:Ntrace
            Par = Hm(FlagGoodPix(:,Itrace),:)\Y(FlagGoodPix(:,Itrace),Itrace);
            FitY(:,Itrace) = Hm*Par;
        end
    end
    
    Result.FitY   = FitY;
    Result.StdY   = StdY;
    Result.ResidY = Y - FitY;
end
