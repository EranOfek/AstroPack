function [Result] = runMeanFilter(M, Args)
    % Apply a running mean top-hat filter to data and normalize results by std.
    %   Will also check that the significance of the filiter with the specified width 
    %   is larger than that of a filter with width=1.
    % Input  : - An array of measurmnets. Column per time series.
    %            The time series are assumed to be equally spaced, but they
    %            may contains NaNs.
    %          * ...,key,val,... 
    %            'Dim' - Dimension of the time axis. Default is 1.
    %            'PolyFit' - A vector of polynomial orders to fit and
    %                   subtract from data prior to filtering.
    %                   If empty, then skip this step.
    %                   Default is [0 1].
    %            'MeanFun' - Fuction handle to calcute the mean of the time
    %                   series. This mean will be stubtracted from the ti
    %                   series prior to filtering.
    %                   Default is @median.
    %            'MeanFunArgs' - A cell array of additional arguments to
    %                   pass to the MeanFun. Default is {1, "omitnan"}.
    %            'MoveFun' - Moving average function (e.g., @movmedian).
    %                   Default is @movmean.
    %            'WinSize' - Moving avergae window size.
    %                   Default is 2.
    %            'EndPoint' - Endpoints parameter for the MoveFun.
    %                   Default is "fill".
    %            'StdFun' - Std function.
    %                   Default is @tools.math.stat.rstd.
    %            'Threshold' - Threshold for flares detection.
    %                   Default is 8.
    %
    % Output : - A structure with the following fields:
    %            .Z - Filter data divided by Std.
    %            .FlagCand - A vector (element per source; i.e., columns of
    %                   the input), indicating if the source have a flare
    %                   or dip above threshold.
    %                   A flare/dip is chosen if its above threshold and
    %                   the number of valid data points within the window
    %                   are equal to the window size, and the Z of the
    %                   flare/dip is higher by one compared to the Z1.
    %                   Z1 is the original data divided by the StD (i.e.,
    %                   unfiltered data).
    %            .NumberNotNaN - A vector (element per source) indicating
    %                   the number of not NaN entries per source.
    %
    % Author : Eran Ofek (2024 May) 
    % Example: M = randn(100,1000);
    %          Result = timeSeries.filter.runMeanFilter(M);

    arguments
        M                      = [];   % [] for simulation mode

        Args.Dim               = 1;
        Args.PolyFit           = [0 1];

        Args.MeanFun           = @median;
        Args.MeanFunArgs       = {1, "omitnan"};

        Args.MoveFun           = @movmean; % @movmedian;
        Args.WinSize           = 2;
        Args.EndPoint          = "fill";

        Args.StdFun            = @tools.math.stat.rstd;

        Args.Threshold         = 8;

    end

    if isempty(M)
        M = randn(100,10);
        M(1,1) = NaN;
        M(20,1) = 10;
        M(21,1) = 10;
    end

    if Args.Dim==2
        M        = M.';
        Args.Dim = 1;
    end

    FlagNotNan    = ~isnan(M);
    NumberNotNan  = sum(FlagNotNan, 1);

    if ~isempty(Args.PolyFit)
        [Nep, Nsrc] = size(M);
        T           = (1:1:Nep).';
        T           = (T - mean(T))./max(T);
    
        % fit a polynomial to eachy column of M
        H = T.^Args.PolyFit;
        Npar = size(H,2);
        % fit each column seperatly and remove NaN
    

        Par = zeros(Npar, Nsrc);
        for Isrc=1:1:Nsrc
            IndNN = find(FlagNotNan(:,Isrc));
            Par(:,Isrc) = H(IndNN,:)\M(IndNN, Isrc);
        end
        % subtract the best fit polynomial
        ResidM = M - H*Par;
    else
        ResidN = M;
    end

    Mean = Args.MeanFun(ResidM, Args.MeanFunArgs{:});

    ResidM = ResidM - Mean;

    MoveM  = Args.MoveFun(ResidM, Args.WinSize, 1, "omitmissing", "Endpoints",Args.EndPoint);

    % count the number of valid points in each window
    ResidM1 = ResidM;
    ResidM1(~isnan(ResidM)) = 1;
    MoveC  = Args.MoveFun(ResidM1, Args.WinSize, 1, "omitmissing", "Endpoints",Args.EndPoint);
    StdM   = Args.StdFun(ResidM);

    Z      = sqrt(Args.WinSize).*MoveM./StdM;

    % filter with window of 1
    Z1     = ResidM./StdM;

    Result.Z            = Z;
    Result.FlagCand     = abs(Z)>Args.Threshold & abs(Z-Z1) > 1 & MoveC==Args.WinSize;
    Result.NumberNotNan = NumberNotNan;
    %Result.Par          = Par;
    
end
