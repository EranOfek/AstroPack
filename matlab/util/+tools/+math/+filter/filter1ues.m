function [S,Nnn,DataStd] = filter1ues(Tdata, Data, Tfilt, Filt, Args)
    % One dimensional filtering for unevenly spaced data.
    %   Given a matrix and filter template, filter each column of the
    %   matrix with the filter. Since the matrix not need to be evenly
    %   spaced, the filter is interpolated over the columns grid. The
    %   matrix rows represents the same time-coordinates. The matrix may
    %   contain NaNs.
    % Input  : - A vector of times of the data matrix rows.
    %          - A data matrix. Each column represents a different data set
    %            to filter, and the rows corresponds to the time-axis.
    %          - A vector of times for the filter.
    %          - A vector of the filter.
    %          * ...,key,val,...
    %            'Lag' - A vector of time lags to test.
    %                   Default is (0:1:100).
    %            'InterpMethod' - Interpolation method used to interplate
    %                   te template over the data matrix times.
    %                   Default is 'linear'.
    %            'SubMean' - A logical indicating if to subtract the mean
    %                   from each column of the data matrix.
    %                   Default is true.
    % Output : - A mtarix of scores in units of std/sigmas.
    %            Each row corresponds to a time lag in the time lag vector
    %            (i.e., 'Lag' argument), while each column corresponds to
    %            the columns in the input data matrix.
    %          - Same size as the first output argument. Here each element
    %            provides the number of not-NaN values in the filtering
    %            process.
    %          - Vector of the data matrix std (per column).
    % Author : Eran Ofek (May 2022)
    % Example: Tdata = (1:1:100).'; Data = randn(100,10);
    %          Tfilt = (1:1:10).'; Filt = ones(10,1);
    %          S = tools.math.filter.filter1ues(Tdata, Data, Tfilt, Filt)
    
    arguments
        Tdata   % sorted times of data
        Data    % Columns of data (sorted by time)
        Tfilt
        Filt
        Args.Lag               = (0:1:100);
        Args.InterpMethod      = 'linear';
        Args.SubMean logical   = true;
    end
    Dim = 1;
    
    Tdata0 = Tdata - Tdata(1);
    if Args.SubMean
        Data   = Data - mean(Data, Dim, 'omitnan');
    end
    DataStd = std(Data, [], Dim, 'omitnan');
    
    [Nrow, Ncol] = size(Data);
    
    Nlag = numel(Args.Lag);
    S    = nan(Nlag, Ncol);
    Nnn  = nan(Nlag, Ncol);
    for Ilag=1:1:Nlag
        TimeLag = Args.Lag(Ilag);
        
        LagFilt = interp1(Tfilt+TimeLag, Filt, Tdata0, Args.InterpMethod);
        
        Product   = LagFilt.*Data;
        Nnn(Ilag,:) = sum(~isnan(Product), Dim, 'omitnan');
    
        Norm      = 1./sqrt(sum(LagFilt.^2, Dim, 'omitnan'));
    
        S(Ilag,:) = Norm .* sum(Product, Dim, 'omitnan')./DataStd;
    end
end
