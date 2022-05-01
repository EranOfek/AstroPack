function S = filter1ues(Tdata, Data, Tfilt, Filt, Args)
    % One dimensional filtering for un-evenly spaced data
    %
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
    for Ilag=1:1:Nlag
        TimeLag = Args.Lag(Ilag);
        
        LagFilt = interp1(Tfilt+TimeLag, Filt, Tdata0, Args.InterpMethod);
        
        Product = LagFilt.*Data;
        Nnn     = sum(~isnan(Product), Dim, 'omitnan');
    
        Norm = 1./sqrt(sum(LagFilt.^2, Dim, 'omitnan'));
    
        S(Ilag,:) = Norm .* sum(Product, Dim, 'omitnan')./(DataStd);
    end
end
