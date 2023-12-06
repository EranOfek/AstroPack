function Result = binningFast(Data, BinSize, StartEnd, OutCol)
    % Bin [X,Y] data and apply functions to Y values in each bin defined by X
    %       This is a faster version of timeSeries.bin.binning
    % Input  : - [X, Y] data.
    %          - Bin size. Default is 1.
    %          - [Start End] points. If one of them is NaN than replace
    %            with min(X)-eps or max(X)+eps.
    %            Default is [NaN NaN].
    %          - Cell array of column names to calculate. Possible column
    %            names are:
    %            'MidBin'
    %            'MeanBin'
    %            'MedianBin'
    %            'StdBin'
    %            Or any function that returns a scalar to apply to the
    %            observations (e.g., @numel, @mean, @median,
    %            @tools.math.stat.nanmedian).
    %            For other special functions use timeSeries.bin.binning.
    %            Default is {'MidBin', @numel, @mean, @median, @std,
    %            @tools.math.stat.nanmedian, }.
    % Output : - A matrix with the requested columns. One line per bin.
    % Author : Eran Ofek (Sep 2021)
    % Example: XY = rand(1000,2);
    %          Result = timeSeries.bin.binningFast(XY, 0.1, [0 1]);
    %          tic;for I=1:1:1000, Result = timeSeries.bin.binningFast(XY, 0.01, [0 1]); end, toc

    
    arguments
        Data               % [X, Y]
        BinSize       = 1;
        StartEnd      = [NaN NaN];
        OutCol cell   = {'MidBin', @numel, @mean, @median, @std};
    end
    
    Ncol = numel(OutCol);
    
    X = Data(:,1);
    Y = Data(:,2);
    
    if isnan(StartEnd(1))
        StartEnd(1) = min(X) - eps;
    end
    if isnan(StartEnd(2))
        StartEnd(2) = max(X) + eps;
    end
    
    BinInd = ceil((X - StartEnd(1))./BinSize);
    
    MidBin = [StartEnd(1)+0.5.*BinSize:BinSize:StartEnd(2)].';
    Countslen = numel(MidBin);
    
    % Filter out NaNs and out-of-range data
    Flag = BinInd>0 & BinInd<=Countslen;
    Subs = [BinInd(Flag)];
    Y    = Y(Flag);
    %Subs = [BinX(:) BinY(:)];
    %Subs(any(Subs==0, 2),:) = [];
    
    Result = nan(Countslen, Ncol);
    for Icol=1:1:Ncol
        if isa(OutCol{Icol}, 'function_handle')
            Result(:,Icol) = accumarray(Subs, Y, [Countslen 1], OutCol{Icol});
        else
            switch lower(OutCol{Icol})
                case 'midbin'
                    Result(:,Icol) = MidBin;
                case 'meanbin'
                    Result(:,Icol) = accumarray(Subs, X, [Countslen 1], @mean);
                case 'medianbin'
                    Result(:,Icol) = accumarray(Subs, X, [Countslen 1], @median);
                case 'stdbin'
                    Result(:,Icol) = accumarray(Subs, X, [Countslen 1], @std);
                otherwise
                    error('Unknown function string name');
            end
        end
    end
end
