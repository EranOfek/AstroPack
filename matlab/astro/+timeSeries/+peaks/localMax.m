function [Result] = localMax(Data, Args)
    % Find local maxima in 1D time series, including pre-filtering and thresholding.
    %     Find local maxima in columns of a matrix.
    %     Optionally pre-filter the data.
    %     Also can define the local maxima using a threshold
    %     ('LocalMaxThreshold') meaning that the peak point - adjuscent
    %     points is larger than LocalMaxThresh.
    %     Finally, can select only peaks that are above some threshold
    %     ('ValThreshold').
    % Input  : - Data
    %          * ...,key,val,... 
    %            'Dim' - Dimension along to search for local max.
    %                   Default is 1.
    %            'Filter' - Pre filter. If empty, then skip.
    %                   If vector then will be used as the filter.
    %                   Alternatively a function_handle of the form Fun(X,
    %                   Width).
    %                   Default is @(X, Sigma) normpdf(X, 0, Sigma)
    %            'FilterArgs' - A cell array of arguments to pass to the
    %                   filter. By default this is the filter width.
    %                   Default is {1.5}.
    %            'X' - A vector of positions inw which to evaluate the
    %                   filter. Default is (-10:1:10).'
    %            'LocalMaxThresh' - A peak is selected if the local max peak point - adjuscent
    %                   points is larger than LocalMaxThresh.
    %                   Default is 0.
    %            'ValThreshold' - A peak is selected only if its pre-filtering value 
    %                   is larger than this threshold.
    %                   If empty, then the .Col field in the output is not
    %                   returned.
    %                   Default is 0.
    % Output : - A structure with the following fields:
    %            .FlagLocalMax - A matrix of logical which size is the same
    %                   as the input. true indicates that a local max was
    %                   found in this position.
    %            .Col - Will not be returned if ValThreshold is [].
    %                   A structure array with the following fields (one
    %                   element per column in the input):
    %                   .Ind - Indices of local maxima.
    %                   .Val - Value of local maxima.
    % Author : Eran Ofek (2023 Dec) 
    % Example: R=timeSeries.peaks.localMax(rand(100,5));

    arguments
        Data
        Args.Dim               = 1
        Args.Filter            = @(X, Sigma) normpdf(X, 0, Sigma);
        Args.FilterArgs        = {1.5};  % sigma of filter
        Args.X                 = (-10:1:10).';
        Args.LocalMaxThresh    = 0;
        Args.ValThreshold      = 0;
    end

    
    % make sure that the wavelengh is in the first dim
    if Args.Dim==2
        Data = Data.';
    end
    
    if isempty(Args.Filter)
        % no pre filter
        FiltData = Data;
    else
        % filter the data
        if isa(Args.Filter, 'function_handle')
            Filter = Args.Filter(Args.X(:), Args.FilterArgs{:});
        else
            Filter = Args.Filter(:);
        end

        Nwave = size(Data,1);
        Filter = fftshift(Filter);  % move to begining to avoid shift
        FiltData = (ifft(fft(Data,[],1).*conj(fft(Filter,Nwave,1))));
    end
    
    [Nwave, Ndata] = size(FiltData);
    % remove edges of data
    
    Result.FlagLocalMax = (FiltData(2:end-1,:) - FiltData(1:end-2,:))>Args.LocalMaxThresh & (FiltData(2:end-1,:) - FiltData(3:end,:))>Args.LocalMaxThresh;
    Result.FlagLocalMax = [false(1, Ndata); Result.FlagLocalMax; false(1, Ndata)];
    
    if ~isempty(Args.ValThreshold)
        for Idata=1:1:Ndata
            Ind = find(Result.FlagLocalMax(:,Idata));
            Val = Data(Ind, Idata);
            Isel = find(Val > Args.ValThreshold);
            Result.Col(Idata).Ind  = Ind(Isel);
            Result.Col(Idata).Val  = Val(Isel);

        end  
    end
    
end
