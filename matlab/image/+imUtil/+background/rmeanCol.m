function [MeanVec]=rmeanCol(Image, Dim, Args)
    % Robust mean/median on rows or columns of a 2D array.
    %   Calculate robust mean by the following scheme. Sort the array by
    %   one of the dimensions, and remove the x% lower percentile and y%
    %   upper percentile. Next take the mean or median of the remaining
    %   values.
    % Input  : - A 2D array.
    %          - Dimension along to calculate the robust mean.
    %            Either 1 or 2. Default is 1.
    %          * ...,key,val,...
    %            'RemoveFracMin' - The lower fraction of values to remove
    %                   before the mean is calcualted.
    %                   Default is 0.05.
    %            'RemoveFracMax' - The upper fraction of values to use.
    %                   Default is 0.8.
    %            'MeanFun' - Mean function of the form Mean=F(X,Dim,...).
    %                   Default is @median.
    %            'MeanFunARgs' - A cell array of additional arguments to
    %                   pass to the mean function after the dim argument.
    % Output : - Row or column vector of means in each row/column.
    % Author : Eran Ofek (Dec 2021)
    % Example: R=randn(1600,1600);
    %          [MeanVec] = imUtil.background.rmeanCol(R, 1)
    
    
    arguments
        Image
        Dim                       = 1;
        Args.RemoveFracMin        = 0.1;
        Args.RemoveFracMax        = 0.7;
        Args.MeanFun              = @median;
        Args.MeanFunArgs          = {}; %{'omitnan'};
    end

    Nlines      = size(Image, Dim);
    Imin        = max(floor(Args.RemoveFracMin.*Nlines), 1);
    Imax        = ceil(Args.RemoveFracMax.*Nlines);
    SortedImage = sort(Image, Dim);
    
    if Dim==1
        MeanVec = Args.MeanFun(SortedImage(Imin:Imax,:), 1, Args.MeanFunArgs{:});
    elseif Dim==2
        MeanVec = Args.MeanFun(SortedImage(:,Imin:Imax), 2, Args.MeanFunArgs{:});
    else
        error('rmeanCol supports only Dim=1 or 2');
    end
    
end
    