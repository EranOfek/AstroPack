function FlagGood=cleanByExternalSigmaClip(ExtraPars, Args)
    % Clean dataset in a matrix by selecting entries that are with N*sigma from the mean of each column.
    % Input  : - A matrix of values.
    %          * ...,key,val,...
    %            'MeanFun' - Function handle for the mean of
    %                   each column. Default is @median
    %            'StdFun' - Function handle for the std (sigma) of each
    %                   column. Default is @tools.math.stat.rstd
    %            'SigmaClip' - [Lower, Upper] sigma clipping
    %                   Default is [-3 3].
    %            'Dim' - Dimension along to perform the clipping.
    %                   Default is 1 (columns).
    % Output : - A vector of logical flags indicating good rows.
    % Author : Eran Ofek (Oct 2023)
    % Example: FlagGood=timeSeries.detrend.cleanByExternalSigmaClip(rand(10000,5));
    
    arguments
        ExtraPars
        Args.MeanFun     = @median;
        Args.StdFun      = @tools.math.stat.rstd;
        Args.SigmaClip   = [-3 3];
        Args.Dim         = 1;
    end
    
    if Args.Dim==1
        DimComp = 2;
    else
        DimComp = 1;
    end
    
    Mean  = Args.MeanFun(ExtraPars, Args.Dim);
    Sigma = Args.StdFun(ExtraPars, Args.Dim);
    
    Z        = (ExtraPars - Mean)./Sigma;
    FlagMat  = Z>Args.SigmaClip(1) & Z<Args.SigmaClip(2);
    FlagGood = all(FlagMat, DimComp);
    
end
        
