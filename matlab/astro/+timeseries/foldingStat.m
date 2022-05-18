function foldingStat(Time, Flux, Err, Period)
    %
   
    arguments
        Time
        Flux
        Err
        Period
        BinSize   = 0.1;
    end
    
    MeanFlux  = mean(Flux);
    Nobs      = numel(Flux);
    Chi2_H0   = sum((Flux./Err).^2);
    ScaledErr = Err.*sqrt(Chi2_H0);
    
    
    Phase = mod(Time(:), Period);
    
    StartEnd = [0 1];
    
    BinInd = ceil((X - StartEnd(1))./BinSize);
    
    MidBin = [StartEnd(1)+0.5.*BinSize:BinSize:StartEnd(2)].';
    Countslen = numel(MidBin);
    
    % Filter out NaNs and out-of-range data
    Flag = BinInd>0 & BinInd<=Countslen;
    Subs = [BinInd(Flag)];
    Y    = Flux(Flag);
    %Subs = [BinX(:) BinY(:)];
    %Subs(any(Subs==0, 2),:) = [];
    
    Result = nan(Countslen, Ncol);
    for Icol=1:1:Ncol
        if isa(OutCol{Icol}, 'function_handle')
            Result(:,Icol) = accumarray(Subs, Y, [Countslen 1], @(x)((x-mean(x)).^2) );
    
    
    switch Method
        case 1
            % binning
            BinSize = 0.1;
            Result = timeseries.binningFast(XY, BinSize, [0 1], {@numel, @mean, @var});
            
            sum(Result(:,3))
    
end