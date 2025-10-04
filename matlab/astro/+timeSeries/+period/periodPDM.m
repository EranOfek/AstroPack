function [Result] = peridPDM(T, M, FreqVec, Args)
    % One line description
    %     New periodicity search using PDM, Length, etc.
    % Input  : - 
    %          - 
    %          * ...,key,val,... 
    % Output : - 
    % Author : Eran Ofek (2025 Oct) 
    % Example: R=timeSeries.period.periodPDM(T,M,FreqVec)

    arguments
        T
        M
        FreqVec                = [];
        Args.freqVecArgs       = {};
        Args.BinSize           = 0.1;
        
        Args.SubFun            = @mean;
    end
    
    if isempty(FreqVec)
        FreqVec = timeSeries.period.getFreq(T);
    end

    if ~isempty(Args.SubFun)
        M = M - Args.SubFun(M);
    end

    Npt = numel(T);

    BinCols = {'MidBin', @numel, @mean, @std};
    Col.BinT = 1;
    Col.BinN = 2;
    Col.BinMean = 3;
    Col.BinStd  = 4;

    Nf = numel(FreqVec);
    Result.TotStd       = nan(Nf,1);
    Result.PhaseEntropy = nan(Nf,1);
    Result.BinEntropy   = nan(Nf,1);
    Result.Length       = nan(Nf,1);
    for If=1:1:Nf
        TF = T.*FreqVec(If);
        Phase = TF - floor(TF);
        B = timeSeries.bin.binningFast([Phase, M], Args.BinSize, [0 1], BinCols);
       
        Result.TotStd(If)       = sum(B(:,Col.BinStd).^2);
        Result.PhaseEntropy(If) = -sum(Phase.*log(Phase+eps));
        Result.BinEntropy(If)   = -sum(B(:,Col.BinN)./Npt.*log(B(:,Col.BinN)./Npt+eps));
        Result.Length(If)       = sum(B(:,Col.BinMean).^2 + Args.BinSize.^2);
    end
    Result.TotStd = sqrt(Result.TotStd);
    Result.Length = sqrt(Result.Length);

    Result.NormBinEntropy = (1-(Result.BinEntropy-median(Result.BinEntropy)));

    %plot(FreqVec,( 1+median(R.TotStd)-R.TotStd).*R.Length./median(R.Length) ./R.NormBinEntropy  )


end
