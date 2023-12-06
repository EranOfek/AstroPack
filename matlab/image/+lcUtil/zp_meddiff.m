function Result = zp_meddiff(MS, Args)
    % Estimate light curve ZP based on a simple median of differences.
    %       In this method, the ZP are calculated relative to a specific
    %       image (specified by RefImInd; default is 1).
    %       No color term is calculated.
    %       The ZP is calculated by selecting the stars that appears in all
    %       images with mean error below some threshold (MagMagErr).
    %       The difference in magnitude between sucessive images is
    %       calculated, the median of mag diff is declared as the ZP.
    % Input  : - A MatchedSources object.
    %          * ...,key,val,...
    %            'MagField' - Mag field in the MatchedSources object data.
    %                   Default is 'MAG'.
    %            'MagErrField' - Mag error field in the MatchedSources object data.
    %                   Default is 'MAGERR'.
    %            'MaxMagErr' - Upper limit on the median mag error to use.
    %                   Default is 0.03.
    %            'RefImInd' - Index of reference image. Default is 1.
    %            'MinNepoch' - Minimum number of epochs required.
    %                   Default is Inf.
    %            'MinNsrc' - Minimum number of sources per epoch.
    %                   Default is 10.
    %            'UseWMedian' - A logical indicating if to use weighted
    %                   median instead of median.
    %                   Default is true.
    % Output : - A structure array (element per MatchedSources element)
    %            with the following fields:
    %            .FitZP    - Fitted ZP [mag] per image. Add to image in
    %                   order to get corrected mag.
    %            .FitStdZP - Std in fitted ZP.
    %            .FitErrZP - Error in fitted ZP.
    %            .Nsrc - Number of sources used in estimating the ZP.
    % Author : Eran Ofek (Nov 2021)
    % Example: Fzp   = 1 + rand(100,1);
    %          Fstar = rand(1,200).*3900 + 100; 
    %          Flux = Fzp.*Fstar;
    %          Flux = poissrnd(Flux);
    %          FluxErr = sqrt(Flux);
    %          Mag     = 22-2.5.*log10(Flux);
    %          MeanMag = mean(Mag);
    %          MagErr  = 1.086.*FluxErr./Flux;
    %          MS = MatchedSources;
    %          MS.addMatrix({Mag, MagErr},{'MAG','MAGERR'});
    %          R = lcUtil.zp_meddiff(MS)
    
    arguments
        MS MatchedSources
        Args.MagField char          = 'MAG';
        Args.MagErrField char       = 'MAGERR';
        
        Args.MaxMagErr              = 0.02;
        Args.RefImInd               = 1;
        Args.MinNepoch              = Inf;  % Inf - source appear in all epochs
        Args.MinNsrc                = 10;
        Args.UseWMedian logical     = false;
        
        %Args.Plot(1,1) logical      = false;
    end
    
    Nms = numel(MS);
    for Ims=1:1:Nms
        Mag    = getMatrix(MS(Ims), Args.MagField);
        MagErr = getMatrix(MS(Ims), Args.MagErrField);

        MedMagErr = median(MagErr, 1, 'omitnan');
        FlagMM    = MedMagErr<Args.MaxMagErr;
        Mag       = Mag(:,FlagMM);
        MagErr    = MagErr(:,FlagMM);

        %[Nep, Nsrc] = size(Mag);

        % select sources with minimum number of observations
        NdetPerSrc = sum(~isnan(Mag),1);

        NsrcPerEpoch  = sum(~isnan(Mag), 2);
        FlagGoodEpoch = NsrcPerEpoch>Args.MinNsrc;
        Nep           = sum(FlagGoodEpoch);
        
        MinNepoch = min(Nep, Args.MinNepoch);

        FlagMin    = NdetPerSrc>=MinNepoch;

        Mag    = Mag(FlagGoodEpoch,FlagMin);
        MagErr = MagErr(FlagGoodEpoch,FlagMin);

        [~, Nsrc] = size(Mag);

        DiffMagEpoch = Mag - Mag(Args.RefImInd,:);

        if Args.UseWMedian
            Result(Ims).FitZP(FlagGoodEpoch)    = tools.math.stat.wmedian(DiffMagEpoch, MagErr, 2); 
        else
            Result(Ims).FitZP(FlagGoodEpoch)    = median(DiffMagEpoch, 2, 'omitnan');
        end
        Result(Ims).FitZP(~FlagGoodEpoch)   = NaN;
        
        Result(Ims).FitStdZP = std(DiffMagEpoch, [], 2, 'omitnan');
        Result(Ims).FitErrZP = Result(Ims).FitStdZP./sqrt(Nsrc);
        Result(Ims).Nsrc     = Nsrc;
    end
end