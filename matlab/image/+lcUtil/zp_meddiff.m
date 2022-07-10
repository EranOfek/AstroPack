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
    %            'MinNepoch' - Use sources which the min. number of epochs
    %                   they appear (not NaN) is larger than this number.
    %                   Default is 10.
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
        
        Args.MaxMagErr              = 0.03;
        Args.RefImInd               = 1;
        Args.MinNepoch              = Inf;  % Inf - source appear in all epochs
                
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

        [Nep, Nsrc] = size(Mag);

        % select sources with minimum number of observations
        NdetPerSrc = sum(~isnan(Mag),1);

        MinNepoch = min(Nep, Args.MinNepoch);

        FlagMin    = NdetPerSrc>=MinNepoch;

        Mag    = Mag(:,FlagMin);
        MagErr = MagErr(:,FlagMin);

        [Nep, Nsrc] = size(Mag);

        DiffMagEpoch = Mag - Mag(Args.RefImInd,:);

        Result(Ims).FitZP    = median(DiffMagEpoch, 2, 'omitnan');
        Result(Ims).FitStdZP = std(DiffMagEpoch, [], 2, 'omitnan');
        Result(Ims).FitErrZP = Result(Ims).FitStdZP./sqrt(Nsrc);
        Result(Ims).Nsrc     = Nsrc;
    end
end