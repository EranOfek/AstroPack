function Result = zp_lsq(MS, Args)
    %
    % Example: Fzp   = 1 + rand(100,1);
    %          Fstar = rand(1,200).*3900 + 100; 
    %          Flux = Fzp.*Fstar;
    %          Flux = poissrnd(Flux);
    %          FluxErr = sqrt(Flux);
    %          Mag     = 22-2.5.*log10(Flux);
    %          MagErr  = 1.086.*FluxErr./Flux;
    %          MS = MatchedSources;
    %          MS.addMatrix({Mag, MagErr},{'MAG','MAGERR'});
    
    % lcUtil.zp_lsq(MS);
   
    arguments
        MS(1,1) MatchedSources
        Args.MagField char          = 'MAG';
        Args.MagErrField char       = 'MAGERR';
        
        Args.MinNepoch              = 20;
        Args.Niter                  = 2;
        Args.UseBL(1,1) logical     = true;   % use \ operator in first iteration
    end
    
    Mag    = getMatrix(MS, Args.MagField);
    MagErr = getMatrix(MS, Args.MagErrField);
    
    % select sources with minimum number of observations
    NdetPerSrc = sum(~isnan(Mag),1);
    FlagMin    = NdetPerSrc>Args.MinNepoch;

    Mag    = Mag(:,FlagMin);
    MagErr = MagErr(:,FlagMin);

    [Nep, Nsrc] = size(Mag);
    
    H       = MatchedSources.designMatrixCalib(Nep, Nsrc);
    Mag     = Mag(:);
    InvVar  = 1./(MagErr.^2); 
    FlagSrc = ~isnan(Mag); 
    
    for I=1:1:Args.Niter
        if (I==1 && Args.Niter>1) || Args.UseBL
            Par    = H(FlagSrc,:)\Mag(FlagSrc);
            ParErr = nan(size(Par));
        else
            % use lscov
            [Par, ParErr] = lscov(H(FlagSrc,:), Mag(FlagSrc), InvVar(FlagSrc));
        end
        
        Resid = Mag - H*Par;
        FitZP      = Par(Nsrc+(1:Nep));
        FitMeanMag = Par(1:Nsrc);
        
        Resid      = reshape(Resid, Nep, Nsrc);
        Std        = std(Resid, [], 1, 'omitnan');
        
        %semilogy(MeanMag,Std,'.')
        %hold on;
        [FlagSrc,Res] = imUtil.calib.resid_vs_mag(FitMeanMag(:),Std(:));
    end
    
    semilogy(FitMeanMag(:),Std(:),'.')
    hold on;  
        

    not working yet
        

            
    
    
    
%     
%     for I=1:1:2
%         Mag    = Mag(:,FlagSrc);
%         MagErr = MagErr(:,FlagSrc);
% 
%         [Nep, Nsrc] = size(Mag);
%         H = MatchedSources.designMatrixCalib(Nep, Nsrc);
% 
%         FlagNN = ~isnan(Mag(:));
% 
%         Par = H(FlagNN,:)\Mag(FlagNN);
% 
%         Resid = Mag(FlagNN) - H(FlagNN,:)*Par;
% 
%         FullResid = nan(size(Mag));
%         FullResid(FlagNN) = Resid;
% 
% 
%         Std = std(FullResid,[],1,'omitnan');
%         MeanMag = median(Mag,1,'omitnan');
%         FitZP      = Par(Nsrc+(1:Nep));
%         FitMeanMag = Par(1:Nsrc);
% 
%         %semilogy(MeanMag,Std,'.')
%         %hold on;
%         [FlagSrc,Res] = imUtil.calib.resid_vs_mag(MeanMag(:),Std(:))
% 
%         % remove bad sources
%     end
    
    
    
    %Fall = ~isnan(sum(FullResid,1))
    %[S2,Summary] = timeseries.sysrem(FullResid(:,Fall))
    
end