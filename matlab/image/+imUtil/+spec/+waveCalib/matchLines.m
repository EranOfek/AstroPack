function [Result] = matchLines(ObsLines, RefLines, Args)
    % One line description
    %     Optional detailed description
    % Input  : - 
    %          - 
    %          * ...,key,val,... 
    % Output : - 
    % Author : Eran Ofek (2023 Dec) 
    % Example: ArcSpec=AstroSpec.getSkyArcsSpecLines;   % FeAr is in 9
    %          RL = timeSeries.peaks.localMax([ArcSpec(9).Flux]);
    %          RL = [ArcSpec(9).Wave(RL.Col.Ind), RL.Col.Val];
    %
    %          [Raper] = imUtil.spec.extract.aperPhot(InterpImageWave, 'DimWave',1, 'SubBack',false);
    %          ObsArc   = [Raper.Wave, Raper.Spec];
    %          RO = timeSeries.peaks.localMax(Raper.Spec);
    %          RO = [Raper.Wave(RO.Col.Ind), RO.Col.Val];
    %          
    %          imUtil.spec.waveCalib.matchLines(RO, RL);

    arguments
        ObsLines
        RefLines
        Args.StrongestN           = 30;
        Args.MinRang
    end

    % add line intensity if missing
    Nobs = size(ObsLines,1);
    Nref = size(RefLines,1);
    if size(ObsLines,2)==1
        % add random intensity
        ObsLines = [ObsLines, 1 + randn(Nobs,1).*0.0001];
    end
    if size(RefLines,2)==1
        % add random intensity
        RefLines = [RefLines, 1 + randn(Nref,1).*0.0001];
    end
    
    % sort line lists by intensity
    ObsLinesSel = sortrows(ObsLines, 2);
    RefLinesSel = sortrows(RefLines, 2);
    
    % select strongest lines:
    ObsLinesSel = ObsLinesSel(1:Args.StrongestN,:);
    RefLinesSel = RefLinesSel(1:Args.StrongestN,:);
    
    % choose 3 out of N
    AllC = nchoosek((1:1:Args.StrongestN).',3);
    
    RefW = sort(RefLinesSel(:,1));
    ObsW = sort(ObsLinesSel(:,1));
    MatRefW = RefW(AllC.');
    MatObsW = ObsW(AllC.');
    % Range of all reference combinations
    RangeRefComb = range(MatRefW)./3000;
    
    
    NrefW  = size(MatRefW,2);
    ColPar = zeros(2, NrefW);
    RStd   = zeros(1, NrefW);
    MinRStd = Inf;
    for IrefW=1:1:NrefW
        H = [ones(3,1), MatRefW(:,IrefW)];
        Par = H\MatObsW;
        
        Resid = MatObsW - H*Par;
        Std   = std(Resid,[],1);
        [MinStd,Imin] = min(Std./RangeRefComb(IrefW));
        ColPar(:,IrefW) = Par(:,Imin);
        ColMin(IrefW) = MinStd;
        
        BestPar = Par(:,Imin);
        
        H = [ones(size(RefW)), RefW];
        PredW = H*BestPar;
        
        Im=tools.find.mfind_bin(ObsW(:), PredW(:).');
        RStd(IrefW) = 1.25.*mad(ObsW(Im) - PredW);
        
        if RStd(IrefW)<MinRStd
            BestObsW  = ObsW(Im);
            BestPredW = PredW;
            BestRefW  = RefW;
            BestInd   = IrefW;
            MinRStd   = RStd(IrefW);
        end
            
    end
    
    [~,Ibest] = min(RStd);
    IrefW = Ibest;
    
    %[Result] = imUtil.spec.waveCalib.fitWaveCalib(BestObsW, BestRefW, 'Nsim',[])
    
    'a'
    
    
    
    
end
