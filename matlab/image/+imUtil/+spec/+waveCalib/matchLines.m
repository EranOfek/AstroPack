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
        ObsLines                  = [];
        RefLines                  = [];
        Args.StrongestN           = 30;
        Args.MinRange             = 500;
    end

    if isempty(ObsLines) && isempty(RefLines)
        fprintf('Simulation mode');
        
        Nl         = 45;
        Noverlap   = 25;
        Nnoise     = 10;
        ObsLines   = rand(Nl,1).*3000 + 3000;
        NoiseLines = rand(Nnoise,1).*3000 + 3000;
        
        Ir       = randi(Nl, Noverlap,1);
        RefLines = [ObsLines(Ir); NoiseLines].*1.3 + 500;
        
        
        
    end
    
    %% an other method
   
    D1 = ObsLines(:) - ObsLines(:).';
    D2 = RefLines(:) - RefLines(:).';
    
    [FlagGood, BestPar, BestStd] = ransacLinearModel(H, Y, Args)
    
    P=polyfit(D1(:),D2(:),1);  % use RANSAC
    D=L1-L2.'./P(1);

    % peak of hist D give the -shift (-100)
    hist(D(:),1000)
    
    
    
    
end
