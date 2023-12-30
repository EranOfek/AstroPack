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
        Noverlap   = 45; %25;
        Nnoise     = 0; %10;
        ObsLines   = rand(Nl,1).*3000 + 3000;
        NoiseLines = rand(Nnoise,1).*3000 + 3000;
        
        Ir       = randi(Nl, Noverlap,1);
        RefLines = [ObsLines(Ir); NoiseLines].*1.0 + 500;
        
        
        
    end
    
    %% an other method
   
    ObsLines = sort(ObsLines);
    RefLines = sort(RefLines);
    
    D1 = ObsLines(:) - ObsLines(:).';
    D2 = RefLines(:) - RefLines(:).';
    D  = ObsLines(:) - RefLines(:).';
    
    R1 = ObsLines(:)./ObsLines(:).';
    R2 = RefLines(:)./RefLines(:).';
    
    N1 = numel(R1);
    N2 = numel(R2);
    N  = min(N1, N2);
    
    H = [ones(N,1), D1(1:N).'];
    [FlagGood, BestPar, BestStd] = tools.math.fit.ransacLinearModel(H, D2(:));
    
    P=polyfit(D1(:),D2(:),1);  % use RANSAC
    D=L1-L2.'./P(1);

    % peak of hist D give the -shift (-100)
    hist(D(:),1000)
    
    
    
    
end
