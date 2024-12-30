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

    
    if isempty(ObsLines)
        % simulation mode
    
        A = 4000;
        B = 1.8;
        ObsLines = rand(90,1).*1000;
        RefLines = ObsLines(1:80).*B + A;
        ObsLines = ObsLines + randn(90,1).*0.1;
        Ir = randi([1 80],80,1);
        RefLines = RefLines(Ir);
        
    end
    
    
    %% new method
    
    Edges = (0.1:0.001:10);
    
    Nobs = numel(ObsLines);
    Nref = numel(RefLines);
    
    RangeObs = range(ObsLines);
    RangeRef = range(RefLines);
    
    Nlines = 3;
    Nsim = 1e6;
    
    N = 0;
    for Isim=1:1
        IrObs = randi([1 Nobs], Nlines, Nsim);
        IrRef = randi([1 Nref], Nlines, Nsim);

        RandObsLines = ObsLines(IrObs);
        RandRefLines = RefLines(IrRef);

        F = range(RandObsLines)>(0.1.*RangeObs);
        RandObsLines = RandObsLines(:,F);
        F = range(RandRefLines)>(0.1.*RangeRef);
        RandRefLines = RandRefLines(:,F);

        H = [ones(Nlines,1), RandRefLines(:,1)];

        Par = H\RandObsLines;
        
        Resid = RandObsLines - H*Par;
        StdResid = std(Resid);
        
        [~,Imin] = min(StdResid);
        BestPar = Par(:,Imin)
        
        Hall = [ones(Nref,1), RefLines];
        PredLines = Hall*BestPar;
        clear All;
        K = 0;
        for Il=1:1:Nref
            [MinPred,Ipred]=min(abs(PredLines(Il)-ObsLines));
            if MinPred<5
                K = K + 1;
                All(K).A=[RefLines(Il), PredLines(Il), ObsLines(Ipred)].';
            end
        end
        Nmatch = size([All.A],2)
 
        
        %N = N + histcounts(Par(2,:), Edges);
    end
    
    %%
    
    
    S = zeros(Nsim,1);
    for Isim=1:1:Nsim
        IrObs = randi([1 Nobs], Nlines, 1);
        IrRef = randi([1 Nref], Nlines, 1);
        
        H   = [ones(3,1), RefLines(IrRef)];
        Par = H\ObsLines(IrObs);
        Resid = ObsLines(IrObs) - H*Par;
        S(Isim)=std(Resid);
    end
    
    
    
    
    if isempty(ObsLines) && isempty(RefLines)
        fprintf('Simulation mode');
        
        Nl         = 45;
        Noverlap   = 45; %25;
        Nnoise     = 0; %10;
        ObsLines   = rand(Nl,1).*3000 + 3000;
        NoiseLines = rand(Nnoise,1).*3000 + 3000;
        
        Ir       = randi(Nl, Noverlap,1);
        RefLines = [ObsLines(Ir); NoiseLines].*1.1 + 500;
        
        
        
    end
    
    %% another method
   
    ObsLines = sort(ObsLines);
    RefLines = sort(RefLines);
    
    D1 = ObsLines(:) - ObsLines(:).';
    
    D2 = RefLines(:) - RefLines(:).';
    D  = ObsLines(:) - RefLines(:).';
    
    R1 = ObsLines(:)./ObsLines(:).';
    R2 = RefLines(:)./RefLines(:).';
    R  = ObsLines(:)./RefLines(:).';
    
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
