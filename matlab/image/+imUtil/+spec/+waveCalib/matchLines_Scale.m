function [Result] = matchLines_Scale(ObsLines, RefLines, Args)
    % One line description
    %     Optional detailed description
    % Input  : - 
    %          - 
    %          * ...,key,val,... 
    % Output : - 
    % Author : Eran Ofek (2024 Jan) 
    % Example: [Result] = imUtil.spec.waveCalib.matchLines_Scale

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
        RefLines = [ObsLines(Ir); NoiseLines].*1.04 + 500;
        
    end
    
    % sort lines
    ObsLines = sort(ObsLines);
    RefLines = sort(RefLines);
    
    % make column vectors
    ObsLines = ObsLines(:);
    RefLines = RefLines(:);
    
    DiffObsRef = ObsLines - RefLines.';
    
    hist(DiffObsRef(:),100)
    
    DiffObs  = ObsLines - ObsLines.';
    DiffRef  = RefLines - RefLines.';
    DiffObs  = DiffObs(:);
    DiffRef  = DiffRef(:);
    DiffObs  = DiffObs(DiffObs>0);
    DiffRef  = DiffRef(DiffRef>0);
    
    
    LogDiffObs = log10(DiffObs);
    LogDiffRef = log10(DiffRef);
    
    Edges = (0:0.01:3);
    Nobs = histcounts(LogDiffObs, Edges);
    Nref = histcounts(LogDiffRef, Edges);
    
    XC = ifft(fft(Nobs).*conj(fft(Nref)));
    
    'a'
    
end
