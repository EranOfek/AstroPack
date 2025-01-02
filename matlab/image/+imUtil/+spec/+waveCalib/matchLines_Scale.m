function [BestScale] = matchLines_Scale(ObsLines, RefLines, Args)
    % Given a list of observed and reference lines, estimate the wavelength calibration scale between the two lists.
    %     This function is designed to find the best scale required for
    %     wavelength calibration. It is based on cross-correlating the log
    %     of differences.
    % Input  : - Vector of observed line positions (typically pixels).
    %            If empty, then run in simulation mode. Default is [].
    %          - Vector of reference line positions (typically wavelength)
    %          * ...,key,val,... 
    %            'MaxScale' - Max scale to test. Default is 10.
    %            'StepScale' - Step size for scale testing.
    %                   Default is 0.0005.
    %            'GaussFilter' - If not empty, then convolve the histograms
    %                   with a Gaussian prior to the cross-correlation.
    %                   The Gaussian sigma width is given by this argument.
    %                   Default is 2.
    % Output : - The best scale. This is the scale needed to multiply the
    %            observed line positions in order to get the reference line
    %            positions.
    % Author : Eran Ofek (2024 Jan) 
    % Example: [BestScale] = imUtil.spec.waveCalib.matchLines_Scale

     arguments
        ObsLines                  = [];
        RefLines                  = [];
        Args.MaxScale             = 10;
        Args.StepScale            = 0.0005;
        Args.GaussFilter          = 2;
    end

    if isempty(ObsLines) && isempty(RefLines)
        fprintf('Simulation mode\n');
        
        %%
        Nl         = 55;
        Noverlap   = 45;
        Nnoise     = 10;
        ObsLines   = rand(Nl,1).*3000 + 3000;
        NoiseLines = rand(Nnoise,1).*3000 + 3000;
        
        Ir       = randi(Nl, Noverlap,1);
        RefLines = [ObsLines(Ir); NoiseLines].*3.27 + 1500;
        ObsLines = ObsLines + randn(size(ObsLines,1),1);
        %%
        
    end
    
    %%
    % sort lines
    ObsLines = sort(ObsLines);
    RefLines = sort(RefLines);
    
    % make column vectors
    ObsLines = ObsLines(:);
    RefLines = RefLines(:);
    
    %DiffObsRef = ObsLines - RefLines.';
    
    %hist(DiffObsRef(:),100)
    
    DiffObs  = ObsLines - ObsLines.';
    DiffRef  = RefLines - RefLines.';
    DiffObs  = DiffObs(:);
    DiffRef  = DiffRef(:);
    DiffObs  = DiffObs(DiffObs>0);
    DiffRef  = DiffRef(DiffRef>0);
    
    
    LogDiffObs = log10(DiffObs);
    LogDiffRef = log10(DiffRef);
        
    ScaleEdges = (0:Args.StepScale:Args.MaxScale);
    BinCenter = (ScaleEdges(1:end-1) + ScaleEdges(2:end)).*0.5;
    BinCenterShift = BinCenter - 0.5.*Args.MaxScale; % - 0.5.*Args.StepScale;
    Nobs = histcounts(LogDiffObs, ScaleEdges);
    Nref = histcounts(LogDiffRef, ScaleEdges);
    

    if ~isempty(Args.GaussFilter)
        X = (-3.*Args.GaussFilter:1:3.*Args.GaussFilter);
        GaussianKernel = exp(-X.^2 ./ (2 .* Args.GaussFilter^2));
        GaussianKernel = GaussianKernel ./ sum(GaussianKernel);  % Normalize
        Nobs = conv(Nobs, GaussianKernel, 'same');
        Nref = conv(Nref, GaussianKernel, 'same');
    end

    XC = fftshift(ifft(fft(Nobs).*conj(fft(Nref))));
    [PeakVal,PeakLoc,~,PeakProm]=findpeaks(XC);
    [~,Ipeak]=max(PeakProm);
    BestScale = 10.^(-BinCenterShift(PeakLoc(Ipeak)));
    
    %plot(BinCenterShift, XC)
    %%
    
end
