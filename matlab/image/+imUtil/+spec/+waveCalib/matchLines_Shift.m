function [BestShift] = matchLines_Shift(ObsLines, RefLines, Args)
    % Given a list of observed and reference lines with the same scale, estimate the wavelength calibration shift.
    %     This function is designed to find the best shift required for
    %     wavelength calibration. It is based on cross-correlating the
    %     histograms of the the two line lists.
    %     First use: imUtil.spec.waveCalib.matchLines_Scale to measure the
    %     scale and correct fot it.
    % Input  : - Vector of observed line positions (typically pixels).
    %            If empty, then run in simulation mode. Default is [].
    %          - Vector of reference line positions (typically wavelength).
    %            Both lists must have the scale scale.
    %          * ...,key,val,... 
    %            'Step' - Shift histogram step. Default is 1.
    %            'GaussFilter' - If not empty, then convolve the histograms
    %                   with a Gaussian prior to the cross-correlation.
    %                   The Gaussian sigma width is given by this argument.
    %                   Default is 2.
    % Output : - The best shift. This is the shift needed to add to the
    %            observed line positions in order to get the reference line
    %            positions.
    % Author : Eran Ofek (2024 Jan) 
    % Example: [BestScale] = imUtil.spec.waveCalib.matchLines_Shift 
    
    arguments
        ObsLines                  = [];
        RefLines                  = [];
        Args.Step                 = 1;
        Args.GaussFilter          = 2;
    end

    if isempty(ObsLines) && isempty(RefLines)
        %fprintf('Simulation mode\n');
        
        %
        Nl         = 55;
        Noverlap   = 45;
        Nnoise     = 10;
        ObsLines   = rand(Nl,1).*3000 + 3000;
        NoiseLines = rand(Nnoise,1).*3000 + 3000;
        
        Ir       = randi(Nl, Noverlap,1);
        RefLines = [ObsLines(Ir); NoiseLines].*1 + 1530;
        ObsLines = ObsLines + randn(size(ObsLines,1),1);
        %
        
    end
    
    ObsLines = ObsLines(:);
    RefLines = RefLines(:);
    
    MinWave = min(min(ObsLines), min(RefLines));
    MaxWave = max(max(ObsLines), max(RefLines));
    
    Edges    = (MinWave:Args.Step:MaxWave);
    
    %ObsEdges = (min(ObsLines):Args.ObsStep:max(ObsLines));
    %RefEdges = (min(RefLines):Args.RefStep:max(RefLines));
    
    Nobs = histcounts(ObsLines, Edges);
    Nref = histcounts(RefLines, Edges);
    
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
    BestShift = (MaxWave-MinWave).*0.5 - PeakLoc(Ipeak) + Args.Step.*0.5;
    
    
    %plot(XC)
    

end
