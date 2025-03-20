function [Flag] = histAnomaly(Image, Args)
    % Search for anaomlies in image histogram
    %   Specifically, search for image histogram which is clearly bi-modal.
    %   This can be due to e.g., electronic noise.
    %   Looks for peaks above RelPeakHeight, from those chose the one with
    %   max dist. If dist is in range of RangeDistPeaks then image is bad.
    % Input  : - Image matrix.
    %          * ...,key,val,... 
    %            'CCDSEC' - CCDSEC in which to calculate the histogram.
    %                   If empty, use all image. Default is [].
    %            'Dilute' - Dilute factor to data. Default is 1.
    %            'HistEdges' - Histogram edges.
    %                   Default is (-0.5:5:5000.5)
    %            'RelPeakHeight' - Select peak with height relative to
    %                   maximum are larger than this value.
    %                   Default is 0.2
    %            'RangeDistPeaks' - Distance range of peaks that will
    %                   define a bad image.
    % Output : - A logical indicating if the bi-modal anomaly was detected
    %            in image. Default is true.
    % Author : Eran Ofek (2025 Mar) 
    % Example: R=imUtil.image.histAnomaly(Image)

    arguments
        Image
        Args.CCDSEC            = [];
        Args.Dilute            = 1;
        Args.HistEdges         = (-0.5:5:5000.5);
        Args.RelPeakHeight     = 0.2;
        Args.RangeDistPeaks    = [15 200]
    end

    % trim image using CCDSEC
    if ~isempty(Args.CCDSEC)
        Image = Image(Args.CCCDSEC(3):Args.CCDSEC(4), Args.CCDSEC(1):Args.CCDSEC(2));
    end

    % Dilute image size
    if Args.Dilute>1
        Image = Image(1:Args.Dilute:end);
    end

    % make histogram
    Nh        = histcounts(Image(:), Args.HistEdges);
    BinCenter = (Args.HistEdges(1:end-1) + Args.HistEdges(2:end)).*0.5;
    Nh        = Nh./max(Nh);

    %plot(BinCenter, Nh)
    
    % highest peak
    R=timeSeries.peaks.localMax(Nh(:), 'Filter', [], 'ValThreshold',Args.RelPeakHeight);
    PeaksH    = R.Col.Val;
    PeaksP    = BinCenter(R.Col.Ind);
    if numel(PeaksP)==1
        Flag = false;  % ok
    else
        DP = PeaksP - PeaksP.';
        DP = DP(DP>0);
        %MaxDP = max(DP);
        if any(DP>Args.RangeDistPeaks(1) & DP<Args.RangeDistPeaks(2))
            %if MaxDP>Args.RangeDistPeaks(1) && MaxDP<Args.RangeDistPeaks(2)
            % hist anomaly detected
            Flag = true;  
        else
            Flag = false;
        end
    end

end
