function [Flag] = histAnomaly(Image, Args)
    % Search for anaomlies in image histogram
    %   Specifically, search for image histogram which is clearly bi-modal.
    %   This can be due to e.g., electronic noise.
    %   Looks for peaks above RelPeakHeight, from those chose the one with
    %   max dist. If dist is in range of RangeDistPeaks then image is bad.
    %   By default the bin width is a constant fraction of the sky level
    %   (but not below MinBinWidth), so that the sky peak broadened on a
    %   bright (e.g., moonlit) sky is not split into several maxima
    %   (issue #1179). For a sky level below MinBinWidth/BinWidthFrac the
    %   histogram is the fixed one used before (-0.5:5:5000.5).
    % Input  : - Image matrix.
    %          * ...,key,val,...
    %            'CCDSEC' - CCDSEC in which to calculate the histogram.
    %                   If empty, use all image. Default is [].
    %            'Dilute' - Dilute factor to data. Default is 1.
    %            'HistEdges' - Histogram edges (regular). If given, these
    %                   fixed edges and RangeDistPeaks are used as is
    %                   (no scaling with the sky level).
    %                   If empty, the edges are set from the sky level:
    %                   bin width W = round(MinBinWidth*S), where
    %                   S = max(1, BinWidthFrac*Sky/MinBinWidth),
    %                   edges from -0.5 up to max(MinHistMax, 2*median).
    %                   Default is [].
    %            'BinWidthFrac' - Bin width as a fraction of the sky level.
    %                   Default is 0.01.
    %            'MinBinWidth' - Minimum bin width [ADU]. Default is 5.
    %            'MinHistMax' - Minimum upper end of the histogram [ADU].
    %                   Default is 5000.
    %            'OverscanSec' - [Xmin Xmax Ymin Ymax] overscan section.
    %                   If given, the sky level is the median of the
    %                   CCDSEC region minus the median of the overscan.
    %                   Default is [].
    %            'Offset' - Offset [ADU] subtracted from the median to get
    %                   the sky level, used if OverscanSec is empty.
    %                   Default is 0.
    %            'RelPeakHeight' - Select peak with height relative to
    %                   maximum are larger than this value.
    %                   Default is 0.04
    %            'RangeDistPeaks' - Distance range [ADU] of peaks that will
    %                   define a bad image. With sky-scaled edges the lower
    %                   limit is multiplied by S (but kept below the upper
    %                   limit). Default is [15 400].
    %            'Plot' - Plot the histogram. Default is false.
    %            'UseMex' - Use mex histogram and median. Default is true.
    % Output : - A logical indicating if the bi-modal anomaly was detected
    %            in image. If true, then the image is bad.
    % Author : Eran Ofek (2025 Mar)
    % Example: R=imUtil.image.histAnomaly(Image)
    %          R=imUtil.image.histAnomaly(Image, 'CCDSEC',[1 6388 25 9600], 'OverscanSec',[6389 6422 1 9600])

    arguments
        Image
        Args.CCDSEC            = [];
        Args.Dilute            = 1;
        Args.HistEdges         = [];
        Args.BinWidthFrac      = 0.01;
        Args.MinBinWidth       = 5;
        Args.MinHistMax        = 5000;
        Args.OverscanSec       = [];
        Args.Offset            = 0;
        Args.RelPeakHeight     = 0.04;
        Args.RangeDistPeaks    = [15 400];
        Args.Plot              = false;
        Args.UseMex            = true;
    end

    % overscan level (before trimming)
    if isempty(Args.OverscanSec)
        Offset = Args.Offset;
    else
        Over   = Image(Args.OverscanSec(3):Args.OverscanSec(4), Args.OverscanSec(1):Args.OverscanSec(2));
        Offset = fastMedian(Over(:), Args.UseMex);
    end

    % trim image using CCDSEC
    if ~isempty(Args.CCDSEC)
        Image = Image(Args.CCDSEC(3):Args.CCDSEC(4), Args.CCDSEC(1):Args.CCDSEC(2));
    end

    % Dilute image size
    if Args.Dilute>1
        Image = Image(1:Args.Dilute:end);
    end

    % histogram grid
    RangeDistPeaks = Args.RangeDistPeaks;
    if isempty(Args.HistEdges)
        % bin width proportional to the sky level, integer so that every
        % bin holds the same number of integer ADU values
        Med      = fastMedian(Image(:), Args.UseMex);
        Scale    = max(1, Args.BinWidthFrac.*(Med - Offset)./Args.MinBinWidth);
        BinSize  = round(Args.MinBinWidth.*Scale);
        BinStart = -0.5;
        BinN     = ceil(max(Args.MinHistMax, 2.*Med)./BinSize);
        RangeDistPeaks(1) = min(RangeDistPeaks(1).*Scale, RangeDistPeaks(2)-1);
    else
        BinStart = Args.HistEdges(1);
        BinSize  = Args.HistEdges(2) - Args.HistEdges(1);
        BinN     = numel(Args.HistEdges) - 1;
    end
    HistEdges = BinStart + (0:BinN).*BinSize;

    % make histogram
    if Args.UseMex
        Nh = double(tools.hist.mex.histcounts1regular(Image(:), BinStart, BinSize, BinN));
    else
        % HistEdges is a full bin-edges vector, consistent with the
        % BinStart/BinSize/BinN of the UseMex branch (issue #1203).
        Nh = histcounts(Image(:), HistEdges);
    end
    BinCenter = (HistEdges(1:end-1) + HistEdges(2:end)).*0.5;
    Nh        = Nh./max(Nh);

    if Args.Plot
        plot(BinCenter, Nh);
    end

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
        if any(DP>RangeDistPeaks(1) & DP<RangeDistPeaks(2))
            %if MaxDP>Args.RangeDistPeaks(1) && MaxDP<Args.RangeDistPeaks(2)
            % hist anomaly detected
            Flag = true;
        else
            Flag = false;
        end
    end

end

function Med = fastMedian(Vec, UseMex)
    % median of a vector, optionally via the mex (single/double only)
    if UseMex
        if ~isfloat(Vec)
            Vec = single(Vec);
        end
        Med = double(tools.math.stat.mex.median(Vec, 1));
    else
        Med = double(median(Vec, 1));
    end
end
