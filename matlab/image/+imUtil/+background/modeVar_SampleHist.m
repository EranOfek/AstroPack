function [Mode, Var, Method] = modeVar_SampleHist(Array, Args)
    % The mode and robust variance of a sample based on its fitted histogram.
    %       See also: imUtil.background.mex.modeVar_SampleHist_mex
    %   This function estimates the mode and a variance-like width of a
    %   sample using a histogram-based approximation. The algorithm performs
    %   the following steps:
    %   1. Draw a sparse initial sample from the input array.
    %   2. Use the initial sample to estimate lower and upper quantile
    %      bounds of the distribution.
    %   3. Build a histogram from a second, typically larger, sparse sample
    %      within this range.
    %   4. Fit log(histogram counts) as a quadratic function of the bin
    %      centers.
    %   5. Estimate the mode from the vertex of the fitted parabola, and
    %      estimate the variance from the curvature term.
    %   6. If the fit is poorly constrained or fails, switch to a robust
    %      fallback estimator based on the median and normalized MAD.
    %
    %   The method is intended mainly for fast estimation of the background
    %   mode and width in approximately unimodal distributions, and is
    %   especially useful for image background estimation in astronomy.
    %
    % Input  : - An array of numeric values (single or double).
    %          * ...,key,val,...
    %            'Ninit' - Approximate number of data points in the initial
    %                   sparse sample used to estimate the quantiles of the
    %                   distribution. The actual number is determined by
    %                   stride sampling. Default is 1000.
    %            'QuantileRange' - Two-element vector specifying the lower
    %                   and upper quantiles used to define the histogram
    %                   range. Default is [0.1 0.7].
    %                   This asymmetric default is useful when the data have
    %                   a positive tail, e.g. astronomical images with stars
    %                   on top of a sky background.
    %            'Nlarge' - Approximate number of data points in the larger
    %                   sparse sample used to build the histogram. The
    %                   actual number is determined by stride sampling.
    %                   Default is 1e4.
    %            'Nbin' - Approximate number of histogram bins across the
    %                   selected quantile range. Default is 10.
    %            'IntegerData' - Logical indicating if the data should be
    %                   treated as integer-like values. When true, histogram
    %                   edges are aligned to half-integers and the bin width
    %                   is forced to an integer number of units. Default is
    %                   true.
    %            'FitOnlyPeak' - Logical indicating if to fit only a small
    %                   region around the maximum histogram bin rather than
    %                   all non-zero bins. This may improve robustness for
    %                   skewed or contaminated distributions. Default is
    %                   false.
    %            'MaxVar' - Maximum allowed variance from the parabolic fit.
    %                   If the fitted variance is negative or exceeds this
    %                   limit, the code switches to the robust fallback
    %                   method. Default is 1e4.
    %            'MaxRangeLogL' - Maximum range in LogL (from max LogL) to
    %                   fit. Default is 0.6.
    %            'UseMex' - Logical indicating if to use the fast MEX
    %                   version. Default is true.
    %
    % Output : - Estimated mode of the sample.
    %          - Estimated variance of the sample. For method 1, this is
    %            derived from the curvature of the quadratic fit to
    %            log(histogram counts). For method 2, this is the square of
    %            a robust sigma estimate based on the normalized MAD.
    %          - Method used:
    %               1 - Histogram fitting to log(counts).
    %               2 - Histogram fitting failed or was poorly constrained;
    %                   estimate is based on the median and normalized MAD.
    %
    % Notes   : - The returned variance is not the uncertainty of the mode
    %            estimator. It is an estimate of the local width of the
    %            underlying distribution.
    %          - The function uses stride sampling for speed, not random
    %            sampling.
    %          - Best suited for approximately unimodal distributions.
    %
    % Author : Eran Ofek (2026 Apr)
    % Example:
    %          [Mode, Var] = imUtil.background.modeVar_SampleHist(Image(:));
    %          [Mode, Var] = imUtil.background.modeVar_SampleHist(Image(:), ...
    %              QuantileRange=[0.1 0.8], FitOnlyPeak=true);


    arguments
        Array
        Args.Ninit           = 1000;
        Args.QuantileRange   = [0.02 0.8];
        Args.Nlarge          = 1e4;
        Args.Nbin            = 20;
        Args.IntegerData     = true;
        Args.FitOnlyPeak     = false;
        Args.MaxVar          = 1e4;
        Args.MaxRangeLogL    = 0.6
        Args.UseMex          = true;

        Args.RN2             = []; % NOT USED
    end

    if Args.UseMex
        [Mode,Var,Method]=imUtil.background.mex.modeVar_SampleHist_mex(Array, Args);
    else
        % matlab version
        Method = 1;
    
        Array = Array(:);
        Npt   = numel(Array);
    
        StepSampleSmall = max(1, floor(Npt./Args.Ninit));
        StepSampleFinal = max(1, floor(Npt./Args.Nlarge));
        SmallArray      = Array(1:StepSampleSmall:end);
    
        Bounds = quantile(SmallArray, Args.QuantileRange);
    
        if Args.IntegerData
            Xmin   = floor(Bounds(1))-0.5;
            Xmax   = ceil(Bounds(2))+0.5;
            if (Xmax-Xmin)<3
                Xmin = Xmin - 2;
                Xmax = Xmax + 2;
            end
            HistStep = max(1, floor((Xmax-Xmin)./Args.Nbin));
        else
            Xmin = Bounds(1);
            Xmax = Bounds(2);
            HistStep = (Xmax-Xmin)./Args.Nbin;
        end
    
        Edges  = (Xmin:HistStep:Xmax);
        BinCenter = (Edges(1:end-1) + Edges(2:end)).*0.5;
        FinalArray = Array(1:StepSampleFinal:end);
    
        Nhist           = histcounts(FinalArray, Edges);
        if Args.FitOnlyPeak
            [~, Imax] = max(Nhist);
            I1 = max(1, Imax-2);
            I2 = min(numel(Nhist), Imax+2);
            Flag = false(size(Nhist));
            Flag(I1:I2) = Nhist(I1:I2) > 0;
        else
            Flag            = Nhist>0;
        end
        
    
    
        if sum(Flag)<5
            % use alterantive method
    
            Fb = FinalArray>Xmin & FinalArray<Xmax;
            Mode = median(FinalArray(Fb));
            Std = tools.math.stat.std_mad(Array,1);  % median abs deviation from median normalized to std
            Var = Std.^2;
            Method = 2;
        else
    
          
            Y = log(Nhist(Flag));
            [MaxY,ImaxY] = max(Y);
            FlagUse = Y > (MaxY-Args.MaxRangeLogL);

            Flag      = Flag(FlagUse);
            BinCenter = BinCenter(FlagUse);
            Y         = Y(FlagUse);
            [~,ImaxY] = max(Y);

            Mode0     = BinCenter(ImaxY);
            H = ones(sum(Flag),3);
            %BinCenter = BinCenter - Mode0;
            H(:,2) = BinCenter(Flag);
            H(:,3) = BinCenter(Flag).^2;
        
            Par = H\Y(:);
            %Mode = Mode0 - 0.5.*Par(2)./Par(3);
            Mode = -0.5.*Par(2)./Par(3);
            DeltaChi2 = 0.5;
            Var = -DeltaChi2./Par(3);
            %plot(BinCenter, H*Par); hold on; plot(BinCenter, Y, 'o')
        
            if Var<0 || Var>Args.MaxVar
                % no maximum to the parabola
                % switch to simplistic method
                Fb = FinalArray>Xmin & FinalArray<Xmax;
                Mode = median(FinalArray(Fb));
                Std = tools.math.stat.std_mad(Array,1);  % median abs deviation from median normalized to std
                Var = Std.^2;
                Method = 2;
            end
        end
    end
end
