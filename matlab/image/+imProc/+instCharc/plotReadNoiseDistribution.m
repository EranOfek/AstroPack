function Summary=plotReadNoiseDistribution(Obj, Args)
    % Plot readout noise disyribution from a set of bias images
    % Input  : - A cube of bias images in which the image index is in the
    %            3rd dimension, or an AstroImage of bias images.
    %          * ...,key,val,...
    %            'Gain' - Image gain. Default is 1.
    %            'ConvKernel' - Convolution kernel to execute on bias std
    % %                 image. Default is 1.
    %            'CCDSEC' - CCDSEC [Xmin Xmax Ymin Ymax] in which to
    %                   calculate statistics and plot.
    %                   If empty, use all. Default is [].
    %            'BinEdges' - bin edges of readnoise histogram.
    %                   Default is (0:1:100).'
    %            'LogY' - A logical indicating if to plot the Y axis in
    %                   log. Default is true.
    %            'Prob' - Probability levels in which to calculate
    %                   readnoise quantile and add lines to plot.
    %                   Default is [1e-2, 1e-3, 1e-4, 1e-5, 1e-6].
    %            'ProbLineRatio' - Ratio of lower part of prob. line in
    %                   units of YLim. Default is 0.3.
    % Output : - A structure of statistical p[roperties.
    % Example: S=imProc.instCharc.plotReadNoiseDistribution(Bias_mat, 'ConvKernel',ones(3,3)./9);

    arguments
        Obj
        Args.Gain          = 1;
        Args.ConvKernel    = 1; 
        Args.CCDSEC        = [];
        Args.BinEdges      = (0:1:100).';
        Args.LogY logical  = true;
        Args.Prob          = [1e-2, 1e-3, 1e-4, 1e-5, 1e-6];
        Args.ProbLineRatio = 0.3;
        
    end

    if isa(Obj, 'AstroImage')
        [CubeImage] = imProc.image.images2cube(Obj,'CCDSEC',Args.CCDSEC, 'DimIndex',3);
    else
        CubeImage = Obj;
    end
    CubeImage = CubeImage.*Args.Gain;
    StdImage  = std(CubeImage,[],3);
    if numel(Args.ConvKernel)>1
        StdImage  = conv2(StdImage, Args.ConvKernel);
    end
    
    [N,X] = histcounts(StdImage(:), Args.BinEdges);
    
    XX = (X(1:end-1)+X(2:end)).*0.5;

    H = plot(XX,N,'k.','MarkerSize',12);
    hold on;

    if Args.LogY
        set(gca,'YS','log');
    end

    Hgca = gca;
    YLim = Hgca.YLim(2);
    
    Summary.Prob = Args.Prob;
    Summary.Quant = zeros(size(Args.Prob));
    Nprob = numel(Args.Prob);
    for Iprob=1:1:Nprob
        ValQ = quantile(StdImage(:), 1-Args.Prob(Iprob));
        plot(ValQ.*ones(1,2),YLim.*[Args.ProbLineRatio 1],'k-')
        Ht = text(ValQ,YLim.*Args.ProbLineRatio,sprintf('%3.1e',Args.Prob(Iprob)));
        Ht.Rotation = -90;
    
        Summary.Quant(Iprob) = ValQ;
    end    
    Summary.MeanStd   = mean(StdImage(:));
    Summary.MedianStd = median(StdImage(:));
    Summary.MeanMean  = mean( mean(CubeImage,[1 2]), 'all');
    Summary.MedMed    = median( median(CubeImage,[1 2]), 'all');
    Summary.DistributionRN = [XX(:), N(:)./sum(N)];
    

    hold off;

    H = xlabel('Read Noise [e$^{-}$]');
    H.FontSize    = 18;
    H.Interpreter = 'latex';
    H = ylabel('Number');
    H.FontSize    = 18;
    H.Interpreter = 'latex';

    

end
