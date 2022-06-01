function Result=biasCharacterization(Files, Args)
    % Characterize a set of bias images
    %   This function is designed to summarize some of the main properties
    %   of bias-like (or dark) images.
    %   The function also generats two plots
    %       fig1 -  histogram of pixel values in an individual image, and
    %           the master bias. Single pix, and 5x5 filter.
    %       fig2 - histogram of master std bias  values. Single pix, and 5x5 filter.
    % Input  : - A dir name, or a file name with wild card, or an
    %            AstroImage object with the bias images.
    %          * ...,key,val,...
    %            'Gain' - Images gain. Default is 1.
    %               All the calculations will be done on the original
    %               images multiplied by the gain.
    %            See code.
    % Output : - A structure with the following fields:
    %            .singleImageChar - corresponds to a single bias image.
    %               with the following fields:
    %               .Npix0 - Number of pixels with val=0
    %               .Npix1 - Number of pixels with val=1
    %               .QuantileLevels - Quantile levels (fraction).
    %               .QuantileVals - Values at which the quantile fractions
    %                       is achived.
    %            .Bias - corresponds to the bias image.
    %               with fields:
    %               .Image - An AstroImage with the master bias.
    %               .Npix0
    %               .Npix1
    %               .QuantileLevels
    %               .QuantileVals
    %               .MeanStd - Mean of std image.
    %               .MedStd
    %               .StdStd
    %               .StdQuantileLevels
    %               .StdQuantileVals
    %            .BiasMask - Corresponds to the bias mask image.
    %               with fields:
    %               .BitSummary - Table of bit mask statistics
    % Author : Eran Ofek (jun 2022)
    % Example:
    % Result=imProc.instCharc.biasCharacterization('/raid/eran/projects/LAST/characterization/Darks/')
   
    arguments
        Files               = '*.fits';  % dir name or file names, or AstroImage object 
        Args.FileTemplate   = '*.fits';
        Args.SingleImInd    = 1;
        Args.Nhist          = 30;
        Args.QuantileLevels = [0.01, 0.1, 0.5, 0.9, 0.99, 0.999, 0.9999, 0.99999]; 
        Args.Gain           = 1;  % correct gain
    end
    
    PWD = pwd;
    
    if isfolder(Files)
        cd(Files);
        
        Files    = dir(Args.FileTemplate);
        FileList = {Files.name};
        AI       = AstroImage(FileList);
    else
        if ischar(Files)
            Files    = dir(Files);
            FileList = {Files.name};
            AI       = AstroImage(FileList);
        else
            % assuming an AstroImage
            AI = Files;
        end
    end
    
    cd(PWD);
    
    % correct gain
    AI = imProc.calib.gainCorrect(AI, Args.Gain);
    
    %% single image histogram 
    % plot the histogram of counts of the fisrt image in the sequence
    % list some properties
    [N,Edges] = histcounts(log10(AI(Args.SingleImInd).Image(:)),Args.Nhist);
    BinPos    = (Edges(2:end) + Edges(1:end-1)).*0.5;
    
    Box = imUtil.kernel2.circ(3);
    
    FilteredAI = AI(Args.SingleImInd).filter(Box, 'CreateNewObj',true);
    [Nf] = histcounts(log10(FilteredAI.Image(:)),Edges);

    figure(1);
    plot(BinPos,N,'-','LineWidth',2);
    hold on;
    plot(BinPos,Nf,'--','LineWidth',2)
    H = gca;
    H.YScale = 'log';
    
    %
    H = xlabel('log$_{10}$(Counts [ADU])');
    H.FontSize = 18;
    H.Interpreter = 'latex';
    H = ylabel('log$_{10}$(N)');
    H.FontSize = 18;
    H.Interpreter = 'latex';
    
    % properties of the first image
    % number of pixels wihz zero counts
    Result.singleImageChar.Npix0 = sum(AI(Args.SingleImInd).Image(:)==0);
    Result.singleImageChar.Npix1 = sum(AI(Args.SingleImInd).Image(:)==1);
    
    Result.singleImageChar.QuantileLevels = Args.QuantileLevels;
    Result.singleImageChar.QuantileVals   = quantile(AI(Args.SingleImInd).Image(:), Args.QuantileLevels);
    
    %% prep dark image
    Bias = imProc.dark.bias(AI);

    Result.Bias.Image = Bias;
    
    %% Bias pixels dustribution
    
    Result.Bias.Npix0 = sum(Bias.Image(:)==0);
    Result.Bias.Npix1 = sum(Bias.Image(:)==1);
    
    Result.Bias.QuantileLevels = Args.QuantileLevels;
    Result.Bias.QuantileVals   = quantile(Bias.Image(:), Args.QuantileLevels);
    
    figure(1);
    [N] = histcounts(log10(Bias.Image(:)),Edges);
        
    FilteredBias = Bias.filter(Box, 'CreateNewObj',true, 'DataProp',{'ImageData','VarData'});
    [Nf] = histcounts(log10(FilteredBias.Image(:)),Edges);

    figure(1);
    plot(BinPos,N,'-','LineWidth',2);
    hold on;
    plot(BinPos,Nf,'--','LineWidth',2)
    legend('single 1x1','single 5x5','bias 1x1','bias 5x5','Location','NorthEast');
    
    %% Bias std distribution
    figure(2);
    [N] = histcounts(log10(sqrt(Bias.Var(:))),Edges);
    [Nf] = histcounts(log10(sqrt(FilteredBias.Var(:))),Edges);

    plot(BinPos,N,'-','LineWidth',2);
    hold on;
    plot(BinPos,Nf,'--','LineWidth',2)
    H = gca;
    H.YScale = 'log';
    
    %
    H = xlabel('log$_{10}$(Counts [ADU])');
    H.FontSize = 18;
    H.Interpreter = 'latex';
    H = ylabel('log$_{10}$(N)');
    H.FontSize = 18;
    H.Interpreter = 'latex';
    
    legend('bias std 1x1','bias std 5x5','Location','NorthEast');
    
    %% Bias std statistics
    Result.Bias.MeanStd = mean(sqrt(Bias.Var(:)));
    Result.Bias.MedStd  = median(sqrt(Bias.Var(:)));
    Result.Bias.StdStd  = std(sqrt(Bias.Var(:)));
    
    Result.Bias.StdQuantileLevels = Args.QuantileLevels;
    Result.Bias.StdQuantileVals   = quantile(sqrt(Bias.Var(:)), Args.QuantileLevels);
    
    %% Mask statistics
    Result.BiasMask = Bias.MaskData.bitStat;
    
end