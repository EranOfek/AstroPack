function [Mode,Var,Method] = modeVar_Hist(Array, Args)
    % Mode and variance estimator based on histogram fitting
    %     The function make an histogram of the data and a fits its log
    %     counts around the peak region.
    % Input  : - An array.
    %          * ...,key,val,... 
    %            'Range' - The range in which the background is expected.
    %                   Default is [-50 5050].
    %            'NinBin' - A very rough estimator to the number of points
    %                   in bin. This depands on the number of pixel and
    %                   dillution. Default is 1000. For smaller images use
    %                   smaller values.
    %            'ApplyCeil' - Apply ceil to the histogram bin step. This
    %                   is necessery if the input image used to be in
    %                   integer values. Default is true.
    %            'NptFit' - Number of points from each side of the peak of
    %                   the histogram to use in the fitting.
    %                   Default is 5.
    %            'Fit' - If false, then the mode will be estimated from the
    %                   peak of the histogram, and the variance=mode.
    %                   Default is true.
    %            
    % Output : - Best fitted Mode.
    %          - Best fitted variance.
    %          - Method:
    %               'histfit' - histogram fitting.
    %               'poiss' - histogram peak.
    % Author : Eran Ofek (2025 Jan) 
    % Example: imUtil.background.modeVar_Hist(R)

    arguments
        Array
        Args.Range             = [-50 5050];
        Args.NinBin            = 1000;
        Args.ApplyCeil         = true;
        Args.NptFit            = 5;
        Args.Fit               = true;
        Args.Dillute           = [];
        
    end

    Method = 'histfit';
    if ~isempty(Args.Dillute)
        Array = Array(1:Args.Dillute:end);
    else
        Array = Array(:);
    end
    
    Narray  = numel(Array);
    BinSize = Args.NinBin./(Narray./Args.Range(2));
    if Args.ApplyCeil
        BinSize = ceil(BinSize);
    end
    Edges   = (Args.Range(1):BinSize:Args.Range(2));
    Nbin    = numel(Edges)-1;
    [Nhist] = tools.hist.mex.histcounts1regular(Array, Args.Range(1), BinSize, Nbin);
    Nhist   = single(Nhist);
    Xhist   = ((Edges(1:end-1) + Edges(2:end)).*0.5).';
    [MaxBin,IndMax] = max(Nhist);
    Mode0   = Xhist(IndMax); 
    
    if Args.Fit
    
        % fit
        Nfit = Nhist(IndMax-Args.NptFit:IndMax+Args.NptFit);
        Xfit = Xhist(IndMax-Args.NptFit:IndMax+Args.NptFit);
        Nbin = numel(Nfit);
        Flag = Nfit>0;
        Npt = sum(Flag);
        if Npt>3
            Nfit = Nfit(Flag);
            Xfit = Xfit(Flag);
            Nfit = Nfit(:);
            Xfit = Xfit(:);
            
            Nbin = Npt;
            
            Nlog = log(Nfit);

            %Par = polyfit(Xfit,log10(Nfit),2)

            H = [(Xfit-Mode0).^2, (Xfit-Mode0), ones(Nbin,1)];
            Par = (H\Nlog(:)).';
            Mode  = Mode0 - Par(2)./(2.*Par(1));
            Var = -0.5./Par(1);
        else
            Mode = Mode0;
            Var  = Mode;
            Method = 'poiss';
        end
    else
        Mode = Mode0;
        Var  = Mode;
        Method = 'poiss';
    end
        
end
