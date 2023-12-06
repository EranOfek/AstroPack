function [H, Ha1, Ha2] = histCumLabel(X, Edges, Args)
    % Plot an histogram (using histogram) with cumulative fraction label in the upper axis.
    % Input  : - A Vector of data.
    %          - Edges, or number of bins (see histogram).
    %            Default is 10.
    %          * ...,key,val,...
    %            'CumTicks' - A cell array of strings of cumulative
    %                   fractional ticks to label.
    %                   default is {'0.001', '0.01',  '0.1', '0.5', '0.9', '0.99', '0.999'};
    %            'histcountsArgs' - A cell array of additional arguments to
    %                   pass to histogram. Default is {}.
    % Output : - Bar handle.
    %          - Main axis handle.
    %          - cumulative quantile axis handle.
    % Author : Eran Ofek (Apr 2022)
    % Example: X = randn(10000,1);
    %          plot.histCumLabel(X, 10)
   
    arguments
        X
        Edges                    = 10;
        Args.CumTicks cell       = {'0.001', '0.01',  '0.1', '0.5', '0.9', '0.99', '0.999'};
        Args.histcountsArgs cell = {};
    end
   
    [N, Edges] = histcounts(X, Edges, Args.histcountsArgs{:});
    MidX = (Edges(1:end-1) + Edges(2:end)).*0.5;
    
    TL = tiledlayout(1,1);
    Ha1 = axes(TL);
    H = bar(Ha1, MidX, N);
    
    
    % plot upper X-axis with quantiles
    
    Quant = quantile(X, str2double(Args.CumTicks));
    
    
    Ha2 = axes(TL);
    %plot(Ha1,1,1)
    Ha2.XAxisLocation = 'top';
    Ha2.YAxisLocation = 'right';
    Ha2.YTickLabel    = [];
    
    Ha1.Box           = 'off';
    Ha2.Box           = 'off';
    Ha2.Color         = 'none';
    Ha2.XLim          = Ha1.XLim;
    Ha2.XTick         = Quant.';
    Ha2.XTickLabel    = Args.CumTicks;    
    
end