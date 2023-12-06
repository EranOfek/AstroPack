function errorxy1(Data, Args)
    %
    % Example : plot.errorxy1(rand(5,4));
   
    arguments
        Data
        Args.ColX     = 1;
        Args.ColY     = 2;
        Args.ColXeL   = 3;
        Args.ColYeL   = 4;
        Args.ColXeU   = [];
        Args.ColYeU   = [];
        Args.LineWidth  = 1;
        Args.Marker     = 'o';
        Args.MarkerSize = 8;
        Args.Color      = 'k';
        Args.MarkerFaceColor  = 'k';
        Args.MarkerEdgeColor  = 'k';
        Args.LineColor  = 'k';
    end
    
    X   = Data(:,Args.ColX);
    Y   = Data(:,Args.ColY);
    XeL = Data(:,Args.ColXeL);
    YeL = Data(:,Args.ColYeL);
    if isempty(Args.ColXeU)
        XeU = XeL;
    else
        XeU = Data(:,Args.ColXeU);
    end
    if isempty(Args.ColYeU)
        YeU = YeL;
    else
        YeU = Data(:,Args.ColYeU);
    end
    
    N = numel(X);
    K = 0;
    XL_x = nan(N.*3,1);
    XL_y = nan(N.*3,1);
    YL_x = nan(N.*3,1);
    YL_y = nan(N.*3,1);
    
    for I=1:1:N
        K = K + 1;
        XL_x(K) = X(I) - XeL(I);
        XL_y(K) = Y(I);
        YL_x(K) = X(I);
        YL_y(K) = Y(I) - YeL(I);
        
        K = K + 1;
        XL_x(K) = X(I) + XeU(I);
        XL_y(K) = Y(I);
        YL_x(K) = X(I);
        YL_y(K) = Y(I) + YeU(I);
        
        K = K + 1;
        
    end
    Hm           = plot(X, Y,'o');
    hold on;
    Hm.Color     = Args.LineColor;
    Hm.Marker    = Args.Marker;
    Hm.MarkerSize = Args.MarkerSize;
    Hm.MarkerFaceColor = Args.MarkerFaceColor;
    Hm.MarkerEdgeColor = Args.MarkerEdgeColor;
    Hl           = plot(XL_x, XL_y);
    Hl.Color     = Args.LineColor;
    Hl.LineWidth = Args.LineWidth;
    
    Hl           = plot(YL_x, YL_y);
    Hl.Color     = Args.LineColor;
    Hl.LineWidth = Args.LineWidth;
end