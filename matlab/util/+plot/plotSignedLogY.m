function [H, Hup, Hlow] = plotSignedLogY(varargin)
    % Plot logarithmic Y axis for both positive/negative values, by using two subplots.
    %     The upper subplot for positive values, and the lower for negative
    %     values.
    % Input  : * Inputs like those of the plot function.
    %            E.g., plot(Y), plot(X,Y), plot(X,Y,'k.'),
    %            plot(X,Y,'.','MarkerSize',1)
    % Output : - Handle for both axes.
    %            Useful for common labeling.
    %            E.g., ylabel(H, 'RMS [arcsec]');
    %          - Handle of upper subplot.
    %          - Handle of lower subplot.
    % Author : Eran Ofek (2025 Sep) 
    % Example: [H,Hl,Hu]=plot.plotSignedLogY(rand(1000,1),randn(1000,1),'.')
    %          xlabel(H, 'Mag'); ylabel(H, 'RMS');
    %          Hl.MarkerSize=3; Hu.MarkerSize=3;

    
    % -------- Parse inputs like PLOT --------
    isNum  = cellfun(@isnumeric, varargin);
    numIdx = find(isNum);
    if isempty(numIdx), error('SignedSplitSemilogy:Input','Need numeric input (Y or X,Y).'); end

    if numel(numIdx) >= 2
        X = varargin{numIdx(end-1)};
        Y = varargin{numIdx(end)};
        varargin(numIdx(end-1:end)) = [];
    else
        Y = varargin{numIdx(end)};
        X = 1:numel(Y);
        varargin(numIdx(end)) = [];
    end

    if isvector(X) && ~isvector(Y)
        X = X(:);
        if size(Y,1) ~= numel(X)
            error('SignedSplitSemilogy:SizeMismatch', ...
                  'Length(X) must equal size(Y,1) when Y is a matrix.');
        end
    end

    % -------- Masks & split data --------
    posMask = Y > 0;
    negMask = Y < 0;

    Ypos = Y;         Ypos(~posMask)   = NaN;   % only Y>0
    YnegAbs = -Y;     YnegAbs(~negMask)= NaN;   % |Y| for Y<0

    % -------- Symmetric decade range across both panels --------
    posVals = Y(posMask);        posVals = posVals(isfinite(posVals));
    negVals = YnegAbs(negMask);  negVals = negVals(isfinite(negVals));

    if isempty(posVals) && isempty(negVals)
        kMin = -1; kMax = 1;
    else
        kCand = [];
        if ~isempty(posVals), kCand = [kCand, floor(min(log10(posVals))), ceil(max(log10(posVals)))]; end
        if ~isempty(negVals), kCand = [kCand, floor(min(log10(negVals))), ceil(max(log10(negVals)))]; end
        kMin = min(kCand);  kMax = max(kCand);
        if ~isfinite(kMin) || ~isfinite(kMax), kMin = -1; kMax = 1; end
    end
    kVec   = kMin:kMax;
    yTickV = 10.^kVec;
    yLimV  = [10^kMin, 10^kMax];

    % -------- Base layout (inner positions) --------
    fig     = gcf;
    marginL = 0.14;  marginR = 0.05;
    marginT = 0.06;  marginB = 0.14;
    spc     = 0.01;

    W  = 1 - marginL - marginR;
    Ht = 1 - marginT - marginB - spc;
    Hh = Ht/2;

    axLowPos = [marginL, marginB,            W, Hh];
    axUpPos  = [marginL, marginB + Hh + spc, W, Hh];

    axUp  = axes('Parent',fig,'Position',axUpPos);
    axLow = axes('Parent',fig,'Position',axLowPos);

    % -------- Upper panel (Y>0) --------
    axes(axUp);  holdUp = ishold(axUp);  hold(axUp,'on');
    if any(posMask(:))
        Hup = plot(axUp, X, Ypos, varargin{:});
    else
        Hup = gobjects(0);
        text(axUp, 0.5, 0.5, 'No Y>0 data', 'Units','normalized', ...
             'HorizontalAlignment','center','Color',[0.4 0.4 0.4]);
    end
    set(axUp,'YScale','log','Box','on', ...
             'YLim',yLimV,'YTick',yTickV, ...
             'TickLabelInterpreter','tex', ...
             'XTickLabel',[]);                 % hide upper X tick labels

    % Upper Y tick labels: lowest gets \pm
    yLblUp = strings(size(kVec));
    for i = 1:numel(kVec)
        if kVec(i) == 0
            base = "1";
        else
            base = sprintf('10^{%d}', kVec(i));
        end
        if i == 1
            yLblUp(i) = "\pm" + base;
        else
            yLblUp(i) = base;
        end
    end
    set(axUp,'YTickLabel', yLblUp);
    if ~holdUp, hold(axUp,'off'); end

    % -------- Lower panel (Y<0) --------
    axes(axLow); holdLow = ishold(axLow); hold(axLow,'on');
    if any(negMask(:))
        Hlow = plot(axLow, X, YnegAbs, varargin{:});
    else
        Hlow = gobjects(0);
        text(axLow, 0.5, 0.5, 'No Y<0 data', 'Units','normalized', ...
             'HorizontalAlignment','center','Color',[0.4 0.4 0.4]);
    end
    set(axLow,'YScale','log','YDir','reverse','Box','on', ...
              'YLim',yLimV,'YTick',yTickV, ...
              'TickLabelInterpreter','tex');

    % Lower Y tick labels: suppress the TOP label (same as upper lowest)
    yLblLow = strings(size(kVec));
    for i = 1:numel(kVec)
        if i == 1
            yLblLow(i) = "";     % suppress overlap at the split
        else
            if kVec(i) == 0
                yLblLow(i) = "-1";
            else
                yLblLow(i) = sprintf('-10^{%d}', kVec(i));
            end
        end
    end
    set(axLow,'YTickLabel', yLblLow);
    if ~holdLow, hold(axLow,'off'); end

    % -------- Sync X both ways --------
    linkaxes([axUp, axLow], 'x');

    % -------- Build label-safe overlay axes H (encloses both + TightInset) --------
    pUp  = get(axUp, 'Position');
    pLow = get(axLow,'Position');
    L = min(pUp(1), pLow(1));
    B = min(pUp(2), pLow(2));
    R = max(pUp(1)+pUp(3), pLow(1)+pLow(3));
    T = max(pUp(2)+pUp(4), pLow(2)+pLow(4));

    tiUp  = get(axUp, 'TightInset');   % [left bottom right top]
    tiLow = get(axLow,'TightInset');
    dL = max(tiUp(1), tiLow(1));
    dB = max(tiUp(2), tiLow(2));
    dR = max(tiUp(3), tiLow(3));
    dT = max(tiUp(4), tiLow(4));

    Lh = max(0, L - dL);   Bh = max(0, B - dB);
    Rh = min(1, R + dR);   Th = min(1, T + dT);
    hPos = [Lh, Bh, max(0, Rh - Lh), max(0, Th - Bh)];

    % H = axes('Parent',fig, 'Position',hPos, ...
    %          'Color','none','Box','off', ...
    %          'XTick',[],'YTick',[],'XTickLabel',[],'YTickLabel',[], ...
    %          'HitTest','off','PickableParts','none', ...
    %          'HandleVisibility','on','Visible','on');
    % 
    % 
    % 
    % % Hide axle lines but keep labels visible
    % try
    %     H.XRuler.Axle.Visible = 'off';
    %     H.YRuler.Axle.Visible = 'off';
    % catch
    %     % Older MATLAB: ignore if property absent
    % end

    H = axes('Parent',fig, 'Position',hPos, ...
             'Color','none','Box','off', ...
             'XTick',[],'YTick',[], ...
             'XTickLabel',[],'YTickLabel',[], ...
             'HitTest','off','PickableParts','none', ...
             'HandleVisibility','on','Visible','on');
    
    drawnow;
    % Hide the axis lines (spines) but keep labels visible
    H.XRuler.Axle.Visible = 'off';
    H.YRuler.Axle.Visible = 'off';


    % Initialize overlay limits from data; add bi-directional listeners
    set(H,'XLim', get(axUp,'XLim'));
    L1 = addlistener(H,    'XLim','PostSet', @(~,~) set([axUp,axLow],'XLim',get(H,'XLim')));
    L2 = addlistener(axUp, 'XLim','PostSet', @(~,~) set(H,'XLim',get(axUp,'XLim')));
    L3 = addlistener(axLow,'XLim','PostSet', @(~,~) set(H,'XLim',get(axLow,'XLim')));
    setappdata(H,'SignedSplitSemilogy_XLimListeners', {L1,L2,L3});

    % No ylabels on data axes (use ylabel(H,...) if needed)
    ylabel(axUp,''); ylabel(axLow,'');

    % Focus overlay for immediate xlabel(H,...) / ylabel(H,...)
    axes(H);

    H.Position(1:2) = H.Position(1:2)+0.01;
end
