function H = astroView(Img, Args)
    % astroView : Minimal DS9-style image viewer
    %   Left click - center on point.
    %   Double left click - back to zoom 1.0 and center of image.
    %   Load WCS via the WCS argument.
    % Input  : - Image.
    %          * ...,key,val,...
    %            'WCS' - An AstroWCS object with image WCS.
    %                   If provided then will display RA, Dec.
    %                   Default is [].
    %            'Stretch' - "linear"|"log"|"sqrt"|"sinh" 
    %                   Default is "linear".
    %            'Z1Z2' - [Z1 Z2]. If [] then auto.
    %                   Default is [].
    %            'Zoom' - Zoom scalar zoom factor (>0).
    %                   Default is 1.
    %            'FlipX' - Flip x-axis. Default is false.
    %            'FlipY' - Flip y-axis. Default is true.
    %            'FontSize' - Font size. Default is 9.
    %
    % Output : - Figure handle.
    % Author : ChatGPT, Eran Ofek (Sep 2025)
    % Example: astroView(Img);
    %          astroView(Img, 'WCS',W, 'Stretch',"sqrt", 'Zoom',2, 'FlipY',true)
    
    arguments
        Img 
        Args.WCS = []
        Args.Stretch = "linear"
        Args.Z1Z2 = []
        Args.Zoom  = 1
        Args.FlipX logical = false
        Args.FlipY logical = true
        Args.FontSize double = 9
    end
    
    Img = double(Img);
    StretchList = {'linear','log','sqrt','sinh'};
    VStretch = find(strcmpi(char(Args.Stretch),StretchList),1,'first');
    if isempty(VStretch)
        VStretch = 1;
    end
    
    if isempty(Args.Z1Z2)
        Z1Z2 = AutoZ(Img);
    else
        Z1Z2 = Args.Z1Z2(:).';
    end
    
    % ---------- Figure & UI ----------
    Fig = figure('Name','astroView','NumberTitle','off','Color',[0.10 0.10 0.12], ...
                 'Menubar','none','Toolbar','none');
    
    Txt = uicontrol('Style','text','Parent',Fig,'Units','normalized', ...
        'Position',[0.01 0.95 0.98 0.04],'BackgroundColor',[0.12 0.12 0.14], ...
        'ForegroundColor',[0.95 0.95 0.95],'HorizontalAlignment','left', ...
        'FontName','Consolas','String','');
    
    if Args.FlipX
        XDir = 'reverse';
    else
        XDir = 'normal';
    end
    if Args.FlipY
        YDir = 'reverse';
    else
        YDir = 'normal';
    end
    
    Ax = axes('Parent',Fig,'Units','normalized','Position',[0.05 0.08 0.70 0.85], ...
        'Color','k','XColor',[.9 .9 .9],'YColor',[.9 .9 .9], ...
        'XDir',XDir,'YDir',YDir,'FontSize',Args.FontSize);
    axis(Ax,'image'); hold(Ax,'on'); colormap(Ax,gray(256));
    
    UIP = uipanel('Parent',Fig,'Units','normalized','Position',[0.78 0.08 0.20 0.85], ...
        'Title','Display','ForegroundColor',[.9 .9 .9],'BackgroundColor',[0.12 0.12 0.14]);
    
    uicontrol(UIP,'Style','text','String','Stretch','Units','normalized', ...
        'Position',[0.08 0.90 0.35 0.06],'BackgroundColor',UIP.BackgroundColor, ...
        'ForegroundColor',[.9 .9 .9],'HorizontalAlignment','left');
    DdStretch = uicontrol(UIP,'Style','popupmenu','Units','normalized', ...
        'Position',[0.45 0.90 0.47 0.065],'String',StretchList,'Value',VStretch);
    
    uicontrol(UIP,'Style','text','String','Z1','Units','normalized', ...
        'Position',[0.08 0.82 0.12 0.06],'BackgroundColor',UIP.BackgroundColor, ...
        'ForegroundColor',[.9 .9 .9],'HorizontalAlignment','left');
    EdZ1 = uicontrol(UIP,'Style','edit','Units','normalized', ...
        'Position',[0.22 0.82 0.30 0.065],'String',num2str(Z1Z2(1),'%.6g'));
    
    uicontrol(UIP,'Style','text','String','Z2','Units','normalized', ...
        'Position',[0.55 0.82 0.12 0.06],'BackgroundColor',UIP.BackgroundColor, ...
        'ForegroundColor',[.9 .9 .9],'HorizontalAlignment','left');
    EdZ2 = uicontrol(UIP,'Style','edit','Units','normalized', ...
        'Position',[0.69 0.82 0.23 0.065],'String',num2str(Z1Z2(2),'%.6g'));
    
    BtnAuto = uicontrol(UIP,'Style','pushbutton','String','Auto Z1/Z2','Units','normalized', ...
        'Position',[0.08 0.75 0.84 0.065]);
    
    uicontrol(UIP,'Style','text','String','Zoom factor','Units','normalized', ...
        'Position',[0.08 0.66 0.50 0.06],'BackgroundColor',UIP.BackgroundColor, ...
        'ForegroundColor',[.9 .9 .9],'HorizontalAlignment','left');
    EdZoom = uicontrol(UIP,'Style','edit','Units','normalized', ...
        'Position',[0.60 0.66 0.32 0.065],'String',num2str(Args.Zoom,'%.3f'));
    
    % Flip X / Flip Y stacked
    CbFlipX = uicontrol(UIP,'Style','checkbox','String','Flip X','Units','normalized', ...
        'Position',[0.08 0.58 0.84 0.06],'BackgroundColor',UIP.BackgroundColor, ...
        'ForegroundColor',[.9 .9 .9],'Value',Args.FlipX);
    CbFlipY = uicontrol(UIP,'Style','checkbox','String','Flip Y','Units','normalized', ...
        'Position',[0.08 0.52 0.84 0.06],'BackgroundColor',UIP.BackgroundColor, ...
        'ForegroundColor',[.9 .9 .9],'Value',Args.FlipY);
    
    % Initial image
    Him = imagesc(Ax, ApplyStretch(Img, Z1Z2, DdStretch));
    axis(Ax,'image'); xlim(Ax,[0.5 size(Img,2)+0.5]); ylim(Ax,[0.5 size(Img,1)+0.5]);
    
    % State
    St.Img = Img; St.WCS = Args.WCS;
    St.EdZ1 = EdZ1; St.EdZ2 = EdZ2; St.DdStretch = DdStretch; St.EdZoom = EdZoom;
    St.CbFlipX = CbFlipX; St.CbFlipY = CbFlipY;
    St.Ax = Ax; St.Him = Him; St.Txt = Txt;
    guidata(Fig, St);
    
    % Callbacks
    set(DdStretch, 'Callback', @(~,~)UpdateImage());
    set(EdZ1,      'Callback', @(~,~)UpdateImage());
    set(EdZ2,      'Callback', @(~,~)UpdateImage());
    set(BtnAuto,   'Callback', @(~,~)AutoZAndRefresh());
    set(EdZoom,    'Callback', @(~,~)ApplyZoom());
    set(CbFlipX,   'Callback', @(~,~)OnFlip('X'));
    set(CbFlipY,   'Callback', @(~,~)OnFlip('Y'));
    set(Fig, 'WindowButtonMotionFcn', @(~,~)OnMotion());
    set(Fig, 'WindowButtonDownFcn',   @(~,~)OnClick());
    
    ApplyZoom();
    UpdateStatus(NaN,NaN);
    
    if nargout
        H = struct('Figure',Fig,'Axes',Ax,'Image',Him);
    end
    
    % ===== Nested helpers =====
        function OnFlip(Which)
            St = guidata(Fig); if isempty(St), return; end
            switch Which
                case 'X'
                    if St.CbFlipX.Value
                        set(St.Ax,'XDir','reverse');
                    else
                        set(St.Ax,'XDir','normal');
                    end
                case 'Y'
                    if St.CbFlipY.Value
                        set(St.Ax,'YDir','reverse');
                    else
                        set(St.Ax,'YDir','normal');
                    end
            end
        end
    
        function AutoZAndRefresh
            St = guidata(Fig);
            if isempty(St)
                return;
            end
            Z = AutoZ(St.Img);
            St.EdZ1.String = num2str(Z(1),'%.6g');
            St.EdZ2.String = num2str(Z(2),'%.6g');
            guidata(Fig,St); UpdateImage();
        end
    
        function ApplyZoom
            St = guidata(Fig);
            if isempty(St)
                return;
            end
            Zf = str2double(St.EdZoom.String);
            if ~isfinite(Zf) || Zf<=0
                Zf=1;
                St.EdZoom.String = '1';
            end
            Sz = size(St.Img); W = Sz(2); Hh = Sz(1);
            ViewW = W / Zf; ViewH = Hh / Zf;
            Xl = xlim(St.Ax); Yl = ylim(St.Ax);
            Cx = mean(Xl); Cy = mean(Yl);
            if ~isfinite(Cx) || ~isfinite(Cy)
                Cx=(W+1)/2;
                Cy=(Hh+1)/2;
            end
            xlim(St.Ax,[Cx - ViewW/2, Cx + ViewW/2]);
            ylim(St.Ax,[Cy - ViewH/2, Cy + ViewH/2]);
        end
    
        function ZoomHome
            St = guidata(Fig); 
            if isempty(St)
                return;
            end
            St.EdZoom.String = '1'; guidata(Fig,St);
            xlim(St.Ax,[0.5 size(St.Img,2)+0.5]);
            ylim(St.Ax,[0.5 size(St.Img,1)+0.5]);
            UpdateStatus(NaN,NaN);
        end
    
        function UpdateImage
            St = guidata(Fig);
            if isempty(St)
                return;
            end
            Z1 = str2double(St.EdZ1.String); Z2 = str2double(St.EdZ2.String);
            if ~isfinite(Z1) || ~isfinite(Z2) || Z2<=Z1
                ZZ = AutoZ(St.Img); Z1=ZZ(1); Z2=ZZ(2);
                St.EdZ1.String = num2str(Z1,'%.6g'); St.EdZ2.String = num2str(Z2,'%.6g');
            end
            set(St.Him,'CData', ApplyStretch(St.Img, [Z1 Z2], St.DdStretch));
            drawnow limitrate;
        end
    
        function OnMotion
            St = guidata(Fig);
            if isempty(St)
                return;
            end
            Cp = get(St.Ax,'CurrentPoint');
            UpdateStatus(Cp(1,1),Cp(1,2));
        end
    
        function OnClick
            St = guidata(Fig);
            if isempty(St)
                return;
            end
            Sel = get(Fig,'SelectionType');
            Cp = get(St.Ax,'CurrentPoint'); X = Cp(1,1); Y = Cp(1,2);
            if strcmp(Sel,'open')          % double-left-click: home
                ZoomHome();
                return;
            end
            if ~isfinite(X) || ~isfinite(Y)
                return;
            end
            Zf = str2double(St.EdZoom.String);
            if ~isfinite(Zf)||Zf<=0
                Zf=1;
            end
            Sz = size(St.Img); W = Sz(2); Hh = Sz(1);
            ViewW = W / Zf; ViewH = Hh / Zf;
            xlim(St.Ax,[X - ViewW/2, X + ViewW/2]);
            ylim(St.Ax,[Y - ViewH/2, Y + ViewH/2]);
            UpdateStatus(X,Y);
        end
    
        function UpdateStatus(X, Y)
            St = guidata(Fig);
            if isempty(St)
                return;
            end
            if ~isfinite(X) || ~isfinite(Y)
                St.Txt.String = 'X= NaN   Y= NaN   Val= NaN   RA= NaN   Dec= NaN'; return;
            end
            Sz = size(St.Img); Xi = round(X); Yi = round(Y);
            if Xi>=1 && Xi<=Sz(2) && Yi>=1 && Yi<=Sz(1)
                Val = St.Img(Yi, Xi);
            else
                Val = NaN;
            end
            Ra = NaN; Dec = NaN;
            if ~isempty(St.WCS)
                try
                    [Ra, Dec] = St.WCS.xy2sky(X, Y);
                catch
                    Ra=NaN; Dec=NaN;
                end
            end
            St.Txt.String = sprintf('X= %.3f   Y= %.3f   Val= %.3f   RA= %.6f   Dec= %.6f', X, Y, Val, Ra, Dec);
        end
    end
    
    % ===== Local utilities =====
    function Z = AutoZ(Im)
        V = Im(:); V = V(isfinite(V));
        if isempty(V), Z=[0 1];
            return;
        end
        N = numel(V);
        if N>2e6
            rng(0);
            V = V(randperm(N,2e6));
        end
        Z = prctile(V,[0.5 99.5]); 
        if Z(2)<=Z(1)
            Z=[min(V) max(V)];
        end
     end
    
    function C = ApplyStretch(Im, Z1Z2, Dd)
        Z1 = Z1Z2(1); Z2 = Z1Z2(2);
        X = (Im - Z1) / max(Z2 - Z1, eps);
        Mode = lower(Dd.String{Dd.Value});
        switch Mode
            case 'linear', Y = X;
            case 'log',    X = max(X,0); Y = log1p(1000*X) / log1p(1000);
            case 'sqrt',   Y = sqrt(max(X,0));
            case 'sinh',   X = max(X,0); Y = sinh(3*X) / sinh(3);
            otherwise,     Y = X;
        end
        C = min(max(Y,0),1);
end

