function [TOut, NewOrder, Deleted] = reorderColumnsGUI(TIn)
    % GUI for reorder and deleting columns in table.
    %     Using this function you can move around table columns.
    % Input  : - A table.
    % Output : - A new table in which the column are reordered according to
    %            the user selection.
    % Author : ChatGPT, Eran Ofek (2025 Sep) 
    % Example: T1=tools.table.reorderColumnsGUI(T);
   
    
    arguments
        TIn table
    end
    
    % ---------- Safe defaults (always assigned) ----------
    TOut      = TIn;
    OrigNames = string(TIn.Properties.VariableNames);
    NewOrder  = 1:numel(OrigNames);
    Deleted   = false(1, numel(OrigNames));
    if isempty(OrigNames), return; end
    
    % ---------- State ----------
    CurrNames = OrigNames;
    Cancelled = true;  % flips to false on Apply
    
    % ---------- UI (classic figure/uicontrol) ----------
    f = figure('Name','Reorder / Delete Table Columns', ...
        'NumberTitle','off','MenuBar','none','ToolBar','none', ...
        'Position',[100 100 520 520], 'Resize','off', ...
        'CloseRequestFcn', @onClose);
    
    uicontrol(f,'Style','text','String','Columns', ...
        'Position',[20 485 300 20],'HorizontalAlignment','left','FontWeight','bold');
    
    lb = uicontrol(f,'Style','listbox','String',cellstr(CurrNames), ...
        'Position',[20 80 300 400],'Max',1,'Min',1,'Value',1, ...
        'FontName','monospaced');
    
    % Buttons
    uicontrol(f,'Style','pushbutton','String','↑','Position',[340 440 60 30], ...
        'Callback',@(src,evt) onMove(-1));
    uicontrol(f,'Style','pushbutton','String','↓','Position',[410 440 60 30], ...
        'Callback',@(src,evt) onMove(+1));
    
    uicontrol(f,'Style','pushbutton','String','Top','Position',[340 400 60 30], ...
        'Callback',@(src,evt) onMoveTo(1));
    uicontrol(f,'Style','pushbutton','String','Bottom','Position',[410 400 60 30], ...
        'Callback',@(src,evt) onMoveTo(numel(CurrNames)));
    
    uicontrol(f,'Style','text','String','New index:','Position',[340 355 130 20], ...
        'HorizontalAlignment','left');
    edIdx = uicontrol(f,'Style','edit','String','1','Position',[340 335 130 25], ...
        'Callback',@(src,evt) onSetIndex());
    
    uicontrol(f,'Style','pushbutton','String','Delete','Position',[340 295 130 30], ...
        'BackgroundColor',[0.95 0.82 0.82], ...
        'Callback',@(src,evt) onDelete());
    
    uicontrol(f,'Style','pushbutton','String','Reset','Position',[20 25 80 30], ...
        'Callback',@(src,evt) onReset());
    uicontrol(f,'Style','pushbutton','String','Cancel','Position',[300 25 80 30], ...
        'Callback',@(src,evt) onCancel());
    uicontrol(f,'Style','pushbutton','String','Apply','Position',[390 25 80 30], ...
        'FontWeight','bold','Callback',@(src,evt) onApply());
    
    % Make sure figure is closed even if an error happens later
    c = onCleanup(@() safeDelete(f));
    
    % Block here
    try
        uiwait(f);
    catch
        % If uiwait fails, we still return safe defaults
    end
    
    % Compute outputs if Apply was pressed
    if isvalid(f) && ~Cancelled
        TOut     = TIn(:, cellstr(CurrNames));
        NewOrder = arrayfun(@(nm) find(OrigNames==nm,1), CurrNames);
    end
    if isvalid(f), delete(f); end
    return
    
    % ===== callbacks =====
        function onMove(step)
            v = get(lb,'Value');
            if isempty(v) || v<1 || v>numel(CurrNames), return; end
            to = min(max(1, v+step), numel(CurrNames));
            if to==v, return; end
            nm = CurrNames(v);
            CurrNames(v) = [];
            CurrNames    = [CurrNames(1:to-1), nm, CurrNames(to:end)];
            refreshLB(to);
        end
    
        function onMoveTo(to)
            v = get(lb,'Value');
            to = min(max(1,to), numel(CurrNames));
            if isempty(v) || v==to, return; end
            nm = CurrNames(v);
            CurrNames(v) = [];
            CurrNames    = [CurrNames(1:to-1), nm, CurrNames(to:end)];
            refreshLB(to);
        end
    
        function onSetIndex()
            v = get(lb,'Value');
            if isempty(v), return; end
            to = str2double(get(edIdx,'String'));
            if ~isfinite(to), return; end
            to = round(to);
            to = min(max(1,to), numel(CurrNames));
            if to==v, return; end
            nm = CurrNames(v);
            CurrNames(v) = [];
            CurrNames    = [CurrNames(1:to-1), nm, CurrNames(to:end)];
            refreshLB(to);
        end
    
        function onDelete()
            v = get(lb,'Value');
            if isempty(v), return; end
            nm = CurrNames(v);
            oi = find(OrigNames==nm,1);
            if ~isempty(oi), Deleted(oi) = true; end
            CurrNames(v) = [];
            if isempty(CurrNames)
                set(lb,'String',{},'Value',[]);
                set(edIdx,'String','');
            else
                newSel = min(v, numel(CurrNames));
                refreshLB(newSel);
            end
        end
    
        function onReset()
            Deleted(:) = false;
            CurrNames  = OrigNames;
            refreshLB(1);
        end
    
        function onApply()
            Cancelled = false;
            if strcmpi(get(f,'WaitStatus'),'waiting'), uiresume(f); end
        end
    
        function onCancel()
            Cancelled = true;
            if strcmpi(get(f,'WaitStatus'),'waiting'), uiresume(f); end
        end
    
        function onClose(~,~)
            Cancelled = true;
            if strcmpi(get(f,'WaitStatus'),'waiting'), uiresume(f); else, delete(f); end
        end
    
        % ===== utils =====
        function refreshLB(sel)
            set(lb,'String',cellstr(CurrNames));
            if nargin<1 || isempty(sel), sel = 1; end
            set(lb,'Value',sel);
            set(edIdx,'String',num2str(sel));
        end
    
        function safeDelete(h)
            if ~isempty(h) && isvalid(h), delete(h); end
        end
    
end
