function idx = selectStringGUI(strs)
    % Display GUI for string selection
    %   After the user select an item, the GUI is closed and the function
    %   returns the index of the selected item.
    % Input  : - Cell array or string array of strings options from which
    %            to choose. E.g., ["SDSS"; "GAIA"]
    %          * ...,key,val,... 
    % Output : - Index of selected item.
    % Author : Eran Ofek (2025 Aug) 
    % Example: Idx = tools.gui.selectStringGUI(["SDSS"; "GAIA"]);

    % Normalize input
    if iscell(strs)
        strs = string(strs);
    end
    strs = strs(:);   % ensure column
    
    % Create figure
    fig = figure('Name','Select Item', ...
                 'MenuBar','none', ...
                 'ToolBar','none', ...
                 'NumberTitle','off', ...
                 'Position',[500 400 300 250], ...
                 'Resize','off', ...
                 'WindowStyle','modal');  %#ok<*UNRCH>
    
    % Listbox
    hList = uicontrol('Style','listbox', ...
                      'String',cellstr(strs), ...
                      'Position',[20 60 260 160], ...
                      'FontSize',11, ...
                      'Max',1,'Min',0, ...
                      'Callback',@onDoubleClick);
    
    % OK button
    uicontrol('Style','pushbutton', ...
              'String','OK', ...
              'Position',[70 20 70 25], ...
              'Callback',@onOK);
    
    % Cancel button
    uicontrol('Style','pushbutton', ...
              'String','Cancel', ...
              'Position',[160 20 70 25], ...
              'Callback',@onCancel);
    
    % Use uiwait/ uiresume to block until user acts
    idx = [];
    uiwait(fig);

    % Nested callbacks
    function onOK(~,~)
        val = get(hList,'Value');
        if ~isempty(val)
            idx = val;
        end
        uiresume(fig);
        delete(fig);
    end

    function onCancel(~,~)
        idx = [];
        uiresume(fig);
        delete(fig);
    end

    function onDoubleClick(src,~)
        % Double-click on item = immediate select
        if strcmp(get(fig,'SelectionType'),'open')
            idx = get(src,'Value');
            uiresume(fig);
            delete(fig);
        end
    end
end
