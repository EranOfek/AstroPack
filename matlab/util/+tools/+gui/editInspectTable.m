function [T, Fig] = editInspectTable(T, Args)
    % Editable table inspector with custom positioning, auto-save, and context actions
    %
    % Input: - (T) A MATLAB table to display and edit
    %        * ...,key,val,...
    %         'InspectFun'    - Function handle or cell array of handles for callbacks
    %               See code for defaults.
    %         'InspectFunArgs'- Cell array of args for each InspectFun
    %         'FunName'       - Cell array of names for context menu entries
    %         'AddLineNumber' - true|false to add row-number column (default false)
    %         'SaveObj'       - string path to a variable in the matlab session base to auto-save edits.
    %                   Default is "ObsS.List.Catalog".
    %         'Units'         - units for the UITable (default 'normalized')
    %         'Position'      - [x y width height] in specified Units. Default is [0.05 0.05 0.9 0.9]
    %
    % Output : 
    % Author : ChatGPT and Eran Ofek (May 2025)
    % Example: tools.gui.editInspectTable(array2table(rand(5,3))

    arguments
        T table
        Args.InspectFun        =     {@(x,y) disp(x),...
                                      @(x,y) pipeline.last.pipes.VisitVariability.plotLC(x, 'AssignToBase','ObjMS'),...
                                      @(x,y) tools.gui.openWebFromCoo(x,'simbad'),...
                                      @(x,y) tools.gui.openWebFromCoo(x,'ned'),...
                                      @(x,y) tools.gui.openWebFromCoo(x,'sdss'),...
                                      @(x,y) tools.gui.openWebFromCoo(x,'decals'),...
                                      @(x,y) tools.gui.openWebFromCoo(x,'ps1'),...
                                      @(x,y) VO.ZTF.wget_ztf_phot(x.ra, x.dec, 1, 'Radius',1.5, 'Plot',true','PlotPS',true, 'AssignToBase','TableZTFg'),...
                                      @(x,y) telescope.obs.daily_observability([35 30]./(180./pi), celestial.time.julday, x.ra./(180./pi), x.dec./(180./pi)),...
                                      @(x,y) telescope.obs.yearly_observability(floor(celestial.time.jd2year(celestial.time.julday)), [x.ra./(180./pi), x.dec./(180./pi)], [35 30]./(180./pi),0,2,0),...
                                      };
    
        Args.InspectFunArgs    = {{},{}, {},{},{}, {}, {}, {}, {}, {}};
        Args.FunName           = {"Display Line", "Plot LC", "SIMBAD", "NED", "SDSS", "DECaLS", "PS1", "ZTF g LC", "Daily Observability", "Yearly Observability"};

        Args.AddLineNumber     = false
        Args.SaveObj           = "ObsS.List.Catalog"
        Args.Units             = "normalized"
        Args.Position          = [0.05 0.05 0.9 0.9]
    end

    % Normalize inputs
    if ~iscell(Args.InspectFun)
        Args.InspectFun = {Args.InspectFun};
    end
    if isempty(Args.InspectFunArgs) || ~iscell(Args.InspectFunArgs{1})
        Args.InspectFunArgs = repmat({{}}, size(Args.InspectFun));
    end
    if ischar(Args.FunName) || isstring(Args.FunName)
        Args.FunName = cellstr(Args.FunName);
    end

    % Create UIFigure
    Fig = uifigure('Name', 'Editable Table Inspector');

    % Prepare uitable layout arguments
    tblArgs = {'Units', Args.Units};
    if ~isempty(Args.Position)
        tblArgs(end+1:end+2) = {'Position', Args.Position};
    end

    % Add row-number column if desired
    if Args.AddLineNumber
        LineNumber = (1:height(T))';
        T = addvars(T, LineNumber, 'Before', 1, 'NewVariableNames', 'N');
        editable = [false, true(1,width(T)-1)];
    else
        editable = true(1,width(T));
    end

    % Create UITable
    Tbl = uitable(Fig, tblArgs{:}, 'Data', T, 'ColumnEditable', editable);
    Tbl.CellEditCallback = @(src,~) cellEditCallback(src, Args);

    % Build context menu
    Cm = uicontextmenu(Fig);
    % Inspect functions
    for i = 1:numel(Args.InspectFun)
        uimenu(Cm, 'Text', Args.FunName{i}, 'MenuSelectedFcn', @(~,~) runInspectFun(Tbl, Args, i));
    end
    % Sort submenu
    sm = uimenu(Cm, 'Text', 'Sort By');
    for c = 1:width(T)
        m = uimenu(sm, 'Text', Tbl.ColumnName{c});
        uimenu(m, 'Text', 'Ascending',  'MenuSelectedFcn', @(~,~) sortBy(Tbl, c, 'ascend'));
        uimenu(m, 'Text', 'Descending', 'MenuSelectedFcn', @(~,~) sortBy(Tbl, c, 'descend'));
    end
    % Add and duplicate actions
    uimenu(Cm, 'Text', 'Add new line',    'MenuSelectedFcn', @(~,~) addNewLine(Tbl, Args));
    uimenu(Cm, 'Text', 'Duplicate line',  'MenuSelectedFcn', @(~,~) duplicateLine(Tbl, Args));
    Tbl.ContextMenu = Cm;

    % Update output
    T = Tbl.Data;

    % Nested helper functions
    function cellEditCallback(src, Args)
        data = src.Data;
        if Args.SaveObj ~= ""
            assignin('base', 'tmpTable', data);
            evalin('base', sprintf('%s = tmpTable;', Args.SaveObj));
            evalin('base', 'clear tmpTable');
        else
            assignin('base', 'T', data);
        end
    end

    function runInspectFun(Tbl, Args, idx)
        sel = Tbl.Selection;
        if isempty(sel)
            uialert(Fig, 'No row selected.', 'Error');
            return;
        end
        row = sel(1);
        rowData = Tbl.Data(row, :);
        Args.InspectFun{idx}(rowData, Args.InspectFunArgs{idx}{:});
    end

    function sortBy(Tbl, colIdx, direction)
        D = Tbl.Data;
        col = D{:,colIdx};
        if isnumeric(col) || isdatetime(col)
            [~,I] = sort(col, direction);
        else
            [~,I] = sort(string(col), direction);
        end
        Tbl.Data = D(I,:);
    end

    function addNewLine(Tbl, Args)
        D = Tbl.Data;
        % Template to preserve types
        template = D(1, :);
        newRow = template;
        for fn = D.Properties.VariableNames
            v = D.(fn{1});
            newRow.(fn{1}) = defaultMissing(v);
        end
        D = [D; newRow];
        if Args.AddLineNumber
            D.N = (1:height(D))';
        end
        Tbl.Data = D;
    end

    function duplicateLine(Tbl, Args)
        sel = Tbl.Selection;
        if isempty(sel), uialert(Fig,'No row selected.','Error'); return; end
        row = sel(1);
        D = Tbl.Data;
        D = [D(1:row, :); D(row, :); D(row+1:end, :)];
        if Args.AddLineNumber
            D.N = (1:height(D))';
        end
        Tbl.Data = D;
    end

    function val = defaultMissing(v)
        if isnumeric(v)
            val = NaN;
        elseif isdatetime(v)
            val = datetime(NaT);
        elseif isduration(v)
            val = duration(NaN);
        elseif isstring(v)
            val = "";
        elseif iscell(v)
            val = {[]} ;
        elseif iscategorical(v)
            val = categorical(missing);
        else
            try
                val = missing(v);
            catch
                val = [];
            end
        end
    end
end
