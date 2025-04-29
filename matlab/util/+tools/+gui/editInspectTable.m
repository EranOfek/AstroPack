function T = editInspectTable(T, Args)
    % editable viewer and inspection tool for table T.
    %   Allows modifying tables and executing pre-defined function on
    %   table rows by menu selection.
    % Input  : - Table.
    %          * ...,key,val,...
    %            'InspectFun' - A function handle or a cell array of
    %                   function handles.
    %                   These are the function that will becom exacutable
    %                   from the right-click mouse menue.
    %                   Functions are of the form Fun(T(Ind,:),Args.InspectFunArgs{:})
    %                   Default is: {@(x,y) disp(x),...
    %                             @(x,y) web(VO.search.simbad_url(x.ra./(180./pi), x.dec./(180./pi)).URL),...
    %                             @(x,y) web(VO.NED.ned_link(x.ra./(180./pi), x.dec./(180./pi)).URL),...
    %                             @(x,y) web(VO.SDSS.navigator_link(x.ra./(180./pi), x.dec./(180./pi)).URL),...
    %                             @(x,y) web(VO.DECaLS.decals_viewer_link(x.ra./(180./pi), x.dec./(180./pi)).URL),...
    %                             @(x,y) web(VO.PS1.navigator_link(x.ra./(180./pi), x.dec./(180./pi)).URL)}
    %            'InspectFunArgs' - Cell array of cell arrays of additional
    %                   parameters to pass to each function.
    %                   Default is {{}}.
    %            'FunName' - A cell array of function names that will
    %                   appear in the right-click-mouse menue.
    %                   Default is {'Inspect'}
    %            'AddLineNumber' - If true, then will add a non-editable
    %                   line number in the first column.
    %                   Default is true.
    % Output : - The modified table.
    % Example: T = tools.gui.editInspectTable(array2table(rand(10,5)))

    arguments
        T table
        Args.InspectFun        = {@(x,y) disp(x),...
                                  @(x,y) web(VO.search.simbad_url(x.ra./(180./pi), x.dec./(180./pi)).URL),...
                                  @(x,y) web(VO.NED.ned_link(x.ra./(180./pi), x.dec./(180./pi)).URL),...
                                  @(x,y) web(VO.SDSS.navigator_link(x.ra./(180./pi), x.dec./(180./pi)).URL),...
                                  @(x,y) web(VO.DECaLS.decals_viewer_link(x.ra./(180./pi), x.dec./(180./pi)).URL),...
                                  @(x,y) web(VO.PS1.navigator_link(x.ra./(180./pi), x.dec./(180./pi)).URL),...
                                  @(x,y) pipeline.last.pipes.VisitVariability.plotLC(x(1,:))};

        Args.InspectFunArgs    = {{},{},{},{}, {}, {}, {}};
        Args.FunName           = {"Display Line", "SIMBAD", "NED", "SDSS", "DECaLS", "PS1", "Plot LC"};
        Args.AddLineNumber     = true;
    end

    if ~iscell(Args.InspectFun)
        Args.InspectFun = {Args.InspectFun};
    end

    if isempty(Args.InspectFunArgs)
        Args.InspectFunArgs = {Args.InspectFunArgs};
    else
        if ~iscell(Args.InspectFunArgs{1})
            Args.InspectFunArgs = {Args.InspectFunArgs};
        end
    end

    if ischar(Args.FunName) || isstring(Args.FunName)
        Args.FunName = cellstr(Args.FunName);
    end

    Fig = uifigure('Name', 'Editable Table Inspector');

    % Add a line number column
    if Args.AddLineNumber
        LineNumber = (1:height(T))';
        T = addvars(T, LineNumber, 'Before', 1, 'NewVariableNames', 'N');
        Tbl = uitable(Fig, ...
            'Data', T, ...
            'ColumnEditable', [false, true(1, width(T)-1)]);
    else
        Tbl = uitable(Fig, ...
            'Data', T, ...
            'ColumnEditable', true);
    end

    % Attach CellEditCallback to update T when edited
    Tbl.CellEditCallback = @(Src, Event) assignin('base', 'T', Src.Data);

    % Create right-click context menu
    Cm = uicontextmenu(Fig);
    for iFun = 1:numel(Args.InspectFun)
        uimenu(Cm, 'Text', Args.FunName{iFun}, 'MenuSelectedFcn', @(Src, Event) runInspectFun(Tbl, Args, iFun));
    end

    % Add sort submenu
    SortMenu = uimenu(Cm, 'Text', 'Sort By');
    for iCol = 1:width(T)
        ColMenu = uimenu(SortMenu, 'Text', Tbl.ColumnName{iCol});
        uimenu(ColMenu, 'Text', 'Sort Ascending', 'MenuSelectedFcn', @(src, event) sortByColumn(Tbl, iCol, 'ascend'));
        uimenu(ColMenu, 'Text', 'Sort Descending', 'MenuSelectedFcn', @(src, event) sortByColumn(Tbl, iCol, 'descend'));
    end

    Tbl.ContextMenu = Cm;

    function runInspectFun(Tbl, Args, FunIndex)
        Row = Tbl.Selection(1);
        CurrentData = Tbl.Data;
        if ~isempty(Row) && Row > 0 && Row <= height(CurrentData)
            Args.InspectFun{FunIndex}(CurrentData(Row,:), Args.InspectFunArgs{FunIndex}{:});
        else
            uialert(ancestor(Tbl,'figure'), 'No row selected.', 'Selection Error');
        end
    end

    function sortByColumn(Tbl, ColIndex, Direction)
        Data = Tbl.Data;
        if isnumeric(Data{:,ColIndex}) || isdatetime(Data{:,ColIndex})
            [~,Idx] = sort(Data{:,ColIndex}, Direction);
        else
            [~,Idx] = sort(string(Data{:,ColIndex}), Direction);
        end
        Tbl.Data = Data(Idx,:);
    end
end
