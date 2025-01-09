function createSubplotFromAxes(handles, layout, Args)
    % Create a sub plot from a cell array of axes handles.
    % Input  : - A cell array of axes handles.
    %          - Layout [Nrow, Ncol]
    %          * ...,key,val,... 
    %            'Spacing' - Two element vector of spacings between sub
    %                   plots in [X,Y]. Default is [0.02 0.02].
    %            'PositionMargins' - Magins [Xmin Xmax Ymin Ymax].
    %                   Defgault is [0.1 0.1 0.95 0.95].
    % Output : null
    % Author : ChatGPT + Eran Ofek (2025 Jan) 
    % Example: plot.createSubplotFromAxes({ax1, ax2, ax3}, [2, 2]);

    arguments
        handles
        layout
        Args.Spacing             = [0.02 0.02];
        Args.PositionMargins     = [0.1 0.95 0.1 0.95];
        Args.XlabelText          = '';
        Args.YlabelText          = '';
    end

    if ~iscell(handles) || ~all(cellfun(@(h) isgraphics(h, 'axes'), handles))
        error('Handles must be a cell array of valid axes handles.');
    end
    if numel(layout) ~= 2 || any(layout <= 0) || any(mod(layout, 1) ~= 0)
        error('Layout must be a two-element vector of positive integers.');
    end
    if numel(Args.Spacing) ~= 2 || any(Args.Spacing < 0)
        error('Spacing must be a two-element vector with non-negative values.');
    end
    if numel(Args.PositionMargins) ~= 4 || any(Args.PositionMargins < 0) || any(Args.PositionMargins > 1)
        error('positionMargins must be a four-element vector [MinX, MaxX, MinY, MaxY] with values between 0 and 1.');
    end

    % Unpack layout, spacing, and positionMargins
    NsubRow = layout(1);
    NsubCol = layout(2);
    spacingX = Args.Spacing(1);
    spacingY = Args.Spacing(2);
    MinX = Args.PositionMargins(1);
    MaxX = Args.PositionMargins(2);
    MinY = Args.PositionMargins(3);
    MaxY = Args.PositionMargins(4);

    % Compute available space for subplots
    totalWidth = MaxX - MinX - (NsubCol - 1) * spacingX;
    totalHeight = MaxY - MinY - (NsubRow - 1) * spacingY;
    subplotWidth = totalWidth / NsubCol;
    subplotHeight = totalHeight / NsubRow;

    % Number of axes to arrange
    numAxes = numel(handles);
    if numAxes > NsubRow * NsubCol
        warning('Number of axes exceeds available subplot slots. Extra axes will be ignored.');
    end

    % Create new figure
    newFig = figure('Name', 'Combined Subplots', 'Units', 'normalized', 'OuterPosition', [0, 0, 1, 1]);

    % Loop through the handles and position them in the new figure
    for i = 1:min(numAxes, NsubRow * NsubCol)
        % Calculate row and column indices
        row = ceil(i / NsubCol);
        col = mod(i - 1, NsubCol) + 1;

        % Compute normalized position for subplot
        posX = MinX + (col - 1) * (subplotWidth + spacingX);
        posY = MaxY - row * (subplotHeight + spacingY);

        % Get the axes handle
        oldAxes = handles{i};

        % Copy axes to the new figure and set position
        newAxes = copyobj(oldAxes, newFig);
        set(newAxes, 'Position', [posX, posY, subplotWidth, subplotHeight]);
    end

    % Add common X and Y labels if provided
    if ~isempty(Args.XlabelText)
       
        %xlabelHandle = annotation(newFig, 'textbox', [0.5, MinY / 2, 0, 0], 'String', Args.XlabelText, ...
        %    'HorizontalAlignment', 'center', 'VerticalAlignment', 'top', 'FontSize', 12, 'EdgeColor', 'none');

        xlabelAxes = axes('Parent', newFig, 'Position', [0, 0, 1, 1], 'Visible', 'off');
        text(0.5, MinY / 2, Args.XlabelText, 'HorizontalAlignment', 'center', ...
             'VerticalAlignment', 'top', 'FontSize', 16, 'Parent', xlabelAxes);
    
        
    end
    if ~isempty(Args.YlabelText)
       
        %ylabelHandle = annotation(newFig, 'textbox', [MinX / 2, 0.5, 0, 0], 'String', Args.YlabelText, ...
        %    'HorizontalAlignment', 'center', 'VerticalAlignment', 'middle', 'FontSize', 12, 'EdgeColor', 'none', 'Rotation', 90);

        
        ylabelAxes = axes('Parent', newFig, 'Position', [0, 0, 1, 1], 'Visible', 'off');
        text(MinX / 4, 0.5, Args.YlabelText, 'HorizontalAlignment', 'center', ...
             'VerticalAlignment', 'middle', 'FontSize', 16, 'Rotation', 90, 'Parent', ylabelAxes);
    
    end


    %set(newFig, 'PaperUnits', 'centimeters');
    %set(newFig, 'PaperSize', [15, 19]); % Width: 15 cm, Height: 19 cm
    %set(newFig, 'PaperPosition', [0, 0, 15, 19]);
    %newFig=gcf;set(newFig, 'PaperUnits', 'centimeters');set(newFig, 'PaperSize', [15, 19]); set(newFig, 'PaperPosition', [0, 0, 15, 19]);

end
