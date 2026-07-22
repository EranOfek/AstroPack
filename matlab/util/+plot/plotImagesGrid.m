function [Haxes, Himages, Hfig] = plotImagesGrid(Images, GridSize, Args)
    % Plot a cell array of images in a rectangular grid.
    %   Display images stored in a cell array in an Nrow-by-Ncol grid.
    %   Images are plotted in cell-array order, moving along rows first.
    %   Empty image cells are skipped.
    %
    %   The spacing and margins are specified in normalized figure units.
    %   By default, the gray colormap is used and the spacing between
    %   adjacent images is 0.01.
    % Input  : - A cell array containing the images to display.
    %          - A two-element vector [Nrow, Ncol] specifying the grid size.
    %          * ...,key,val,...
    %            'Parent' - Parent figure. If empty, use the current figure.
    %                   Default is [].
    %            'Colormap' - Figure colormap. This may be a colormap name,
    %                   a function handle, or an N-by-3 colormap matrix.
    %                   Default is 'gray'.
    %            'Spacing' - Spacing between adjacent axes in normalized
    %                   figure units. May be a scalar, or
    %                   [Xspacing,Yspacing]. Default is 0.01.
    %            'Margin' - Margin between the axes grid and the figure
    %                   boundaries. May be a scalar, [Xmargin,Ymargin], or
    %                   [Left,Right,Bottom,Top]. Default is 0.01.
    %            'Z1Z2' - Common grayscale limits [Z1,Z2] for all images.
    %                   If empty, use the automatic color limits of each
    %                   image independently. Default is [].
    %            'AxisEqual' - Use equal data-unit lengths along both axes.
    %                   Default is true.
    %            'AxisOff' - Hide axis ticks, labels, and box.
    %                   Default is true.
    %            'YDir' - Direction of the image Y-axis: 'reverse' or
    %                   'normal'. Default is 'reverse'.
    %            'Titles' - Titles associated with the images. May be empty,
    %                   a string array, or a cell array of character
    %                   vectors. Default is [].
    %            'TitleArgs' - Cell array of additional arguments passed to
    %                   title. Default is {}.
    %            'ImageArgs' - Cell array of additional arguments passed to
    %                   imagesc. Default is {}.
    %            'FigureArgs' - Cell array of arguments passed to figure
    %                   when a new current figure is required.
    %                   Default is {}.
    % Output : - Column vector of axes handles. The number of elements is
    %            prod(GridSize).
    %          - Column vector of image-object handles. Empty grid positions
    %            contain invalid graphics placeholders.
    %          - Figure handle.
    % Author : ChatGPT + Eran Ofek (2026 Jul)
    % Example:
    %   Images = {rand(100), peaks(100), magic(50), randn(80)};
    %   plot.plotImagesGrid(Images,[2 2]);
    %
    %   plot.plotImagesGrid(Images,[2 2], ...
    %       'Spacing',0.005, 'Margin',0.02, 'Z1Z2',[-2 2]);

    arguments
        Images                    cell
        GridSize                  (1,2) double {mustBeInteger,mustBePositive}
        Args.Parent               = [];
        Args.Colormap             = 'gray';
        Args.Spacing              = 0.01;
        Args.Margin               = 0.01;
        Args.Z1Z2                 = [];
        Args.AxisEqual            (1,1) logical = true;
        Args.AxisOff              (1,1) logical = true;
        Args.YDir                 = 'reverse';
        Args.Titles               = [];
        Args.TitleArgs            cell = {};
        Args.ImageArgs            cell = {};
        Args.FigureArgs           cell = {};
    end

    Nrow   = GridSize(1);
    Ncol   = GridSize(2);
    Nslot  = Nrow.*Ncol;
    Nimage = numel(Images);

    if Nimage > Nslot
        error('plot:plotImagesGrid:TooManyImages', ...
            ['The number of images (%d) is larger than the number of ' ...
             'grid positions (%d).'],Nimage,Nslot);
    end

    %------------------------------
    % Parse common grayscale limits
    %------------------------------
    if isempty(Args.Z1Z2)
        Z1Z2 = [];
    else
        validateattributes(Args.Z1Z2,{'numeric'}, ...
            {'real','vector','numel',2,'finite'});

        Z1Z2 = double(reshape(Args.Z1Z2,1,2));

        if Z1Z2(2) <= Z1Z2(1)
            error('plot:plotImagesGrid:InvalidZ1Z2', ...
                'Z1Z2 must satisfy Z2 > Z1.');
        end
    end

    %------------------------------
    % Parse spacing
    %------------------------------
    Spacing = Args.Spacing;

    if isscalar(Spacing)
        Xspacing = Spacing;
        Yspacing = Spacing;

    elseif isnumeric(Spacing) && numel(Spacing) == 2
        Xspacing = Spacing(1);
        Yspacing = Spacing(2);

    else
        error('plot:plotImagesGrid:InvalidSpacing', ...
            'Spacing must be a scalar or a two-element vector.');
    end

    validateattributes(Xspacing,{'numeric'}, ...
        {'real','scalar','finite','nonnegative'});
    validateattributes(Yspacing,{'numeric'}, ...
        {'real','scalar','finite','nonnegative'});

    %------------------------------
    % Parse margins
    %------------------------------
    Margin = Args.Margin;

    if isscalar(Margin)
        LeftMargin   = Margin;
        RightMargin  = Margin;
        BottomMargin = Margin;
        TopMargin    = Margin;

    elseif isnumeric(Margin) && numel(Margin) == 2
        LeftMargin   = Margin(1);
        RightMargin  = Margin(1);
        BottomMargin = Margin(2);
        TopMargin    = Margin(2);

    elseif isnumeric(Margin) && numel(Margin) == 4
        LeftMargin   = Margin(1);
        RightMargin  = Margin(2);
        BottomMargin = Margin(3);
        TopMargin    = Margin(4);

    else
        error('plot:plotImagesGrid:InvalidMargin', ...
            ['Margin must be a scalar, a two-element vector, or a ' ...
             'four-element vector.']);
    end

    validateattributes( ...
        [LeftMargin,RightMargin,BottomMargin,TopMargin], ...
        {'numeric'},{'real','finite','nonnegative'});

    %------------------------------
    % Calculate axes dimensions
    %------------------------------
    AvailableWidth = 1 - LeftMargin - RightMargin ...
                       - (Ncol - 1).*Xspacing;

    AvailableHeight = 1 - BottomMargin - TopMargin ...
                        - (Nrow - 1).*Yspacing;

    if AvailableWidth <= 0 || AvailableHeight <= 0
        error('plot:plotImagesGrid:InsufficientSpace', ...
            ['The requested margins and spacing leave no space for the ' ...
             'image axes.']);
    end

    AxesWidth  = AvailableWidth./Ncol;
    AxesHeight = AvailableHeight./Nrow;

    %------------------------------
    % Get parent figure
    %------------------------------
    if isempty(Args.Parent)
        Hfig = gcf;

        if isempty(Hfig) || ~isgraphics(Hfig,'figure')
            Hfig = figure(Args.FigureArgs{:});
        end
    else
        Hfig = Args.Parent;

        if ~isgraphics(Hfig,'figure')
            error('plot:plotImagesGrid:InvalidParent', ...
                'Parent must be a valid figure handle.');
        end
    end

    %------------------------------
    % Set colormap
    %------------------------------
    if isa(Args.Colormap,'function_handle')
        Map = Args.Colormap();
        colormap(Hfig,Map);

    elseif isnumeric(Args.Colormap)
        validateattributes(Args.Colormap,{'numeric'}, ...
            {'2d','ncols',3,'real','finite','nonnegative'});
        colormap(Hfig,Args.Colormap);

    elseif ischar(Args.Colormap) || ...
            (isstring(Args.Colormap) && isscalar(Args.Colormap))
        colormap(Hfig,char(Args.Colormap));

    else
        error('plot:plotImagesGrid:InvalidColormap', ...
            ['Colormap must be a colormap name, a function handle, or ' ...
             'an N-by-3 numeric matrix.']);
    end

    %------------------------------
    % Prepare titles
    %------------------------------
    if isempty(Args.Titles)
        Titles = strings(0,1);

    elseif isstring(Args.Titles)
        Titles = Args.Titles(:);

    elseif iscell(Args.Titles)
        Titles = string(Args.Titles(:));

    elseif ischar(Args.Titles)
        Titles = string({Args.Titles});

    else
        error('plot:plotImagesGrid:InvalidTitles', ...
            'Titles must be empty, a string array, or a cell array.');
    end

    if ~isempty(Titles) && numel(Titles) < Nimage
        error('plot:plotImagesGrid:TooFewTitles', ...
            'The number of titles is smaller than the number of images.');
    end

    %------------------------------
    % Create axes and plot images
    %------------------------------
    Haxes  = gobjects(Nslot,1);
    Himages = gobjects(Nslot,1);

    for Islot = 1:Nslot
        Irow = floor((Islot - 1)./Ncol) + 1;
        Icol = mod(Islot - 1,Ncol) + 1;

        Xpos = LeftMargin + ...
               (Icol - 1).*(AxesWidth + Xspacing);

        % Count rows from top to bottom.
        Ypos = 1 - TopMargin - Irow.*AxesHeight ...
                 - (Irow - 1).*Yspacing;

        Haxes(Islot) = axes( ...
            'Parent',Hfig, ...
            'Units','normalized', ...
            'Position',[Xpos,Ypos,AxesWidth,AxesHeight]);

        if Islot <= Nimage && ~isempty(Images{Islot})
            Himages(Islot) = imagesc( ...
                Haxes(Islot),Images{Islot},Args.ImageArgs{:});

            if ~isempty(Z1Z2)
                Haxes(Islot).CLim = Z1Z2;
            end

            Haxes(Islot).YDir = char(Args.YDir);

            if Args.AxisEqual
                axis(Haxes(Islot),'image');
            end

            if Args.AxisOff
                axis(Haxes(Islot),'off');
            end

            if ~isempty(Titles)
                title(Haxes(Islot),Titles(Islot),Args.TitleArgs{:});
            end

        else
            Haxes(Islot).Visible = 'off';
        end
    end
end
