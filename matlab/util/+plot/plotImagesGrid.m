function [Haxes, Himages, Hfig, Hcolorbar] = plotImagesGrid(Images, GridSize, Args)
    % Plot a cell array of images in a rectangular grid.
    %
    % Package: plot
    %
    % Description:
    %   Display images stored in a cell array in an Nrow-by-Ncol grid.
    %   Images are plotted in cell-array order, moving along rows first.
    %   Empty image cells are skipped.
    %
    %   When Spacing is scalar, the physical spacing between images is
    %   identical in the X and Y directions, independent of the figure
    %   aspect ratio.
    %
    %   The image axes themselves are constructed with the appropriate
    %   image aspect ratio. This avoids the extra internal padding that
    %   may be introduced by "axis image".
    %
    % Input:
    %   Images
    %       Cell array containing images.
    %
    %   GridSize
    %       [Nrow Ncol].
    %
    %   ...,key,val,...
    %
    %   'Parent'
    %       Parent figure. If empty, use the current figure. If no current
    %       figure exists, create one.
    %       Default: [].
    %
    %   'Colormap'
    %       Colormap name, function handle, or N-by-3 matrix.
    %       Examples:
    %           'gray'
    %           'parula'
    %           @() flipud(gray)
    %       Default: 'gray'.
    %
    %   'Spacing'
    %       If scalar:
    %           Horizontal spacing in normalized figure-width units.
    %           The vertical spacing is automatically chosen so the
    %           physical X/Y spacings are identical.
    %
    %       If [Xspacing,Yspacing]:
    %           Explicit normalized figure units are used.
    %
    %       Default: 0.01.
    %
    %   'Margin'
    %       Scalar, [Xmargin,Ymargin], or [Left,Right,Bottom,Top].
    %       Values are normalized figure units.
    %       Default: 0.01.
    %
    %   'Z1Z2'
    %       Common color limits [Z1 Z2].
    %       If empty, use automatic limits independently for each image.
    %       Default: [].
    %
    %   'Colorbar'
    %       Add a shared colorbar on the right.
    %       Default: true.
    %
    %   'AxisEqual'
    %       Preserved for compatibility. The image aspect ratio is handled
    %       explicitly by the axes geometry.
    %       Default: true.
    %
    %   'AxisOff'
    %       Hide axes.
    %       Default: true.
    %
    %   'YDir'
    %       'reverse' or 'normal'.
    %       Default: 'reverse'.
    %
    %   'Titles'
    %       Titles for images.
    %       Default: [].
    %
    %   'TitleArgs'
    %       Additional arguments passed to title.
    %       Default: {}.
    %
    %   'ImageArgs'
    %       Additional arguments passed to imagesc.
    %       Default: {}.
    %
    %   'FigureArgs'
    %       Arguments passed to figure if a new figure is created.
    %       Default: {}.
    %
    % Output:
    %   Haxes
    %       Nslot-by-1 axes handles.
    %
    %   Himages
    %       Nslot-by-1 image handles.
    %
    %   Hfig
    %       Figure handle.
    %
    %   Hcolorbar
    %       Colorbar handle, or empty graphics array.
    %
    % Example:
    %
    %   Images = {rand(100,150), rand(100,150), ...
    %             rand(100,150), rand(100,150)};
    %
    %   plot.plotImagesGrid(Images,[2 2], ...
    %       'Spacing',0.01, ...
    %       'Margin',0.02, ...
    %       'Colorbar',true);
    %
    % Inverted grayscale:
    %
    %   plot.plotImagesGrid(Images,[2 2], ...
    %       'Colormap',@() flipud(gray));
    %
    % Author: ChatGPT + Eran Ofek
    % Date: 2026 Aug


    arguments
        Images                    cell
        GridSize                  (1,2) double {mustBeInteger,mustBePositive}

        Args.Parent               = [];
        Args.Colormap             = 'gray';
        Args.Spacing              = 0.01;
        Args.Margin               = 0.01;
        Args.Z1Z2                 = [];
        Args.Colorbar             (1,1) logical = true;
        Args.AxisEqual            (1,1) logical = true;
        Args.AxisOff              (1,1) logical = true;
        Args.YDir                 = 'reverse';
        Args.Titles               = [];
        Args.TitleArgs            cell = {};
        Args.ImageArgs            cell = {};
        Args.FigureArgs           cell = {};
    end


    %--------------------------------------------------------------
    % Basic dimensions
    %--------------------------------------------------------------

    Nrow   = GridSize(1);
    Ncol   = GridSize(2);
    Nslot  = Nrow .* Ncol;
    Nimage = numel(Images);

    if Nimage > Nslot
        error('plot:plotImagesGrid:TooManyImages', ...
            ['The number of images (%d) is larger than the number of ' ...
             'grid positions (%d).'],Nimage,Nslot);
    end


    %--------------------------------------------------------------
    % Parse common color limits
    %--------------------------------------------------------------

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


    %--------------------------------------------------------------
    % Find first non-empty image
    %--------------------------------------------------------------

    FirstImage = find(~cellfun(@isempty,Images),1);

    if isempty(FirstImage)
        error('plot:plotImagesGrid:NoImages', ...
            'Images contains no non-empty images.');
    end


    %--------------------------------------------------------------
    % Parent figure
    %
    % Do not use gcf here because gcf creates a figure automatically,
    % which would prevent FigureArgs from being applied.
    %--------------------------------------------------------------

    if isempty(Args.Parent)

        Hfig = get(groot,'CurrentFigure');

        if isempty(Hfig)
            Hfig = figure(Args.FigureArgs{:});
        end

    else

        Hfig = Args.Parent;

        if ~isgraphics(Hfig,'figure')
            error('plot:plotImagesGrid:InvalidParent', ...
                'Parent must be a valid figure handle.');
        end

    end


    %--------------------------------------------------------------
    % Figure dimensions in pixels
    %--------------------------------------------------------------

    drawnow;

    OldFigUnits = Hfig.Units;
    Hfig.Units  = 'pixels';

    FigPos = Hfig.Position;

    Hfig.Units = OldFigUnits;

    FigWidth  = FigPos(3);
    FigHeight = FigPos(4);

    if FigWidth <= 0 || FigHeight <= 0
        error('plot:plotImagesGrid:InvalidFigureSize', ...
            'The figure must have positive width and height.');
    end


    %--------------------------------------------------------------
    % Spacing
    %
    % Scalar spacing means:
    %   same PHYSICAL spacing horizontally and vertically.
    %--------------------------------------------------------------

    Spacing = Args.Spacing;

    if isscalar(Spacing)

        validateattributes(Spacing,{'numeric'}, ...
            {'real','scalar','finite','nonnegative'});

        GapPix = double(Spacing) .* FigWidth;

        GapXPix = GapPix;
        GapYPix = GapPix;

    elseif isnumeric(Spacing) && numel(Spacing)==2

        validateattributes(Spacing,{'numeric'}, ...
            {'real','vector','numel',2,'finite','nonnegative'});

        GapXPix = double(Spacing(1)) .* FigWidth;
        GapYPix = double(Spacing(2)) .* FigHeight;

    else

        error('plot:plotImagesGrid:InvalidSpacing', ...
            'Spacing must be a scalar or a two-element vector.');

    end


    %--------------------------------------------------------------
    % Margins
    %
    % Convert normalized units to physical pixels.
    %--------------------------------------------------------------

    Margin = Args.Margin;

    if isscalar(Margin)

        LeftMargin   = Margin;
        RightMargin  = Margin;
        BottomMargin = Margin;
        TopMargin    = Margin;

    elseif isnumeric(Margin) && numel(Margin)==2

        LeftMargin   = Margin(1);
        RightMargin  = Margin(1);
        BottomMargin = Margin(2);
        TopMargin    = Margin(2);

    elseif isnumeric(Margin) && numel(Margin)==4

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
        {'numeric'}, ...
        {'real','finite','nonnegative'});


    LeftPix   = LeftMargin   .* FigWidth;
    RightPix  = RightMargin  .* FigWidth;
    BottomPix = BottomMargin .* FigHeight;
    TopPix    = TopMargin    .* FigHeight;


    %--------------------------------------------------------------
    % Determine image aspect ratio
    %
    % Width / Height.
    %--------------------------------------------------------------

    Image0 = Images{FirstImage};

    if ndims(Image0) > 3
        error('plot:plotImagesGrid:InvalidImage', ...
            'Images must be 2-D matrices or M-by-N-by-3 RGB images.');
    end

    Niy = size(Image0,1);
    Nix = size(Image0,2);

    ImageAspect = Nix ./ Niy;


    %--------------------------------------------------------------
    % Colorbar geometry
    %
    % Reserve space for:
    %   1. gap between images and colorbar
    %   2. colorbar itself
    %   3. colorbar tick-label text
    %
    % The text reservation is deliberately fairly generous so labels do
    % not run outside the printed/exported figure.
    %--------------------------------------------------------------

    if Args.Colorbar

        CBGapPix   = max(GapXPix,8);
        CBWidthPix = 18;
        CBTextPix  = 65;

        CBTotalPix = CBGapPix + CBWidthPix + CBTextPix;

    else

        CBGapPix   = 0;
        CBWidthPix = 0;
        CBTextPix  = 0;
        CBTotalPix = 0;

    end


    %--------------------------------------------------------------
    % Available region for image grid
    %--------------------------------------------------------------

    ImageRegionWidth = ...
        FigWidth ...
        - LeftPix ...
        - RightPix ...
        - CBTotalPix;

    ImageRegionHeight = ...
        FigHeight ...
        - BottomPix ...
        - TopPix;


    AvailWidth = ...
        ImageRegionWidth ...
        - (Ncol - 1).*GapXPix;

    AvailHeight = ...
        ImageRegionHeight ...
        - (Nrow - 1).*GapYPix;

    if AvailWidth <= 0 || AvailHeight <= 0
        error('plot:plotImagesGrid:InsufficientSpace', ...
            ['Margins, spacing, and colorbar reservation leave no ' ...
             'space for the images.']);
    end


    %--------------------------------------------------------------
    % Find maximum image dimensions that:
    %
    %   1. preserve image aspect ratio
    %   2. fit all Ncol columns
    %   3. fit all Nrow rows
    %--------------------------------------------------------------

    MaxImageWidthFromX = AvailWidth ./ Ncol;

    MaxImageHeightFromX = ...
        MaxImageWidthFromX ./ ImageAspect;


    MaxImageHeightFromY = AvailHeight ./ Nrow;

    MaxImageWidthFromY = ...
        MaxImageHeightFromY .* ImageAspect;


    if MaxImageHeightFromX <= MaxImageHeightFromY

        ImageWidthPix  = MaxImageWidthFromX;
        ImageHeightPix = MaxImageHeightFromX;

    else

        ImageWidthPix  = MaxImageWidthFromY;
        ImageHeightPix = MaxImageHeightFromY;

    end


    %--------------------------------------------------------------
    % Complete grid dimensions
    %--------------------------------------------------------------

    GridWidthPix = ...
        Ncol .* ImageWidthPix ...
        + (Ncol - 1).*GapXPix;

    GridHeightPix = ...
        Nrow .* ImageHeightPix ...
        + (Nrow - 1).*GapYPix;


    %--------------------------------------------------------------
    % Center grid in available image region
    %--------------------------------------------------------------

    XstartPix = ...
        LeftPix ...
        + 0.5.*(ImageRegionWidth - GridWidthPix);

    YstartPix = ...
        BottomPix ...
        + 0.5.*(ImageRegionHeight - GridHeightPix);


    %--------------------------------------------------------------
    % Set colormap
    %--------------------------------------------------------------

    if isa(Args.Colormap,'function_handle')

        Map = Args.Colormap();

        validateattributes(Map,{'numeric'}, ...
            {'2d','ncols',3,'real','finite','nonnegative'});

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
            ['Colormap must be a colormap name, function handle, or ' ...
             'an N-by-3 numeric matrix.']);

    end


    %--------------------------------------------------------------
    % Titles
    %--------------------------------------------------------------

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
            ['Titles must be empty, a string array, character vector, ' ...
             'or a cell array.']);

    end


    if ~isempty(Titles) && numel(Titles) < Nimage
        error('plot:plotImagesGrid:TooFewTitles', ...
            'The number of titles is smaller than the number of images.');
    end


    %--------------------------------------------------------------
    % Allocate graphics objects
    %--------------------------------------------------------------

    Haxes     = gobjects(Nslot,1);
    Himages   = gobjects(Nslot,1);
    Hcolorbar = gobjects(0);

    FirstImageSlot = [];


    %--------------------------------------------------------------
    % Create image grid
    %--------------------------------------------------------------

    for Islot = 1:Nslot

        Irow = floor((Islot - 1)./Ncol) + 1;
        Icol = mod(Islot - 1,Ncol) + 1;


        %----------------------------------------------------------
        % Position in physical pixels
        %----------------------------------------------------------

        XPix = ...
            XstartPix ...
            + (Icol - 1).*(ImageWidthPix + GapXPix);


        % Rows proceed from TOP to BOTTOM.
        %
        % Irow = 1 is the upper row.
        %
        YPix = ...
            YstartPix ...
            + (Nrow - Irow).*(ImageHeightPix + GapYPix);


        % Convert physical pixels to normalized figure coordinates.

        AxPos = [ ...
            XPix          ./ FigWidth, ...
            YPix          ./ FigHeight, ...
            ImageWidthPix ./ FigWidth, ...
            ImageHeightPix./ FigHeight];


        Haxes(Islot) = axes( ...
            'Parent',Hfig, ...
            'Units','normalized', ...
            'Position',AxPos);


        %----------------------------------------------------------
        % Plot image
        %----------------------------------------------------------

        if Islot <= Nimage && ~isempty(Images{Islot})

            Himages(Islot) = imagesc( ...
                Haxes(Islot), ...
                Images{Islot}, ...
                Args.ImageArgs{:});


            if isempty(FirstImageSlot)
                FirstImageSlot = Islot;
            end


            %------------------------------------------------------
            % Common color limits
            %------------------------------------------------------

            if ~isempty(Z1Z2)
                Haxes(Islot).CLim = Z1Z2;
            end


            %------------------------------------------------------
            % Y direction
            %------------------------------------------------------

            Haxes(Islot).YDir = char(Args.YDir);


            %------------------------------------------------------
            % Important:
            %
            % Do NOT use:
            %
            %       axis image
            %
            % The axes rectangle itself already has exactly the
            % appropriate image aspect ratio.
            %
            % Set limits explicitly so the pixels occupy the full axes.
            %------------------------------------------------------

            ThisImage = Images{Islot};

            Ny = size(ThisImage,1);
            Nx = size(ThisImage,2);

            Haxes(Islot).XLim = [0.5, Nx + 0.5];
            Haxes(Islot).YLim = [0.5, Ny + 0.5];

            Haxes(Islot).DataAspectRatioMode = 'auto';
            Haxes(Islot).PlotBoxAspectRatioMode = 'auto';


            %------------------------------------------------------
            % Axis visibility
            %------------------------------------------------------

            if Args.AxisOff
                axis(Haxes(Islot),'off');
            end


            %------------------------------------------------------
            % Title
            %------------------------------------------------------

            if ~isempty(Titles)

                title( ...
                    Haxes(Islot), ...
                    Titles(Islot), ...
                    Args.TitleArgs{:});

            end


        else

            Haxes(Islot).Visible = 'off';

        end

    end


    %--------------------------------------------------------------
    % Shared colorbar
    %--------------------------------------------------------------

    if Args.Colorbar && ~isempty(FirstImageSlot)

        % Save axes positions because MATLAB's colorbar() may resize the
        % peer axes.

        SavedPositions = cell(Nslot,1);

        for Islot = 1:Nslot
            SavedPositions{Islot} = Haxes(Islot).Position;
        end


        Hcolorbar = colorbar(Haxes(FirstImageSlot));


        % Restore all image axes positions.

        for Islot = 1:Nslot
            Haxes(Islot).Position = SavedPositions{Islot};
        end


        %----------------------------------------------------------
        % Position colorbar explicitly in PIXELS
        %----------------------------------------------------------

        Hcolorbar.Units = 'pixels';


        CBXPix = ...
            XstartPix ...
            + GridWidthPix ...
            + CBGapPix;


        Hcolorbar.Position = [ ...
            CBXPix, ...
            YstartPix, ...
            CBWidthPix, ...
            GridHeightPix];


        %----------------------------------------------------------
        % If common CLim is supplied, enforce it on the peer axes.
        %----------------------------------------------------------

        if ~isempty(Z1Z2)
            Haxes(FirstImageSlot).CLim = Z1Z2;
        end

    end

end