function Img = mosaicGrid(Ax, Values, Args)
    % Draw a per-crop value mosaic on a given axes.
    % Description: Places one value per LAST crop onto a Nrows x Ncols image
    %              using PhotCalibTrans.cropID2RowCol for the geometry, draws
    %              it with imagesc, and optionally annotates each tile with
    %              its crop index and value. Shared focal-plane mosaic
    %              primitive.
    % Input  : - Ax     - target axes handle.
    %          - Values - 1xNcrop numeric vector (value per crop index).
    %          * ...,key,val,...
    %            'Nrows'       - Mosaic rows. Default 4.
    %            'Ncols'       - Mosaic columns. Default 6.
    %            'TileOrder'   - 'rowmajor' | 'colmajor'. Default 'rowmajor'.
    %            'ShowValues'  - Annotate tiles with crop id + value.
    %                            Default true.
    %            'ValueFormat' - printf format for the value. Default '%.3g'.
    %            'Colormap'    - [] (default grey ramp), a colormap name, or
    %                            an Nx3 matrix.
    %            'CLim'        - [lo hi] colour limits. Default [] (auto).
    % Output : - Img - the Nrows x Ncols matrix that was displayed (NaN where
    %            no crop maps).
    % Author : photCalib package refactor (2026-05)
    % Example: mosaicGrid(gca, ParamPerCrop, 'TileOrder', 'rowmajor');

    arguments
        Ax
        Values
        Args.Nrows       (1,1) double       = 4
        Args.Ncols       (1,1) double       = 6
        Args.TileOrder   {mustBeTextScalar} = 'rowmajor'
        Args.ShowValues  logical            = true
        Args.ValueFormat {mustBeTextScalar} = '%.3g'
        Args.Colormap                       = []
        Args.CLim                           = []
    end

    Values = Values(:).';
    Ncrop  = numel(Values);

    Img = nan(Args.Nrows, Args.Ncols);
    RC  = nan(Ncrop, 2);
    for Ic = 1:Ncrop
        [Row, Col] = PhotCalibTrans.cropID2RowCol( ...
            Ic, Args.Nrows, Args.Ncols, Args.TileOrder);
        RC(Ic,:)     = [Row, Col];
        Img(Row,Col) = Values(Ic);
    end

    imagesc(Ax, Img);
    axis(Ax, 'image');
    set(Ax, 'YDir', 'normal', 'XTick', [], 'YTick', []);

    if isempty(Args.Colormap)
        Cmap = flipud(gray(256));
        Cmap = Cmap(26:230, :);
    elseif ischar(Args.Colormap) || isstring(Args.Colormap)
        Cmap = colormap(char(Args.Colormap));
    else
        Cmap = Args.Colormap;
    end
    colormap(Ax, Cmap);
    if ~isempty(Args.CLim)
        caxis(Ax, Args.CLim);
    end

    if Args.ShowValues
        hold(Ax, 'on');
        for Ic = 1:Ncrop
            if isfinite(Values(Ic))
                Str = sprintf(['%d\n' Args.ValueFormat], Ic, Values(Ic));
                FW  = 'bold';
            else
                Str = sprintf('%d', Ic);
                FW  = 'normal';
            end
            text(Ax, RC(Ic,2), RC(Ic,1), Str, ...
                'HorizontalAlignment', 'center', 'Color', 'w', ...
                'FontSize', 8, 'FontWeight', FW, 'Interpreter', 'none');
        end
    end
end
