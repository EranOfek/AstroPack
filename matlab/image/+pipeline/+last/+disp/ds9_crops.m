function [Result, AstrometricCat] = ds9_crops(AI, Args)
    % Display all crops of an image together in ds9, and overplot their catalogs.
    %   Two display methods are available:
    %   'mosaic' - each crop is trimmed to its unique section and loaded into
    %              a single ds9 frame as a WCS mosaic, i.e., ds9 places every
    %              crop on the sky using its own WCS. No new WCS is fitted.
    %   'stitch' - the crops are stitched into one image by
    %              imProc.stack.stitchCrops, optionally with a new WCS fitted
    %              to the merged catalog, and displayed as a regular image.
    %   The catalog sources are marked twice: by their RA/Dec columns and by
    %   their X/Y columns (in 'mosaic', converted to RA/Dec with the crop WCS).
    % Input  : - An AstroImage array of crops of the same image, or a file
    %            name with wild cards, or a cell array of file names of the
    %            crops main product (e.g., the '_Image_' files).
    %          * ...,key,val,...
    %            'Method' - 'mosaic' | 'stitch'. Default is 'mosaic'.
    %            'AddProduct' - Products to read along with the main product
    %                   files (input of file names only). A product file
    %                   missing on disk is skipped. Default is {'Mask','Cat'}.
    %            'MainProduct' - Name of the main product in the file names.
    %                   Default is 'Image'.
    %            'Trim' - ('mosaic') A logical indicating whether to trim
    %                   each crop to its unique section (ORIGUSEC), so that
    %                   overlaps are displayed once. Default is true.
    %            'UpdateWCS' - ('stitch') Fit a WCS to the stitched image.
    %                   Without it, only the X/Y markers are plotted.
    %                   Default is true.
    %            'AstrometricCat' - ('stitch') An AstroCatalog passed to
    %                   stitchCrops, skipping the catsHTM query.
    %                   Default is [].
    %            'PlotXY' - Mark sources by their X/Y columns. Default is true.
    %            'PlotRADec' - Mark sources by their RA/Dec columns.
    %                   Default is true.
    %            'ColorXY' - Color of the X/Y markers. Default is 'g'.
    %            'ColorRADec' - Color of the RA/Dec markers. Default is 'b'.
    %            'MarkerXY' - Shape of the X/Y markers: 'o'|'s'. Default is 'o'.
    %            'MarkerRADec' - Shape of the RA/Dec markers: 'o'|'s'.
    %                   Default is 's'.
    %            'MarkerSize' - Radius of a circle marker, half the side of a
    %                   box marker [arcsec in 'mosaic', pix in 'stitch'].
    %                   The RA/Dec markers are 1.25 times larger, so that
    %                   both marker sets are seen. Default is 10.
    %            'Frame' - ds9 frame. Default is 1.
    %            'Zoom' - ds9 zoom. Default is 'to fit'.
    %            'KeyOrigSec' - Header keyword of the crop section in the
    %                   full image. Default is 'ORIGSEC'.
    %            'KeyOrigUSec' - Header keyword of the unique crop section in
    %                   the full image. Default is 'ORIGUSEC'.
    %            'Verbosity' - Default is false.
    % Output : - The displayed AstroImage: the trimmed crops ('mosaic') or
    %            the stitched image ('stitch').
    %          - AstroCatalog used for astrometry ('stitch' with
    %            UpdateWCS), otherwise [].
    % Author : A.M. Krassilchtchikov (Oct 2026)
    % Example: AI = pipeline.last.disp.ds9_crops('LAST.01.07.01_20260423.001037.811_clear_1626.N2_000_001_0*_sci_coadd_Image_1.fits');
    %          AI = pipeline.last.disp.ds9_crops(AI, 'Method','stitch');

    arguments
        AI
        Args.Method             = 'mosaic';   % 'mosaic'|'stitch'
        Args.AddProduct         = {'Mask','Cat'};
        Args.MainProduct        = 'Image';
        Args.Trim logical       = true;
        Args.UpdateWCS logical  = true;
        Args.AstrometricCat     = [];
        Args.PlotXY logical     = true;
        Args.PlotRADec logical  = true;
        Args.ColorXY            = 'g';
        Args.ColorRADec         = 'b';
        Args.MarkerXY           = 'o';
        Args.MarkerRADec        = 's';
        Args.MarkerSize         = 10;
        Args.Frame              = 1;
        Args.Zoom               = 'to fit';
        Args.KeyOrigSec         = 'ORIGSEC';
        Args.KeyOrigUSec        = 'ORIGUSEC';
        Args.Verbosity logical  = false;
    end

    AstrometricCat = [];

    if ~isa(AI, 'AstroImage')
        AI = readCrops(AI, Args.MainProduct, Args.AddProduct, Args.Verbosity);
    else
        % the crop WCS is needed to place and trim the crops
        for Icrop = 1:numel(AI)
            if isempty(AI(Icrop).WCS) || ~AI(Icrop).WCS.Success
                AI(Icrop) = populateWCS(AI(Icrop));
            end
        end
    end
    Ncrop = numel(AI);

    if ~ds9.isopen
        ds9.open;
    end
    ds9.frame(Args.Frame);
    ds9.clear_frame([]);

    switch lower(Args.Method)
        case 'mosaic'
            Result = repmat(AstroImage, 1, Ncrop);
            RD     = cell(Ncrop, 1);    % catalog RA/Dec
            XYsky  = cell(Ncrop, 1);    % catalog X/Y converted to RA/Dec
            for Icrop = 1:Ncrop
                try
                    Result(Icrop) = trimCrop(AI(Icrop), Args);
                    % the X/Y markers are placed with the WCS ds9 uses for the crop
                    Cat = Result(Icrop).CatData;
                    if ~isempty(Cat.Catalog)
                        RD{Icrop} = Cat.getLonLat('deg');
                        XY = Cat.getXY;
                        [XYsky{Icrop}(:,1), XYsky{Icrop}(:,2)] = Result(Icrop).WCS.xy2sky(XY(:,1), XY(:,2), 'OutUnits','deg');
                    end

                    TmpName = sprintf('%s.fits', tempname);
                    FITS.write(Result(Icrop).Image, TmpName, 'Header',Result(Icrop).HeaderData.Data);
                    ds9.system('xpaset -p ds9 mosaic wcs %s', TmpName);
                    delete(TmpName);
                catch ME
                    fprintf('ds9_crops: crop %d not displayed: %s\n', Icrop, ME.message);
                end
            end
            % one intensity scale for all crops, not one per crop
            ds9.system('xpaset -p ds9 scale scope global');
            CooType = 'fk5';
            RD      = cell2mat(RD);
            XYplot  = cell2mat(XYsky);

        case 'stitch'
            % stitchCrops needs a mask for every crop; add it to a copy
            % so that the input crops are not modified
            NoMask = arrayfun(@(X) isempty(X.MaskData.Data), AI);
            if any(NoMask)
                AI = AI.copy();
                for Icrop = find(NoMask(:)')
                    AI(Icrop).MaskData.Data = zeros(size(AI(Icrop).Image), 'uint32');
                end
            end
            [Result, AstrometricCat] = imProc.stack.stitchCrops(AI, 'UpdateWCS',Args.UpdateWCS,...
                                                                'AstrometricCat',Args.AstrometricCat);
            ds9.disp(Result, Args.Frame);
            CooType = 'image';
            XYplot  = Result.CatData.getXY;
            RD      = [];
            if Args.PlotRADec
                if Args.UpdateWCS && Result.WCS.Success
                    RD = Result.CatData.getLonLat('deg');
                elseif Args.Verbosity
                    fprintf('ds9_crops: no WCS for the stitched image, RA/Dec markers not plotted\n');
                end
            end

        otherwise
            error('Unknown Method option: %s', Args.Method);
    end

    ds9.zoom(Args.Zoom);

    % one region file per marker set
    if Args.PlotXY && ~isempty(XYplot)
        ds9.plotXY(XYplot, [], 'Color',Args.ColorXY, 'Marker',Args.MarkerXY,...
                   'MarkerSize',markerSize(Args.MarkerXY, Args.MarkerSize), 'CooType',CooType);
    end
    if Args.PlotRADec && ~isempty(RD)
        ds9.plotXY(RD, [], 'Color',Args.ColorRADec, 'Marker',Args.MarkerRADec,...
                   'MarkerSize',markerSize(Args.MarkerRADec, 1.25.*Args.MarkerSize), 'CooType','fk5');
    end
    if Args.Verbosity
        fprintf('ds9_crops: %d crops, %d X/Y and %d RA/Dec markers\n', Ncrop, size(XYplot,1), size(RD,1));
    end
end


function AI = readCrops(Files, MainProduct, AddProduct, Verbosity)
    % Read the crops main product and the existing additional products
    if ischar(Files) || isstring(Files)
        if contains(Files, '*') || contains(Files, '?')
            List  = dir(Files);
            Files = fullfile({List.folder}, {List.name});
        else
            Files = cellstr(Files);
        end
    end
    Ncrop = numel(Files);
    if Ncrop==0
        error('ds9_crops: no files found');
    end

    AI = repmat(AstroImage, 1, Ncrop);
    for Icrop = 1:Ncrop
        ProdArgs = {};
        for Iprod = 1:numel(AddProduct)
            ProdFile = strrep(Files{Icrop}, ['_' MainProduct '_'], ['_' AddProduct{Iprod} '_']);
            if isfile(ProdFile)
                ProdArgs = [ProdArgs, AddProduct(Iprod), {{ProdFile}}];
            elseif Verbosity
                fprintf('ds9_crops: %s not found\n', ProdFile);
            end
        end
        AI(Icrop) = AstroImage(Files(Icrop), ProdArgs{:});
    end
    AI = populateWCS(AI);
end


function Result = trimCrop(AI, Args)
    % Trim a crop to its unique section and select the catalog sources it owns
    if ~Args.Trim
        Result = AI;
        return
    end

    OrigSec  = AI.HeaderData.getVal(Args.KeyOrigSec,  'ReadCCDSEC',true);
    OrigUSec = AI.HeaderData.getVal(Args.KeyOrigUSec, 'ReadCCDSEC',true);
    % the unique section in the crop coordinates
    USec = OrigUSec - OrigSec([1 1 3 3]) + 1;

    Result = crop(AI, USec, 'UpdateCat',false, 'UpdateWCS',true, 'CreateNewObj',true);

    % own a source if the pixel holding its centroid is owned, as in
    % imProc.stack.stitchCrops (issue #1236)
    Cat = AI.CatData.copy;
    if ~isempty(Cat.Catalog)
        IndX = Cat.colname2ind({'XPEAK','X1','X'});
        IndY = Cat.colname2ind({'YPEAK','Y1','Y'});
        Xcat = Cat.Catalog(:,IndX(1));
        Ycat = Cat.Catalog(:,IndY(1));
        FlagIn = Xcat>=(USec(1)-0.5) & Xcat<(USec(2)+0.5) & ...
                 Ycat>=(USec(3)-0.5) & Ycat<(USec(4)+0.5);
        Cat.Catalog = Cat.Catalog(FlagIn,:);
    end
    % the catalog of the trimmed crop in its own pixel coordinates
    Result.CatData = imProc.cat.shiftXY(Cat, 1-USec(1), 1-USec(3));
end


function Size = markerSize(Marker, Radius)
    % ds9 size of a marker: the radius of a circle, the side of a box
    if any(strcmpi(Marker, {'s','box'}))
        Size = 2.*Radius;
    else
        Size = Radius;
    end
end
